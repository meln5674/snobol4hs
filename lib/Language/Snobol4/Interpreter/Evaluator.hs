{-|
Module          : Language.Snobol4.Interpreter.Evaluator
Description     : Expression Evaluation
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

Expressions are evaluated as actions of the 'Evaluator' type, which can be
returned to the 'Interpreter' type using either 'unliftEval', which yields an
Either value, or by 'catchEval', which takes a function to call if the
evaluation fails.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Snobol4.Interpreter.Evaluator where

import Prelude hiding (toInteger)

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Error

import Language.Snobol4.Interpreter.Internal.Types
import Language.Snobol4.Interpreter.Internal.StateMachine




-- | Evaluate an arithmetic operation
arithmetic :: ( InterpreterShell m
--              {-, Snobol4Machine program-}
              , LocalVariablesClass m 
              ) 
           => (Snobol4Integer -> Snobol4Integer -> Snobol4Integer) -- ^ Integer version 
           -> (Snobol4Real -> Snobol4Real -> Snobol4Real) -- ^ Real version
           -> (Data (ExprType m)) -- ^ Left argument
           -> (Data (ExprType m)) -- ^ Right argument
           -> InterpreterGeneric program m (Data (ExprType m))
arithmetic f_int f_real a b = do
    result <- raiseArgs a b
    case result of
        Just (IntegerData a'', IntegerData b'') -> return $ IntegerData $ f_int a'' b''
        Just (RealData a'', RealData b'') -> return $ RealData $ f_real a'' b''
        _ -> programError IllegalDataType

-- | Evaluate a pattern operation
pattern :: ( InterpreterShell m
--           {-, Snobol4Machine program-}
           ) 
        => ((Pattern (ExprType m)) -> (Pattern (ExprType m)) -> (Pattern (ExprType m)))
        -> (Data (ExprType m)) 
        -> (Data (ExprType m)) 
        -> InterpreterGeneric program m (Data (ExprType m))
pattern f a b = do
    a' <- toPattern a
    b' <- toPattern b
    return $ TempPatternData $ f a' b'

-- | Evaluate a binary operation on data
evalOp :: ( InterpreterShell m{-, Snobol4Machine program-}, LocalVariablesClass m ) 
       => Operator 
       -> (Data (ExprType m)) 
       -> (Data (ExprType m)) 
       -> InterpreterGeneric program m (Data (ExprType m))
evalOp Plus = arithmetic (+) (+)
evalOp Minus = arithmetic (-) (-)
evalOp Star = arithmetic (*) (*)
evalOp Slash = arithmetic div (/)
evalOp Bang = arithmetic (^) (**)
evalOp DoubleStar = evalOp Bang
evalOp Pipe = pattern AlternativePattern
evalOp Blank = pattern ConcatPattern
evalOp _ = \_ _ -> programError ErrorInSnobol4System

{-
-- | Evaluate an expression, then choose one of two actions/values to use
checkSuccess :: ( Snobol4Machine program
                , InterpreterShell m 
                , LocalVariablesClass m
                )
             => Expr -- ^ Expression to evaluate
             -> EvaluatorGeneric program (EvaluationError program) m Data -- ^ Action on success
             -> EvaluatorGeneric program (EvaluationError program) m Data -- ^ Action on failure
             -> EvaluatorGeneric program (EvaluationError program) m Data
checkSuccess expr success failure = do
    result <- liftEval $ unliftEval $ evalExpr expr
    case result of
        Right _ -> success
        Left _ -> failure

-- | Evaluate an expression as if it were an L-Value
evalLookup :: ( Snobol4Machine program
              , InterpreterShell m
              , LocalVariablesClass m
              ) 
           => Expr 
           -> EvaluatorGeneric program (EvaluationError program) m Lookup
evalLookup expr@(LitExpr _) = LookupLiteral <$> evalExpr expr
evalLookup (IdExpr "INPUT") = return $ Input
evalLookup (IdExpr "OUTPUT") = return $ Output
evalLookup (IdExpr "PUNCH") = return $ Punch
evalLookup (IdExpr s) = return $ LookupId $ mkString s
evalLookup (PrefixExpr Dollar expr) = do
    expr' <- evalExpr expr
    s <- liftEval $ toString expr'
    return $ LookupId s
evalLookup (RefExpr s args) = LookupAggregate (mkString s) <$> mapM evalExpr args
evalLookup (ParenExpr expr) = evalLookup expr
evalLookup expr = LookupLiteral <$> evalExpr expr

-- | Evaluate an expression as if it were an R-value
evalExpr :: ( Snobol4Machine program
            , InterpreterShell m 
            , LocalVariablesClass m
            )
         => Expr -> EvaluatorGeneric program (EvaluationError program) m Data
evalExpr (PrefixExpr Not expr) = checkSuccess 
    expr 
    failEval 
    (return $ StringData nullString)
evalExpr (PrefixExpr Question expr) = checkSuccess 
    expr 
    (return $ StringData nullString)
    failEval
evalExpr (PrefixExpr Minus expr) = do
    data_ <- evalExpr expr
    case data_ of
        s@(StringData _) -> do
            r <- liftEval $ toReal s
            liftEval $ maybe (programError IllegalDataType) (return . RealData . negate) r
        (IntegerData i) -> return $ IntegerData $ -i
        (RealData r) -> return $ RealData $ -r
        _ -> liftEval $ programError IllegalDataType
evalExpr (PrefixExpr Star expr) = return $ TempPatternData $ UnevaluatedExprPattern expr
evalExpr (PrefixExpr Dot (IdExpr name)) = return $ Name $ LookupId $ mkString name
evalExpr (PrefixExpr Dollar expr) = do
    expr' <- evalExpr expr
    s <- liftEval $ toString expr'
    result <- liftEval $ execLookup $ LookupId s
    case result of
        Just d -> return d
        Nothing -> failEval
evalExpr (PrefixExpr And (IdExpr "")) = liftEval $ programError ErrorInSnobol4System
evalExpr (IdExpr "INPUT") = StringData <$> (lift $ mkString <$> input)
evalExpr (IdExpr "OUTPUT") = StringData <$> (lift $ mkString <$> lastOutput)
evalExpr (IdExpr "PUNCH") = StringData <$> (lift $ mkString <$> lastPunch)
evalExpr (IdExpr name) = do
    lookupResult <- liftEval $ execLookup $ LookupId $ mkString name
    case lookupResult of
        Just val -> return val
        Nothing -> failEval
evalExpr (LitExpr (Int i)) = return $ IntegerData $ mkInteger i
evalExpr (LitExpr (Real r)) = return $ RealData $ mkReal r
evalExpr (LitExpr (String s)) = return $ StringData $ mkString s
evalExpr (CallExpr name args) = do
    args' <- mapM evalExpr args
    callResult <- call (mkString name) args'
    case callResult of
        Just val -> return val
        Nothing -> failEval
evalExpr (RefExpr name args) = do
    args' <- mapM evalExpr args
    lookupResult <- liftEval $ execLookup $ LookupAggregate (mkString name) args'
    case lookupResult of
        Just val -> return val
        Nothing -> liftEval $ programError ErroneousArrayOrTableReference
evalExpr (ParenExpr expr) = evalExpr expr
evalExpr (BinaryExpr a op b) = do
    a' <- evalExpr a
    b' <- evalExpr b
    liftEval $ evalOp op a' b'
evalExpr NullExpr = return $ StringData nullString
evalExpr _ = liftEval $ programError ErrorInSnobol4System
-}
