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

module Language.Snobol4.Interpreter.Evaluator where

import Prelude hiding (toInteger)

import Text.Read hiding (lift, String, step, get)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Array (Array)
import qualified Data.Array as A

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal
import Language.Snobol4.Interpreter.Internal.Types

-- | Evaluate an arithmetic operation
arithmetic :: InterpreterShell m 
           => (Int -> Int -> Int) -- ^ Integer version 
           -> (Float -> Float -> Float) -- ^ Real version
           -> Data -- ^ Left argument
           -> Data -- ^ Right argument
           -> Evaluator m Data
arithmetic f_int f_real a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData $ f_int a'' b''
        (RealData a'', RealData b'') -> return $ RealData $ f_real a'' b''
        _ -> liftEval $ programError IllegalDataType

-- | Evaluate a pattern operation
pattern :: InterpreterShell m 
        => (Pattern -> Pattern -> Pattern) 
        -> Data 
        -> Data 
        -> Evaluator m Data
pattern f a b = do
    a' <- toPattern a
    b' <- toPattern b
    return $ PatternData $ f a' b'

-- | Evaluate a binary operation on data
evalOp :: InterpreterShell m 
       => Operator 
       -> Data 
       -> Data 
       -> Evaluator m Data
evalOp Plus = arithmetic (+) (+)
evalOp Minus = arithmetic (-) (-)
evalOp Star = arithmetic (*) (*)
evalOp Slash = arithmetic div (/)
evalOp Bang = arithmetic (^) (**)
evalOp DoubleStar = evalOp Bang
evalOp Pipe = pattern AlternativePattern 
evalOp Blank = pattern ConcatPattern

-- | Evaluate an expression, then choose one of two actions/values to use
checkSuccess :: InterpreterShell m 
             => Expr -- ^ Expression to evaluate
             -> Evaluator m Data -- ^ Action on success
             -> Evaluator m Data -- ^ Action on failure
             -> Evaluator m Data
checkSuccess expr success failure = do
    result <- liftEval $ unliftEval $ evalExpr expr
    case result of
        Right _ -> success
        Left _ -> failure

-- | Evaluate an expression
evalExpr :: InterpreterShell m => Expr -> Evaluator m Data
evalExpr (PrefixExpr Not expr) = checkSuccess 
    expr 
    failEvaluation 
    (return $ StringData "")
evalExpr (PrefixExpr Question expr) = checkSuccess 
    expr 
    (return $ StringData "") 
    failEvaluation
evalExpr (PrefixExpr Minus expr) = do
    data_ <- evalExpr expr
    case data_ of
        s@(StringData _) -> do
            r <- toReal s
            return $ RealData $ -r
        (IntegerData i) -> return $ IntegerData $ -i
        (RealData r) -> return $ RealData $ -r
        _ -> liftEval $ programError IllegalDataType
evalExpr (UnevaluatedExpr expr) = return $ PatternData $ UnevaluatedExprPattern expr
evalExpr (IdExpr "INPUT") = StringData <$> (lift $ input)
evalExpr (IdExpr "OUTPUT") = StringData <$> (lift $ lastOutput)
evalExpr (IdExpr "PUNCH") = StringData <$> (lift $ lastPunch)
evalExpr (IdExpr name) = do
    d <- liftEval $ varLookup name
    case d of
        Just (_,d) -> return d
        Nothing -> failEvaluation
evalExpr (LitExpr (Int i)) = return $ IntegerData i
evalExpr (LitExpr (Real r)) = return $ RealData r
evalExpr (LitExpr (String s)) = return $ StringData s
evalExpr (CallExpr id args) = do
    args' <- mapM evalExpr args
    result <- liftEval $ call id args'
    case result of
        Just result -> return result
        Nothing -> failEvaluation
evalExpr (RefExpr id args) = do
    args' <- mapM evalExpr args
    x <- execLookup (LookupAggregate id args')
    case x of
        Just x -> return x
        Nothing -> liftEval $ programError ErroneousArrayOrTableReference
evalExpr (ParenExpr expr) = evalExpr expr
evalExpr (BinaryExpr a op b) = do
    a' <- evalExpr a
    b' <- evalExpr b
    evalOp op a' b'
evalExpr NullExpr = return $ StringData ""
evalExpr _ = liftEval $ programError ErrorInSnobol4System

-- | Execute a lookup
execLookup :: InterpreterShell m => Lookup -> Evaluator m (Maybe Data) 
execLookup Input = (Just . StringData) <$> lift input 
execLookup Output = (Just . StringData) <$> lift lastOutput 
execLookup Punch = (Just . StringData) <$> lift lastPunch 
execLookup (LookupLiteral x) = return $ Just x 
execLookup (LookupId i) = liftEval $ varLookup' i
execLookup (LookupAggregate id args) = do
    base <- liftEval $ varLookup' id
    case base of
        Nothing -> return Nothing
        Just val -> do
            let loop (ArrayData arr) ((IntegerData i):as) = case arr `arrayGet` i of
                    Nothing -> Nothing
                    Just d -> loop d as
                loop (ArrayData _) _ = Nothing
                loop (TableData tab) (a:as) = case M.lookup a tab of
                    Nothing -> Nothing
                    Just d -> loop d as
                loop x [] = Just x
                loop x _ = Nothing
            return $ loop val args

-- | Take an evaluation and return it to the interpreter stack, with a handler 
-- for a failed evaluation
catchEval :: InterpreterShell m 
          => Evaluator m a 
          -> (EvalStop -> Interpreter m a)
          -> Interpreter m a
catchEval m h = do
    result <- unliftEval m
    case result of
        Right val -> return val
        Left stop -> h stop
