module Language.Snobol4.Interpreter.Evaluator where

import Prelude hiding (toInteger)

import Text.Read hiding (lift, String, step, get)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.Types

arithmetic :: InterpreterShell m 
           => (Int -> Int -> Int) 
           -> (Float -> Float -> Float)
           -> Data 
           -> Data 
           -> Evaluator m Data
arithmetic f_int f_real a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData $ f_int a'' b''
        (RealData a'', RealData b'') -> return $ RealData $ f_real a'' b''
        _ -> liftEval $ programError ProgramError

pattern :: InterpreterShell m => (Pattern -> Pattern -> Pattern) -> Data -> Data -> Evaluator m Data
pattern f a b = do
    a' <- toPattern a
    b' <- toPattern b
    return $ PatternData $ f a' b'

-- | Evaluate a binary operation on data
evalOp :: InterpreterShell m => Operator -> Data -> Data -> Evaluator m Data
evalOp Plus = arithmetic (+) (+)
evalOp Minus = arithmetic (-) (-)
evalOp Star = arithmetic (*) (*)
evalOp Slash = arithmetic div (/)
evalOp Bang = arithmetic (^) (**)
evalOp DoubleStar = evalOp Bang
evalOp Pipe = pattern AlternativePattern 
evalOp Blank = pattern ConcatPattern

-- | Evaluate an expression
evalExpr :: InterpreterShell m => Expr -> Evaluator m Data
evalExpr (PrefixExpr Not expr) = do
    result <- liftEval $ unliftEval $ evalExpr expr
    case result of
        Right x -> failEvaluation
        Left x -> return $ StringData ""
evalExpr (PrefixExpr Question expr) = do
    result <- liftEval $ unliftEval $ evalExpr expr
    case result of
        Right x -> return $ StringData ""
        Left x -> failEvaluation
evalExpr (PrefixExpr Minus expr) = do
    data_ <- evalExpr expr
    case data_ of
        s@(StringData _) -> do
            i <- toReal s
            case i of
                RealData i -> return $ RealData $ -i
        (IntegerData i) -> return $ IntegerData $ -i
        (RealData r) -> return $ RealData $ -r
           
-- TODO: UnevaluatedExpr
evalExpr (IdExpr "INPUT") = StringData <$> (lift $ input)
evalExpr (IdExpr "OUTPUT") = StringData <$> (lift $ lastOutput)
evalExpr (IdExpr "PUNCH") = StringData <$> (lift $ lastPunch)
evalExpr (IdExpr name) = do
    d <- liftEval $ varLookup name
    case d of
        Just d -> return d
        Nothing -> failEvaluation
evalExpr (LitExpr (Int i)) = return $ IntegerData i
evalExpr (LitExpr (Real r)) = return $ RealData r
evalExpr (LitExpr (String s)) = return $ StringData s
-- TODO: CallExpr
evalExpr (RefExpr id args) = do
    args' <- mapM evalExpr args
    x <- execLookup (LookupAggregate id args')
    case x of
        Just x -> return x
        Nothing -> liftEval $ programError ProgramError
evalExpr (ParenExpr expr) = evalExpr expr
evalExpr (BinaryExpr a op b) = do
    a' <- evalExpr a
    b' <- evalExpr b
    evalOp op a' b'
evalExpr NullExpr = return $ StringData ""
evalExpr _ = liftEval $ programError ProgramError

execLookup :: InterpreterShell m => Lookup -> Evaluator m (Maybe Data)
execLookup Input = (Just . StringData) <$> lift input
execLookup Output = (Just . StringData) <$> lift lastOutput
execLookup Punch = (Just . StringData) <$> lift lastPunch
execLookup (LookupLiteral x) = return $ Just x
execLookup (LookupId i) = liftEval $ varLookup i
execLookup (LookupAggregate id args) = do
    base <- liftEval $ varLookup id
    case base of
        Nothing -> return Nothing
        Just val -> do
            let loop (ArrayData vec) ((IntegerData i):as) = case vec V.!? i of
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
