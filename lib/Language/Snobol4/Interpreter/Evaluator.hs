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
module Language.Snobol4.Interpreter.Evaluator where

import Prelude hiding (toInteger)

import Control.Monad.Trans

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Error

import Language.Snobol4.Interpreter.Internal.Types
import Language.Snobol4.Interpreter.Internal.StateMachine

-- | Evaluate an arithmetic operation
arithmetic :: InterpreterShell m 
           => (Snobol4Integer -> Snobol4Integer -> Snobol4Integer) -- ^ Integer version 
           -> (Snobol4Real -> Snobol4Real -> Snobol4Real) -- ^ Real version
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
    return $ TempPatternData $ f a' b'

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
evalOp _ = \_ _ -> liftEval $ programError ErrorInSnobol4System


