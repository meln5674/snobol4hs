{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Run
Description     : Moving between tranformer stacks
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.Run where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Error

-- | Lift an operation from non-evaluation stack into evaluation stack
liftEval :: InterpreterShell m 
         => InterpreterGeneric program instruction m a 
         -> EvaluatorGeneric program instruction m a
liftEval = Evaluator . ExceptT . lift . runExceptT . runInterpreter

-- | Lift an evaluation stack result back into the non-evaluation stack
unliftEval :: InterpreterShell m 
           => EvaluatorGeneric program instruction m a 
           -> InterpreterGeneric program instruction m (Either EvalStop a)
unliftEval (Evaluator m) = do
    x <- Interpreter $ lift $ runExceptT $ runExceptT m
    case x of
        Left stop -> return $ Left stop
        Right (Left err) -> programError err
        Right (Right val) -> return $ Right val

-- | Take an evaluation and return it to the interpreter stack, with a handler 
-- for a failed evaluation
catchEval :: InterpreterShell m 
          => EvaluatorGeneric program instruction m a 
          -> (EvalStop -> InterpreterGeneric program instruction m a)
          -> InterpreterGeneric program instruction m a
catchEval m h = do
    result <- unliftEval m
    case result of
        Right val -> return val
        Left stop -> h stop

