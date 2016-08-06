module Language.Snobol4.Interpreter.Internal.StateMachine.Run where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Error

-- | Lift an evaluation stack result back into the non-evaluation stack
unliftEval :: InterpreterShell m => Evaluator m a -> Interpreter m (Either EvalStop a)
unliftEval (Evaluator m) = do
    x <- Interpreter $ lift $ runExceptT $ runExceptT m
    case x of
        Left stop -> return $ Left stop
        Right (Left err) -> programError err
        Right (Right val) -> return $ Right val

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
