module Language.Snobol4.Interpreter.Internal.StateMachine.Error.Types where

import Language.Snobol4.Interpreter.Data

-- | The reason evaluation stopped
data EvalStop
    -- | The evaluation succeeded and there is nothing else to evaluate
    = EvalSuccess (Maybe Data)
    -- | The evaluation failed
    | EvalFailed
  deriving (Show, Eq)
