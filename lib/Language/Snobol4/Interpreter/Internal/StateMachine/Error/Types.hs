{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Error.Types
Description     : State machine error types
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.Error.Types where

import Language.Snobol4.Interpreter.Data

-- | The reason evaluation stopped
data EvalStop
    -- | The evaluation succeeded and there is nothing else to evaluate
    = EvalSuccess (Maybe Data)
    -- | The evaluation failed
    | EvalFailed
  deriving (Show, Eq)
