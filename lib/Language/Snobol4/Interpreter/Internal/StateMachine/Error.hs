{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Error
Description     : State machine errors
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.Error where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error

import Language.Snobol4.Interpreter.Internal.StateMachine.Types

-- | Terminate the program with an error
programError :: InterpreterShell m => ProgramError -> Interpreter m a
programError = Interpreter . throwE

-- | Mark the current evaluation as failed and prohibit additional evaluation
failEvaluation :: InterpreterShell m => Evaluator m a
failEvaluation = Evaluator
               $ lift 
               $ throwE 
                 EvalFailed

-- | Mark the current evaluation as successful and prohibit additional evaluation
finishEvaluation :: InterpreterShell m => Maybe Data -> Evaluator m a
finishEvaluation = Evaluator
                 . lift 
                 . throwE
                 . EvalSuccess


