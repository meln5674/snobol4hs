{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine
Description     : The SNOBOL4 Interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE ExplicitForAll #-}
module Language.Snobol4.Interpreter.Internal.StateMachine 
    ( module Language.Snobol4.Interpreter.Internal.StateMachine 
    , Address (..)
    , InterpreterGeneric (..)
    , Interpreter
    , EvaluatorGeneric (..)
    , Evaluator
    , PausedInterpreter (..)
    , Snobol4Machine (..)
    , programError
    , execLookup
    , liftEval
    , failEvaluation
    , finishEvaluation
    , EvalStop (..)
    , toString
    , toInteger
    , toReal
    , toPattern
    , toCode
    , raiseArgs
    , lowerArgs
    , assign
    , ScanResult (..)
    , fetch
    , ExecResult (..)
    , catchEval
    , liftEval
    , unliftEval
    , getProgramCounter
    , putProgramCounter
    , labelLookup
    , scanForLabels
    , Statements (..)
    , callFunction
    , Label (..)
    , modifyProgramCounter
    , putProgram
    , getProgramState
    ) where

import Prelude hiding (toInteger)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Shell

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.Variables
import Language.Snobol4.Interpreter.Internal.StateMachine.Statements
import Language.Snobol4.Interpreter.Internal.StateMachine.Functions
import Language.Snobol4.Interpreter.Internal.StateMachine.Patterns
import Language.Snobol4.Interpreter.Internal.StateMachine.Arrays
import Language.Snobol4.Interpreter.Internal.StateMachine.Tables
import Language.Snobol4.Interpreter.Internal.StateMachine.ObjectCode
import Language.Snobol4.Interpreter.Internal.StateMachine.UserData
import Language.Snobol4.Interpreter.Internal.StateMachine.CallStack
import Language.Snobol4.Interpreter.Internal.StateMachine.GC
import Language.Snobol4.Interpreter.Internal.StateMachine.Error
import Language.Snobol4.Interpreter.Internal.StateMachine.Labels
import Language.Snobol4.Interpreter.Internal.StateMachine.Convert
import Language.Snobol4.Interpreter.Internal.StateMachine.Run


-- | A ProgramState no functions or variables, no program
-- loaded, an empty call stack, and pointing at the first statement
emptyState :: forall m . InterpreterShell m => ProgramState m
emptyState = ProgramState
    noVariables
    emptyProgram
    noLabels
    initialProgramCounter
    noFunctions
    emptyCallStack
    noArrays
    noTables
    noPatterns
    noCodes
    noDatatypes
    noUserData

-- | Execute an interpreter action
interpret :: InterpreterShell m 
          => ProgramState m
          -> Interpreter m a 
          -> m (Either ProgramError a)
interpret st m = flip evalStateT st
        $ runExceptT 
        $ runInterpreter
        $ m
