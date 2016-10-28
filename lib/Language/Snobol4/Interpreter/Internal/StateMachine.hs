{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine
Description     : The SNOBOL4 Interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExplicitForAll #-}
module Language.Snobol4.Interpreter.Internal.StateMachine 
    ( module Language.Snobol4.Interpreter.Internal.StateMachine 
    , Address (..)
    , ProgramStateGeneric
    , InterpreterGeneric
    , ProgramClass (..)
    , EmptyProgramClass (..)
    , PausedInterpreterGeneric (..)
    , NewSnobol4Machine (..)
    , ProgramType
    , ExprType
    , FuncType
    , LocalVariablesClass (..)
    , programError
    , execLookup
    --, liftEval
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
    
    , Forceable (..)
    , force

    , getProgramCounter
    , putProgramCounter
    , labelLookup
    , scanForLabels
    , Statements (..)
    , Label (..)
    , modifyProgramCounter
    , getProgram
    , putProgram
    , getProgramState

    , getReference
    , setReference
    
    , lookup
    , funcLookup
    , userDataLookup
    , lookupBinOpSyn
    , lookupUnOpSyn
    
    , putVariables
    , putLabels
    
    , userDataConstruct
    
    , getAnchorMode
    , incFailCount
    , setReturnType
    , incFunctionLevel
    , decFunctionLevel
    ) where

import Prelude hiding (toInteger, lookup)

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
import Language.Snobol4.Interpreter.Internal.StateMachine.GC
import Language.Snobol4.Interpreter.Internal.StateMachine.Error
import Language.Snobol4.Interpreter.Internal.StateMachine.Labels
import Language.Snobol4.Interpreter.Internal.StateMachine.Keywords
import Language.Snobol4.Interpreter.Internal.StateMachine.Convert
import Language.Snobol4.Interpreter.Internal.StateMachine.Lazy
--import Language.Snobol4.Interpreter.Internal.StateMachine.Run


-- | A ProgramState no functions or variables, no program
-- loaded, an empty call stack, and pointing at the first statement
emptyState :: ( EmptyProgramClass program
--              {-, Snobol4Machine program-}
              , InterpreterShell m 
              , NewSnobol4Machine m
              )
           => ProgramStateGeneric program m
emptyState = ProgramState
    noVariables
    emptyProgram
    noLabels
    initialProgramCounter
    noFunctions
    noArrays
    noTables
    noPatterns
    noCodes
    noDatatypes
    noUserData
    noBinOpSyns
    noUnOpSyns
    noProtectedKeywords
    noUnprotectedKeywords

-- | Execute an interpreter action
interpret :: ( InterpreterShell m 
             , EmptyProgramClass program
             )
          => ProgramStateGeneric program m
          -> InterpreterGeneric program m a 
          -> m (Either ProgramError a)
interpret st m = flip evalStateT st
        $ runExceptT 
        $ runInterpreter
        $ m

-- | Don't ask
--
-- See at Language.Snobol4.VM.Bytecode.Interpreter.Internal.mkVM
mkInterpreterGeneric :: ( InterpreterShell m 
                        ) 
                     => ( forall s e
                        .  ( forall b . InterpreterGeneric program m b -> s -> m (Either e b, s) )
                        -> ( forall b . (s -> m (Either e b, s)) -> InterpreterGeneric program m b )
                        -> s 
                        -> m (Either e a, s) 
                        )
                     -> InterpreterGeneric program m a
mkInterpreterGeneric f = Interpreter $ ExceptT $ StateT $ f runFunc stateFunc
  where
    runFunc g st = runStateT (runExceptT $ runInterpreter g) st
    stateFunc = Interpreter . ExceptT . StateT
