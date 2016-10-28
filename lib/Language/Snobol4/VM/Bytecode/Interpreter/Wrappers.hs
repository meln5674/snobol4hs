{-|
Module          : Language.Snobol4.VM.Bytecode.Interpreter.Wrappers
Description     : Virtual Machine Convienience Functions
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Snobol4.VM.Bytecode.Interpreter.Wrappers 
    ( programError
    , getProgramCounter
    , putProgramCounter
    , incProgramCounter
    , lookup
    , assign
    , getFailLabel
    , putFailLabel
    , putSystemLabels
    , lookupSystemLabel
    , push
    , pop
    , popFailStack
    , popToCallStackFrame
    , popCallStackFrame
    , pushCallStackFrame
    , getCallStackFrameStart
    , pushFailLabel
    , popFailLabel
    , getLoadedProgram
    , execLookup
    , toString
    ) where

import Prelude hiding (lookup)

import Data.Vector (Vector)

import Control.Monad.Trans

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Shell

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import qualified Language.Snobol4.Interpreter.Internal.StateMachine as I
import qualified Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState as I

import Language.Snobol4.VM.Bytecode

import Language.Snobol4.VM.Bytecode.Interpreter.Types

import Language.Snobol4.VM.Bytecode.Interpreter.StackMachine (StackMachine)
import qualified Language.Snobol4.VM.Bytecode.Interpreter.StackMachine as S

-- | Terminate the VM with an error
programError :: (Monad m) => ProgramError -> VM m a
programError = VM . I.programError

-- | Retreive the label to jump to on failure
getFailLabel :: (Monad m) => VM m Address
getFailLabel = VM $ lift S.getFailLabel

-- | Overwrite the label to jump to on failure, and create a checkpoint to
-- restore to if that happens
putFailLabel :: ( Monad m) => Address -> VM m ()
putFailLabel = VM . lift . S.putFailLabel

-- | Set the compiler generated labels
putSystemLabels :: (Monad m) => Vector Address -> VM m ()
putSystemLabels = VM . lift . S.putSystemLabels

-- | Reterieve a compiler generated label
lookupSystemLabel :: (Monad m) => SystemLabel -> VM m Address
lookupSystemLabel = VM . lift . S.lookupSystemLabel

-- | Push onto the stack
push :: Monad m => Data ExprKey -> VM m ()
push = VM . lift . S.push

-- | Pop from the stack
pop :: Monad m => VM m (Data ExprKey)
pop = (VM $ lift S.pop) >>= \case
    Nothing -> programError ErrorInSnobol4System
    Just x -> return x

-- | Restore the stack to the state before the current failure handler was set
popFailStack :: Monad m => VM m ()
popFailStack = VM $ lift S.popFailStack

-- | Pop all items from the current stack frame
popToCallStackFrame :: Monad m => VM m ()
popToCallStackFrame = VM $ lift S.popToCallStackFrame

-- | Pop the current stack frame
popCallStackFrame :: Monad m => VM m ()
popCallStackFrame = do
    x <- VM $ lift S.popCallStackFrame
    case x of
        Just _ -> return ()
        Nothing -> programError ErrorInSnobol4System

-- | Push a new stack frame
pushCallStackFrame :: Monad m => Int -> VM m ()
pushCallStackFrame = VM . lift . S.pushCallStackFrame

-- | Get the number of items on the current stack frame
getCallStackFrameStart :: Monad m => VM m Int
getCallStackFrameStart = VM $ lift S.getCallStackFrameStart

-- | Push the current failure handler and checkpoint onto the stack and then
-- create a new one
pushFailLabel :: Monad m => Address -> VM m ()
pushFailLabel = VM . lift . S.pushFailLabel

-- | Pop a failure handler and checkpoint from the stack
popFailLabel :: Monad m => VM m ()
popFailLabel = do
    x <- VM $ lift S.popFailLabel
    case x of
        Just _ -> return ()
        Nothing -> programError ErrorInSnobol4System

-- | Get the address of the current instruction
getProgramCounter :: InterpreterShell m => VM m Address
getProgramCounter = VM I.getProgramCounter

-- | Set the address of the current instruction
putProgramCounter :: InterpreterShell m => Address -> VM m ()
putProgramCounter = VM . I.putProgramCounter

-- | Set the address of the current instruction to the next one
incProgramCounter :: InterpreterShell m => VM m ()
incProgramCounter = VM I.incProgramCounter

-- | Retreive the value of a variable, etc, or null if it doesn't exist
lookup :: ( InterpreterShell m 
          , NewSnobol4Machine (StackMachine m)
          )
       => Lookup ExprKey
       -> VM m (Data ExprKey)
lookup = VM . I.lookup

-- | Assign the value of a variable, etc
assign :: ( InterpreterShell m
          , NewSnobol4Machine (StackMachine m)
          )
       => Lookup ExprKey
       -> Data ExprKey
       -> VM m ()
assign s = VM . I.assign s


-- | Get the program loaded into the VM
getLoadedProgram :: InterpreterShell m => VM m CompiledProgram
getLoadedProgram = VM I.getProgram

-- | Look up a variable, array, etc
execLookup :: ( InterpreterShell m 
              , NewSnobol4Machine (StackMachine m)
              )
    => Lookup ExprKey
    -> VM m (Maybe (Data ExprKey))
execLookup = VM . I.execLookup

-- | Convert a value to a string
toString :: ( InterpreterShell m
            , NewSnobol4Machine (StackMachine m)
            )
         => Data ExprKey
         -> VM m Snobol4String
toString = VM . I.toString
