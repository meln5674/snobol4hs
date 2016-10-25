{-|
Module          : Language.Snobol4.VM.Bytecode.Interpreter.Types
Description     : Virtual Machine Types
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.VM.Bytecode.Interpreter.Types where

import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Trans

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine

import Language.Snobol4.VM.Bytecode

import Language.Snobol4.VM.Bytecode.Interpreter.StackMachine

-- | A paused virtual machine
data PausedVM m = PausedVM (PausedInterpreterGeneric CompiledProgram (StackMachine m))
                           StackMachineState

-- | State of the virtual machine
type VMState = ProgramStateGeneric CompiledProgram

-- | Monad transformer for the interpreter
newtype VM m a = VM { runVMInternal :: InterpreterGeneric CompiledProgram (StackMachine m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Lift actions to do make no changes to the VM
instance MonadTrans VM where
    lift = VM . lift . lift

-- | Empty program is a program with no instructions
instance EmptyProgramClass CompiledProgram where
    emptyProgram = CompiledProgram V.empty

-- | Program contains bytecode instructions, indexed by address
instance ProgramClass CompiledProgram where
    type InstructionType CompiledProgram = Instruction
    getInstruction (Address ix) (CompiledProgram is) = is V.! unmkInteger ix
