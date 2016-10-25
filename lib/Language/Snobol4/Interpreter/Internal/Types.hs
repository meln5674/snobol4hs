{-|
Module          : Language.Snobol4.Interpreter.Internal.Types
Description     : Types used by the AST interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown


-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.Interpreter.Internal.Types where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import qualified Data.Vector as V

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Internal.StateMachine
import Language.Snobol4.Interpreter.Internal.StateMachine.Types

-- | Expressions used by the stack machine
type StackMachineExpr = Expr

-- | Data used by the stack machine
type StackMachineData = Data StackMachineExpr

-- | A paused AST interpreter
type PausedInterpreter m = PausedInterpreterGeneric Statements (StackMachine m)

-- | State of the AST interpreter
type ProgramState m = ProgramStateGeneric Statements (StackMachine m)

-- | Monad for the AST interpreter
type Interpreter m = InterpreterGeneric Statements (StackMachine m)

-- | A stack
type Stack = []

-- | Stack machine
data StackMachineState
    = StackMachineState
    { 
      stack :: Stack CallStackFrame
    }

-- | Monad for running the stack machine
newtype StackMachine m a = StackMachine
    { runStackMachineInternal :: StateT StackMachineState m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | An empty program has no statements
instance EmptyProgramClass Statements where
    emptyProgram = Statements V.empty

-- | Program has statements indexed by address
instance ProgramClass Statements where
    type InstructionType Statements = Stmt
    getInstruction (Address ix) (Statements v) = v V.! unmkInteger ix

-- | A frame of the call stack
data CallStackFrame
    = Frame
    { 
    -- | Static local variables at the current frame
      locals :: StaticVars StackMachineExpr
    -- | References held by the return value, arguments, and local variable names
    -- before calling
    , oldReferences :: [(Snobol4String,Maybe VarType)]
    -- | The index of the statement that called this function
    , returnAddr :: Address
    -- | The name of the function called
    , callName :: Snobol4String
    }
  deriving Show

-- | Lift another InterpreterShell into the stack machine
instance InterpreterShell m => InterpreterShell (StackMachine m) where
    input = lift input
    output = lift . output
    punch = lift . output
    lastOutput = lift lastOutput
    lastPunch = lift lastPunch
    date = lift date
    time = lift time
