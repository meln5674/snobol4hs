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

type StackMachineExpr = Expr
type StackMachineData = Data StackMachineExpr

type PausedInterpreter m = PausedInterpreterGeneric Statements (StackMachine m)

type ProgramState m = ProgramStateGeneric Statements (StackMachine m)

type Interpreter m = InterpreterGeneric Statements (StackMachine m)

--type Evaluator m = EvaluatorGeneric Statements EvalStop (StackMachine m)

-- | A stack
type Stack = []


data StackMachineState
    = StackMachineState
    { 
      stack :: Stack CallStackFrame
    }

-- | Monad for running the stack machine
newtype StackMachine m a = StackMachine
    { runStackMachineInternal :: StateT StackMachineState m a }
  deriving (Functor, Applicative, Monad, MonadTrans)


instance EmptyProgramClass Statements where
    emptyProgram = Statements V.empty

instance ProgramClass Statements where
    type InstructionType Statements = Stmt
    getInstruction (Address ix) (Statements v) = v V.! unmkInteger ix

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

instance InterpreterShell m => InterpreterShell (StackMachine m) where
    input = lift input
    output = lift . output
    punch = lift . output
    lastOutput = lift lastOutput
    lastPunch = lift lastPunch
    date = lift date
    time = lift time
