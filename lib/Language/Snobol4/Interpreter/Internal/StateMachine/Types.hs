{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Types
Description     : Types used by the state machine
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Snobol4.Interpreter.Internal.StateMachine.Types 
    ( module Language.Snobol4.Interpreter.Internal.StateMachine.Types 
    , module Language.Snobol4.Interpreter.Internal.StateMachine.Error.Types 
    ) where

import Prelude hiding (toInteger)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Array (Array)
import qualified Data.Array as A

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Shell

import Language.Snobol4.Interpreter.Internal.StateMachine.GC.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Error.Types

-- | Address of a statement
newtype Address = Address { getAddress :: Snobol4Integer }
  deriving (Show, Eq, Ord, Bounded, Enum, Num)

-- | A node of the call stack
data CallStackFrame
    = Frame
    { 
    -- | Static local variables at the current frame
      locals :: StaticVars
    -- | References held by the return value, arguments, and local variable names
    -- before calling
    , oldReferences :: [(Snobol4String,Maybe VarType)]
    -- | The index of the statement that called this function
    , returnAddr :: Address
    -- | The name of the function called
    , callName :: Snobol4String
    }
  deriving Show

{-
-- | Information for calling a function
data Function program instruction evaluationError m
    -- A user defined function
    = UserFunction
    { 
    -- | Name of the function
      funcName :: Snobol4String
    -- | The names of the formal arguments of the function
    , formalArgs :: [Snobol4String]
    -- | The names of the local variables of the function
    , localNames :: [Snobol4String]
    -- | Index of the statement to start this function
    , entryPoint :: Address
    }
    | PrimitiveFunction
    {
    -- | Name of the function
       funcName :: Snobol4String
    -- | The primitive function to call
    ,  funcPrim :: [Data] ->  EvaluatorGeneric program evaluationError m (Maybe Data)
    }
-}

data UserFunction 
    = Function
    { 
    -- | Name of the function
      funcName :: Snobol4String
    -- | The names of the formal arguments of the function
    , formalArgs :: [Snobol4String]
    -- | The names of the local variables of the function
    , localNames :: [Snobol4String]
    -- | Index of the statement to start this function
    , entryPoint :: Address
    }
  deriving (Show, Eq)

-- | Information for calling a function
data Function program (m :: * -> *) where
    UserFunction :: ( InterpreterShell m
                    , Snobol4Machine program
                    , ProgramClass program
                   )
         => UserFunction
         -> Function program m
    PrimitiveFunction :: ( InterpreterShell m
             , Snobol4Machine program
             , ProgramClass program
             )
          => 
        {
        -- | Name of the function
           primName :: Snobol4String
        -- | The primitive function to call
        ,  funcPrim :: [Data] 
                    -> EvaluatorGeneric program 
                                        (EvaluationError program) 
                                        m (Maybe Data)
        } -> Function program m

-- | 
instance Show (Function program m) where
    show (UserFunction f) = show f
    show (PrimitiveFunction a _) = "(PrimitiveFunction " ++ show a ++ ")"

-- |
instance Eq (Function program m) where
    PrimitiveFunction{} == _ = False
    _ == PrimitiveFunction{} = False
    (UserFunction f) == (UserFunction f') = f == f;

-- | A label that can be jumped to
data Label
    = 
    -- | A normal goto label
      Label Address
    -- | A label within dynamic object code
    | CodeLabel CodeKey Address
  deriving Show

type StaticVars = Vector Data
type DynamicVars = Map Snobol4String VarType

-- | Flag for variables as local or global
data VarType = LocalVar Int | GlobalVar Int deriving Show

-- | Collection of variables
data Variables = Variables
    { staticVars :: StaticVars
    , dynamicVars :: DynamicVars
    }
  deriving Show

-- | A loaded program
newtype Statements = Statements { getStatements :: Vector Stmt }
  deriving (Show)

-- | Collection of labels
type Labels = Map Snobol4String Label

-- | Collection of functions
type Functions program m = Map Snobol4String (Function program m)

-- | Collection of arrays
type Arrays = Map ArrayKey (RefCounted Snobol4Array)

-- | Collection of tables
type Tables = Map TableKey (RefCounted Snobol4Table)

-- | Collection of patterns
type Patterns = Map PatternKey (RefCounted Pattern)

-- | Collection of object code
type Codes = Map CodeKey (RefCounted Snobol4Code)

-- | Collection of user-defined datatypes
type Datatypes = Map Snobol4String Snobol4Datatype

-- | Collection of values of user-defined datatypes
type UserDatas = Map UserKey Snobol4UserData


class EmptyProgramClass program where
    emptyProgram :: program
    
class ProgramClass program where
    type InstructionType program
    getInstruction :: Address -> program -> InstructionType program

{-
-- | State of the interpreter
data ProgramStateGeneric program instruction m
    = ProgramState
    { 
    -- | A map of names to variables bound
      variables :: Variables
    -- | The statements in the current program
    , program :: program
    -- | A map of label names to the index =of their statement
    , labels :: Labels
    -- | The index of the current statement
    , programCounter :: Address
    -- | The functions known to the interpreter
    , functions :: Functions program m
    -- | The call stack
    , callStack :: [CallStackNode]
    -- | The arrays known to the interpreter
    , arrays :: Arrays
    -- | The tables known to the interpreter
    , tables :: Tables
    -- | The patterns known to the interpreter
    , patterns :: Patterns
    -- | Object code values known to the interpreter
    , codes :: Codes
    -- | User-defined datatype known to the interpreter
    , datatypes :: Datatypes
    -- | User define datatype values known to the interpreter
    , userDatas :: UserDatas
    }
  deriving Show
-}

-- | State of the interpreter
data ProgramStateGeneric program m where
    ProgramState :: ( InterpreterShell m, Snobol4Machine program )
                 => 
        { 
        -- | A map of names to variables bound
          variables :: Variables
        -- | The statements in the current program
        , program :: program
        -- | A map of label names to the index =of their statement
        , labels :: Labels
        -- | The index of the current statement
        , programCounter :: Address
        -- | The functions known to the interpreter
        , functions :: Functions program m
        -- | The call stack
        , callStack :: [CallStackFrame]
        -- | The arrays known to the interpreter
        , arrays :: Arrays
        -- | The tables known to the interpreter
        , tables :: Tables
        -- | The patterns known to the interpreter
        , patterns :: Patterns
        -- | Object code values known to the interpreter
        , codes :: Codes
        -- | User-defined datatype known to the interpreter
        , datatypes :: Datatypes
        -- | User define datatype values known to the interpreter
        , userDatas :: UserDatas
        } -> ProgramStateGeneric program m

deriving instance (Show program, Show (Function program m)) => Show (ProgramStateGeneric program m)

-- | Transformer stack which represents the actions of the interpreter
newtype InterpreterGeneric program m a
    = Interpreter
    { runInterpreter
        :: ExceptT ProgramError (StateT (ProgramStateGeneric program m) m) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)


-- |
instance MonadTrans (InterpreterGeneric a) where
    lift = Interpreter . lift . lift

-- | Transformer stack for when the interpreter is evaluating a statement
newtype EvaluatorGeneric program error (m :: * -> *) a
    = Evaluator
    { runEvaluator
        :: ExceptT ProgramError 
          (ExceptT error
          (StateT (ProgramStateGeneric program m)
           m
          )
          ) a
    
    }
  deriving (Functor, Applicative, Monad, MonadIO)

class (ProgramClass program) => Snobol4Machine (program :: *) where
    type EvaluationError program
    call :: InterpreterShell m 
         => Snobol4String 
         -> [Data] 
         -> EvaluatorGeneric program (EvaluationError program) m (Maybe Data)
    eval :: InterpreterShell m 
         => Expr 
         -> EvaluatorGeneric program (EvaluationError program) m Data
    failEval :: InterpreterShell m
             => EvaluatorGeneric program (EvaluationError program) m a

-- |
instance MonadTrans (EvaluatorGeneric a b) where
    lift = Evaluator . lift . lift . lift

-- | A paused interpreter
data PausedInterpreterGeneric program m
    =
    -- | An interpreter that has been paused
      Paused (ProgramStateGeneric program m)
    -- | An interpreter that has been terminated
    | Terminated ProgramResult
  deriving Show

-- | A result from running the pattern scanner
data ScanResult
    = 
    -- | No match
      NoScan
    -- | A match with the matched part, assignments to perform, and the start
    -- and end index of the entire match
    | Scan Data [(Lookup,Data)] Snobol4Integer Snobol4Integer
  deriving Show

-- | The result of executing a statement
data ExecResult
    = 
    -- | The statement executed and returned a result or nothing
      StmtResult (Maybe Data)
    -- | The statement resulted in returning from the current function call
    | Return
    -- | The statement resulted in returning from the current function call
    -- with failure
    | FReturn
    -- | The statement executed was the end statement
    | EndOfProgram
