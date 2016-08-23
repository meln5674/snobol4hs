{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Types
Description     : Types used by the state machine
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
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
data CallStackNode
    = Node
    { 
    -- | Local variables
      locals :: Map Snobol4String Data
    -- | The index of the statement that called this function
    , returnAddr :: Address
    -- | The name of the function called
    , callName :: Snobol4String
    }
  deriving Show

-- | Information for calling a function
data Function program instruction m
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
    ,  funcPrim :: [Data] -> EvaluatorGeneric program instruction m (Maybe Data)
    }

-- | 
instance Show (Function program instruction m) where
    show (UserFunction a b c d) = "(UserFunction " 
                                ++ show a 
                                ++ " " 
                                ++ show b 
                                ++ " " 
                                ++ show c 
                                ++ " " 
                                ++ show d 
                                ++ ")"
    show (PrimitiveFunction a _) = "(PrimitiveFunction " ++ show a ++ ")"

-- |
instance Eq (Function program instruction m) where
    PrimitiveFunction{} == _ = False
    _ == PrimitiveFunction{} = False
    (UserFunction a b c d) == (UserFunction a' b' c' d') = a == a' && b == b' && c == c' && d == d'

-- | A label that can be jumped to
data Label
    = 
    -- | A normal goto label
      Label Address
    -- | A label within dynamic object code
    | CodeLabel CodeKey Address
  deriving Show

-- | Collection of variables
type Variables = Map Snobol4String Data

-- | A loaded program
newtype Statements = Statements { getStatements :: Vector Stmt }
  deriving (Show)

-- | Collection of labels
type Labels = Map Snobol4String Label

-- | Collection of functions
type Functions program instruction m = Map Snobol4String (Function program instruction m)

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
    
class ProgramClass program instruction | program -> instruction where
    getInstruction :: Address -> program -> instruction

instance EmptyProgramClass Statements where
    emptyProgram = Statements V.empty

instance ProgramClass Statements Stmt where
    getInstruction (Address ix) (Statements v) = v V.! unmkInteger ix

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
    , functions :: Functions program instruction m
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

-- | State of the interpreter
type ProgramState = ProgramStateGeneric Statements Stmt

-- | Transformer stack which represents the actions of the interpreter
newtype InterpreterGeneric program instruction m a
    = Interpreter
    { runInterpreter
        :: ExceptT ProgramError (StateT (ProgramStateGeneric program instruction m) m) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

type Interpreter = InterpreterGeneric Statements Stmt

-- |
instance MonadTrans (InterpreterGeneric a b) where
    lift = Interpreter . lift . lift

-- | Transformer stack for when the interpreter is evaluating a statement
newtype EvaluatorGeneric program instruction m a 
    = Evaluator
    { runEvaluator
        :: ExceptT ProgramError 
          (ExceptT EvalStop 
          (StateT (ProgramStateGeneric program instruction m) 
           m
          )
          ) a
    
    }
  deriving (Functor, Applicative, Monad, MonadIO)

type Evaluator = EvaluatorGeneric Statements Stmt

class Snobol4Machine program instruction | program -> instruction where
    call :: InterpreterShell m => Snobol4String -> [Data] -> EvaluatorGeneric program instruction m (Maybe Data)
    eval :: InterpreterShell m => Expr -> EvaluatorGeneric program instruction m Data
    code :: InterpreterShell m => Program -> EvaluatorGeneric program instruction m Data

-- |
instance MonadTrans (EvaluatorGeneric a b) where
    lift = Evaluator . lift . lift . lift

-- | A paused interpreter
data PausedInterpreter m
    =
    -- | An interpreter that has been paused
      Paused (ProgramState m)
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
