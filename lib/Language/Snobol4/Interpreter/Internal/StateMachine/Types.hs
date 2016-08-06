{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  deriving (Eq, Ord, Bounded, Enum, Num)

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

-- | Information for calling a function
data Function m
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
    ,  funcPrim :: [Data] -> Evaluator m (Maybe Data)
    }



data Label
    = Label Address
    | CodeLabel CodeKey Address

type Variables = Map Snobol4String Data
type Statements = Vector Stmt
type Labels = Map Snobol4String Label
type Functions m = Map Snobol4String (Function m)
type Arrays = Map ArrayKey (RefCounted Snobol4Array)
type Tables = Map TableKey (RefCounted Snobol4Table)
type Patterns = Map PatternKey (RefCounted Pattern)
type Codes = Map CodeKey (RefCounted Snobol4Code)
type Datatypes = Map Snobol4String Snobol4Datatype
type UserDatas = Map UserKey Snobol4UserData


-- | State of the interpreter
data ProgramState m
    = ProgramState
    { 
    -- | A map of names to variables bound
      variables :: Variables
    -- | The statements in the current program
    , statements :: Statements
    -- | A map of label names to the index of their statement
    , labels :: Labels
    -- | The index of the current statement
    , programCounter :: Address
    -- | The functions known to the interpreter
    , functions :: Functions m
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


-- | Transformer stack which represents the actions of the interpreter
newtype Interpreter m a
    = Interpreter
    { runInterpreter
        :: ExceptT ProgramError (StateT (ProgramState m) m) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans Interpreter where
    lift = Interpreter . lift . lift

-- | Transformer stack for when the interpreter is evaluating a statement
newtype Evaluator m a 
    = Evaluator
    { runEvaluator
        :: ExceptT ProgramError (ExceptT EvalStop (StateT (ProgramState m) m)) a
    
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans Evaluator where
    lift = Evaluator . lift . lift . lift

-- | A paused interpreter
data PausedInterpreter m
    =
    -- | An interpreter that has been paused
      Paused (ProgramState m)
    -- | An interpreter that has been terminated
    | Terminated ProgramError

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

-- | Lift an operation from non-evaluation stack into evaluation stack
liftEval :: InterpreterShell m => Interpreter m a -> Evaluator m a
liftEval = Evaluator . ExceptT . lift . runExceptT . runInterpreter

