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
                    {-, Snobol4Machine program-}
                    --, ProgramClass program
                   )
         => UserFunction
         -> Function program m
    PrimitiveFunction :: ( InterpreterShell m
             {-, Snobol4Machine program-}
             , NewSnobol4Machine m
             , ProgramClass program
             )
          => 
        {
        -- | Name of the function
           primName :: Snobol4String
        -- | The primitive function to call
        ,  funcPrim :: [Data (ExprType m)]
                    -> InterpreterGeneric program
                                        m (Maybe (Data (ExprType m)))
        } -> Function program m
    FunctionUnOperatorSynonym :: Snobol4String -> Operator -> Function program m
    FunctionBinOperatorSynonym :: Snobol4String -> Operator -> Function program m
    FunctionFunctionSynonym :: Snobol4String -> Function program m -> Function program m
    DataSelectorFunction :: Snobol4String -> Snobol4String -> Int -> Function program m
    DataConstructorFunction :: Snobol4String -> Int -> Function program m
    
getFuncName :: Function program m -> Snobol4String
getFuncName (UserFunction func) = funcName func
getFuncName (PrimitiveFunction name _) = name
getFuncName (FunctionUnOperatorSynonym name _) = name
getFuncName (FunctionBinOperatorSynonym name _) = name
getFuncName (FunctionFunctionSynonym name _) = name

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

type StaticVars expr = Vector (Data expr)
type DynamicVars = Map Snobol4String VarType

-- | Flag for variables as local or global
data VarType 
    = LocalVar Int
    | GlobalVar Int
  deriving Show

-- | Collection of variables
data Variables expr = Variables
    { staticVars :: StaticVars expr
    , dynamicVars :: DynamicVars
    }
  deriving Show

data OpSyn program m where
    NoOperator :: OpSyn program m
    PrimitiveOperator :: ( NewSnobol4Machine m
                         )
                      => ( [Data (ExprType m)]
                         -> InterpreterGeneric program m (Maybe (Data (ExprType m)))
                         ) 
                      -> OpSyn program m
    OperatorOperatorSynonym :: Operator -> OpSyn program m
    OperatorFunctionSynonym :: NewSnobol4Machine m => FuncType m -> OpSyn program m


instance (Show (FuncType m)) => Show (OpSyn program m) where
    show NoOperator = "NoOperator"
    show (PrimitiveOperator _) = "PrimitiveOperator"
    show (OperatorOperatorSynonym op) = "OperatorOperatorSynonym " ++ show op
    show (OperatorFunctionSynonym func) = "OperatorFunctionSynonym " ++ show func

type OpSyns program m = Map Operator (OpSyn program m)

-- | A loaded program
newtype Statements = Statements { getStatements :: Vector Stmt }
  deriving (Show)

-- | Collection of labels
type Labels = Map Snobol4String Label

-- | Collection of functions
type Functions program m = Map Snobol4String (Function program m)

-- | Collection of arrays
type Arrays expr = Map ArrayKey (RefCounted (Snobol4Array expr))

-- | Collection of tables
type Tables expr = Map TableKey (RefCounted (Snobol4Table expr))

-- | Collection of patterns
type Patterns expr = Map PatternKey (RefCounted (Pattern expr))

-- | Collection of object code
type Codes = Map CodeKey (RefCounted Snobol4Code)

-- | Collection of user-defined datatypes
type Datatypes = Map Snobol4String Snobol4Datatype

-- | Collection of values of user-defined datatypes
type UserDatas expr = Map UserKey (Snobol4UserData expr)

type UnprotectedKeywords expr = Map Snobol4String (Data expr)

type ProtectedKeywords expr = Map Snobol4String (Data expr)

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
    ProgramState :: ( InterpreterShell m
--                    {-, Snobol4Machine program-}
                    , NewSnobol4Machine m
                    )
                 => 
        { 
        -- | A map of names to variables bound
          variables :: Variables (ExprType m)
        -- | The statements in the current program
        , program :: program
        -- | A map of label names to the index of their statement
        , labels :: Labels
        -- | The index of the current statement
        , programCounter :: Address
        -- | The functions known to the interpreter
        , functions :: Functions program m
        -- | The arrays known to the interpreter
        , arrays :: Arrays (ExprType m)
        -- | The tables known to the interpreter
        , tables :: Tables (ExprType m)
        -- | The patterns known to the interpreter
        , patterns :: Patterns (ExprType m)
        -- | Object code values known to the interpreter
        , codes :: Codes
        -- | User-defined datatype known to the interpreter
        , datatypes :: Datatypes
        -- | User define datatype values known to the interpreter
        , userDatas :: UserDatas (ExprType m)
        -- | Map of binary operators to their functions
        , binOpSyns :: OpSyns program m
        -- | Map of unary operators to their functions
        , unOpSyns :: OpSyns program m 
        , protectedKeywords :: ProtectedKeywords (ExprType m)
        , unprotectedKeywords :: UnprotectedKeywords (ExprType m)
        } -> ProgramStateGeneric program m

deriving instance ( Show program
                  , Show (Function program m)
                  , Show (ExprType m)
                  , Show (FuncType m)
                  ) => Show (ProgramStateGeneric program m)

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

{-
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
-}

{-
class (ProgramClass program) => Snobol4Machine (program :: *) where
    type EvaluationError program
    --type CallStackFrame program
    call :: ( InterpreterShell m 
            , LocalVariablesClass m
            )
         => Snobol4String 
         -> [Data] 
         -> EvaluatorGeneric program (EvaluationError program) m (Maybe Data)
    eval :: ( InterpreterShell m 
            , LocalVariablesClass m
            )
         => Expr 
         -> EvaluatorGeneric program (EvaluationError program) m Data
    failEval :: ( InterpreterShell m
                , LocalVariablesClass m
                )
             => EvaluatorGeneric program (EvaluationError program) m a
-}

class (Monad m) => NewSnobol4Machine m where
    type ProgramType m
    type ExprType m
    type FuncType m
    type ArgType m
    eval :: ExprType m
         -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
    registerExpr :: Expr -> InterpreterGeneric (ProgramType m) m (ExprType m)

class (NewSnobol4Machine m) => LocalVariablesClass m where
    lookupLocal :: Int -> m (Maybe (Data (ExprType m)))
    writeLocal :: Int -> Data (ExprType m) -> m (Maybe ())

{-
-- |
instance MonadTrans (EvaluatorGeneric a b) where
    lift = Evaluator . lift . lift . lift
-}

-- | A paused interpreter
data PausedInterpreterGeneric program m
    =
    -- | An interpreter that has been paused
      Paused (ProgramStateGeneric program m)
    -- | An interpreter that has been terminated
    | Terminated ProgramResult

deriving instance ( Show program
                  , Show (ExprType m)
                  , Show (FuncType m)
                  ) => Show (PausedInterpreterGeneric program m)

-- | A result from running the pattern scanner
data ScanResult expr
    = 
    -- | No match
      NoScan
    -- | A match with the matched part, assignments to perform, and the start
    -- and end index of the entire match
    | Scan (Data expr) [(Lookup expr, Data expr)] Snobol4Integer Snobol4Integer
  deriving Show

-- | The result of executing a statement
data ExecResult expr
    = 
    -- | The statement executed and returned a result or nothing
      StmtResult (Maybe (Data expr))
    -- | The statement resulted in returning from the current function call
    | Return
    -- | The statement resulted in returning from the current function call
    -- with failure
    | FReturn
    -- | The statement executed was the end statement
    | EndOfProgram
