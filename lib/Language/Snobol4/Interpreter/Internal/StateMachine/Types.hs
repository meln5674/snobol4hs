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

-- | A user-defined function
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
    -- | A function defined by the user
    UserFunction :: ( InterpreterShell m
                    )
                 => UserFunction
                 -> Function program m
    -- | A function provided by the interpreter
    PrimitiveFunction :: ( InterpreterShell m
                         , NewSnobol4Machine m
                         , ProgramClass program
                         )
                      => {
                         -- | Name of the function
                            primName :: Snobol4String
                         -- | The primitive function to call
                         ,  funcPrim :: [Data (ExprType m)]
                                     -> InterpreterGeneric program
                                                        m (Maybe (Data (ExprType m)))
                         } -> Function program m
    -- | A function defined as a synonym for a unary operator
    FunctionUnOperatorSynonym :: Snobol4String -> Operator -> Function program m
    -- | A function defined as a synonym for a binary
    FunctionBinOperatorSynonym :: Snobol4String -> Operator -> Function program m
    -- | A function defined as a synonym for another function
    FunctionFunctionSynonym :: Snobol4String -> Function program m -> Function program m
    -- | A field selector for a user-defined datatype
    DataSelectorFunction :: Snobol4String -> Snobol4String -> Int -> Function program m
    -- | A constructor for a user-defined datatype
    DataConstructorFunction :: Snobol4String -> Int -> Function program m

-- | Get the name of a function
getFuncName :: Function program m -> Snobol4String
getFuncName (UserFunction func) = funcName func
getFuncName (PrimitiveFunction name _) = name
getFuncName (FunctionUnOperatorSynonym name _) = name
getFuncName (FunctionBinOperatorSynonym name _) = name
getFuncName (FunctionFunctionSynonym name _) = name
getFuncName (DataSelectorFunction name _ _) = name
getFuncName (DataConstructorFunction name _) = name

-- | Mimic the behavior of deriving Show, excluding the argument for
-- PrimitiveFunction
instance Show (Function program m) where
    show (UserFunction f) = show f
    show (PrimitiveFunction a _) = "(PrimitiveFunction " ++ show a ++ ")"
    show (FunctionUnOperatorSynonym a b) = "(FunctionUnOperatorSynonym " ++ show a ++ " " ++ show b ++ ")"
    show (FunctionBinOperatorSynonym a b) = "(FunctionBinOperatorSynonym " ++ show a ++ " " ++ show b ++ ")"
    show (FunctionFunctionSynonym a b) = "(FunctionFunctionSynonym " ++ show a ++ " " ++ show b ++ ")"
    show (DataSelectorFunction a b c) = "(DataSelectorFunction " ++ show a ++ " " ++ show b ++ " " ++ show c ++ ")"
    show (DataConstructorFunction a b) = "(DataConstructorFunction " ++ show a ++ " " ++ show b ++ ")"

-- | Functions are equal if they are the same type (user, operator, etc),
-- and if their arguments are the same (same name, same operator, etc)
--
-- Primitive functions are never considered equal
instance Eq (Function program m) where
    PrimitiveFunction{} == _ = False
    _ == PrimitiveFunction{} = False

    (UserFunction f) == (UserFunction f') = f == f';
    (UserFunction _) == _ = False
    _ == (UserFunction _) = False

    (FunctionUnOperatorSynonym a b) == (FunctionUnOperatorSynonym a' b') = a == a' && b == b'
    FunctionUnOperatorSynonym{} == _ = False
    _ == FunctionUnOperatorSynonym{} = False

    (FunctionBinOperatorSynonym a b) == (FunctionBinOperatorSynonym a' b') = a == a' && b == b'
    FunctionBinOperatorSynonym{} == _ = False
    _ == FunctionBinOperatorSynonym{} = False

    (FunctionFunctionSynonym a b) == (FunctionFunctionSynonym a' b') = a == a' && b == b'
    FunctionFunctionSynonym{} == _ = False
    _ == FunctionFunctionSynonym{} = False
    
    (DataSelectorFunction a b c) == (DataSelectorFunction a' b' c') = a == a' && b == b' && c == c'
    DataSelectorFunction{} == _ = False
    _ == DataSelectorFunction{} = False
    
    (DataConstructorFunction a b) == (DataConstructorFunction a' b') = a == a' && b == b'
    DataConstructorFunction{} == _ = False
    _ == DataConstructorFunction{} = False

-- | A label that can be jumped to
data Label
    = 
    -- | A normal goto label
      Label Address
    -- | A label within dynamic object code
    | CodeLabel CodeKey Address
  deriving Show

-- | A collection of variables
type StaticVars expr = Vector (Data expr)
-- | A mapping of names to variable locations
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

-- | An operator synonym
data OpSyn program m where
    -- | This operator has no meaning yet
    NoOperator :: OpSyn program m
    -- | An operator provided by the interpreter
    PrimitiveOperator :: ( NewSnobol4Machine m
                         )
                      => ( [Data (ExprType m)]
                         -> InterpreterGeneric program m (Maybe (Data (ExprType m)))
                         ) 
                      -> OpSyn program m
    -- | A synonym for another operator
    OperatorOperatorSynonym :: Operator -> OpSyn program m
    -- | A synonym for a function
    OperatorFunctionSynonym :: NewSnobol4Machine m => FuncType m -> OpSyn program m

-- | Mimic the deriving Show behavior, exluding the function argument to
-- PrimitiveOperator
instance (Show (FuncType m)) => Show (OpSyn program m) where
    show NoOperator = "NoOperator"
    show (PrimitiveOperator _) = "PrimitiveOperator"
    show (OperatorOperatorSynonym op) = "(OperatorOperatorSynonym " ++ show op ++ ")"
    show (OperatorFunctionSynonym func) = "(OperatorFunctionSynonym " ++ show func ++ ")"

-- | Collection of operator synonym
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

-- | Collection of unprotected keywords
type UnprotectedKeywords expr = Map Snobol4String (Data expr)

-- | Collection of protected keywords
type ProtectedKeywords expr = Map Snobol4String (Data expr)

-- | Class of types which have an empty version
class EmptyProgramClass program where
    -- | Empty value of type
    emptyProgram :: program
    
-- | Class of types which are a collection of instructions indexed by address
class ProgramClass program where
    -- | The type of instructions in the program
    type InstructionType program
    -- | Retreive an instruction by address
    getInstruction :: Address -> program -> InstructionType program

-- | State of the interpreter
data ProgramStateGeneric program m where
    ProgramState :: ( InterpreterShell m
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
        -- | Map of names to protected keywords
        , protectedKeywords :: ProtectedKeywords (ExprType m)
        -- | Map of names to unprotected keywords
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


-- | Lift an action to do nothing to the interpreter
instance MonadTrans (InterpreterGeneric a) where
    lift = Interpreter . lift . lift

-- | Type used as a program by an interpreter
type family ProgramType (m :: * -> *) :: *
-- | Type used to reference unevaluated expressions
type family ExprType (m :: * -> *) :: *
-- | Type used to reference functions
type family FuncType (m :: * -> *) :: *
--type ArgType m

-- | Class of monads which can register unevaluated expressions and then
-- evaluate them later
class (Monad m) => NewSnobol4Machine m where
    -- | Register an unevaluated expression
    registerExpr :: Expr -> InterpreterGeneric (ProgramType m) m (ExprType m)
    -- | Evaluate an unevaluated expression
    eval :: ( InterpreterShell m 
            )
         => ExprType m
         -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))

-- | Class of monads which can read and write local variables
class (Monad m) => LocalVariablesClass m where
    -- | Look up a local variable
    lookupLocal :: Int -> m (Maybe (Data (ExprType m)))
    -- | Assign a local variable
    writeLocal :: Int -> Data (ExprType m) -> m (Maybe ())

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
