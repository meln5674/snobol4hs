module Language.Snobol4.Interpreter.Types where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import Language.Snobol4.Syntax.AST

-- State of the interpreter
data ProgramState
    = ProgramState
    -- A map of names to variables bound
    { variables :: Map String Data
    -- The statements in the current program
    , statements :: Vector Stmt
    -- A map of label names to the index of their statement
    , labels :: Map String Int
    -- The index of the current statement
    , programCounter :: Int
    }

emptyState :: ProgramState
emptyState = ProgramState M.empty V.empty M.empty 0

-- Result of an evaluation step
data ExecResult a           
    -- Evaluation succeeded with a value
    = Success a
    -- Evaluation failed
    | Failure
    -- Evaluation failed in such a way that the program needs to terminate
    -- | Error ProgramError

data EvalStop
    = EvalSuccess
    | EvalFailed
  deriving Show

-- A lookup request
data Lookup 
    = Lookup String
    | Output
    | Input
    | Punch
  deriving Show

-- The data types allowed in a snobol4 program
data Data
    -- A string
    = StringData String
    -- A pattern
    | PatternData Pattern
    -- An integer
    | IntegerData Int
    -- A real number
    | RealData Float
    -- An array
    | ArrayData (Vector Data)
    -- A table
    | TableData (Map Data Data)
    | Name Expr
    | Unevaluated Expr
  deriving Show

-- A pattern
data Pattern
    -- A pattern which records the matched value in the provided lookup on 
    -- success
    = AssignmentPattern Pattern Lookup
    -- A pattern which records the matched value in the provided lookup 
    -- immediately after matching
    | ImmediateAssignmentPattern String String
    -- A pattern to match a literal string
    | LiteralPattern String
    -- An alternative between two pattern
    | AlternativePattern Pattern Pattern
    -- A concatination of two patterns
    | ConcatPattern Pattern Pattern
    -- A pattern which matches any string of N characters
    | LengthPattern Int
    -- A pattern which matches anything
    | EverythingPattern
  deriving Show

-- A program error
data ProgramError
    -- TODO
    = ProgramError
    | NormalTermination
  deriving Show
