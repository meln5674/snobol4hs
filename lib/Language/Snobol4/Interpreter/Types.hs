{-|
Module          : Language.Snobol4.Interpreter.Types
Description     : Types used by the SNOBOL4 Interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Types where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import Language.Snobol4.Syntax.AST


-- | State of the interpreter
data ProgramState
    = ProgramState
    { 
    -- | A map of names to variables bound
       variables :: Map String Data 
    -- | The statements in the current program
    , statements :: Vector Stmt
    -- | A map of label names to the index of their statement
    , labels :: Map String Int
    -- | The index of the current statement
    , programCounter :: Int
    }

-- | A ProgramState with no variable, statements, or labels, pointed at the 
-- first statement
emptyState :: ProgramState
emptyState = ProgramState M.empty V.empty M.empty 0

-- | The reason evaluation stopped
data EvalStop
    -- | The evaluation succeeded and there is nothing else to evaluate
    = EvalSuccess (Maybe Data)
    -- | The evaluation failed
    | EvalFailed
  deriving Show

-- | A lookup request
data Lookup 
    -- | Lookup a variable by name
    = Lookup String
    -- | The output varaible
    | Output
    -- | The input variable
    | Input
    -- | The output variable
    | Punch
  deriving Show

-- | The data types allowed in a snobol4 program
data Data
    -- | A string
    = StringData String
    -- | A pattern
    | PatternData Pattern
    -- | An integer
    | IntegerData Int
    -- | A real number
    | RealData Float
    -- | An array
    | ArrayData (Vector Data)
    -- | A table
    | TableData (Map Data Data)
    -- | Passing an expression by name
    | Name Expr
    -- | An unevaluated expression
    | Unevaluated Expr
  deriving Show

-- | A pattern
data Pattern
    -- | A pattern which records the matched value in the provided lookup on 
    -- success
    = AssignmentPattern Pattern Lookup
    -- | A pattern which records the matched value in the provided lookup 
    -- immediately after matching
    | ImmediateAssignmentPattern Pattern Lookup
    -- | A pattern to match a literal string
    | LiteralPattern String
    -- | An alternative between two pattern
    | AlternativePattern Pattern Pattern
    -- | A concatination of two patterns
    | ConcatPattern Pattern Pattern
    -- | A pattern which matches any string of N characters
    | LengthPattern Int
    -- | A pattern which matches anything
    | EverythingPattern
  deriving Show

-- | A program error INCOMPLETE
data ProgramError
    -- | The program terminated due to an error
    = ProgramError
    -- | The program ended by reaching the END statement
    | NormalTermination
  deriving Show
