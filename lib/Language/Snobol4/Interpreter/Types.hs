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

import Data.Array (Array)
import qualified Data.Array as A

import Language.Snobol4.Syntax.AST



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
    = LookupId String
    -- | Lookup an element in an array by index or a table by key
    | LookupAggregate String [Data]
    -- | The output varaible
    | Output
    -- | The input variable
    | Input
    -- | The output variable
    | Punch
    | LookupLiteral Data
  deriving (Show, Eq, Ord)


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
    | ArrayData (Array Int Data)
    -- | A table
    | TableData (Map Data Data)
    -- | Passing an expression by name
    | Name Expr
    -- | An unevaluated expression
    | Unevaluated Expr
  deriving (Show, Eq, Ord)

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
    -- | An alternative between two ore more patterns
    | AlternativePattern Pattern Pattern
    -- | A concatination of two or more patterns
    | ConcatPattern Pattern Pattern
    -- | A pattern which matches any string of N characters
    | LengthPattern Int
    -- | A pattern which matches anything
    | EverythingPattern
    -- | A pattern which contains an unevaluated expression
    | UnevaluatedExprPattern Expr
    -- | A pattern which assigns the cursor position to a variable and matches
    -- the null string
    | HeadPattern Lookup
    -- | A pattern which matches the longest string containing only certain
    -- characters
    | SpanPattern [Char]
    -- | A pattern which matches the longest string not containing certain
    -- characters
    | BreakPattern [Char]
    -- | A pattern which matches one character of a list of characters
    | AnyPattern [Char]
    -- | A pattern which matches one character not in a list of characters
    | NotAnyPattern [Char]
    -- | A pattern which succeeds if the cursor is before the given column
    -- measured from the start
    | TabPattern Int
    -- | A pattern which succeeds if the cursor is after the given column
    -- measured from the end
    | RTabPattern Int
    -- | A pattern which succeeds if the cursor is at the given column measured
    -- from the start
    | PosPattern Int
    -- | A pattern which succeeds if the cursor is at the given column measured
    -- from the end
    | RPosPattern Int
    -- | A pattern which always fails
    | FailPattern
    -- | A pattern which succeeds the first time, but fails any time after
    | FencePattern
    -- | A pattern which aborts the scanner
    | AbortPattern
    | ArbPattern
    | ArbNoPattern Pattern
  deriving (Show, Eq, Ord)

-- | A program error INCOMPLETE
data ProgramError
    = 
    -- | The program ended by reaching the END statement
      NormalTermination
    | IllegalDataType
    | ErrorInArithmeticOperation
    | ErroneousArrayOrTableReference
    | NullStringInIllegalContext
    | UndefinedFunctionOrOperation
    | ErroneousPrototype
    | UnknownKeyword
    | VariableNotPresentWhereRequired
    | EntryPointOfFunctionNotLabel
    | IllegalArgumentToPrimitiveFunction
    | ReadingError
    | IllegalIOUnit
    | LimitOnDefinedDataTypesExceeded
    | NegativeNumberInIllegalContext
    | StringOverflow
    | OverflowDuringPatternMatching
    | ErrorInSnobol4System
    | ReturnFromZeroLevel
    | FailureDuringGotoEvaluation
    | InsufficientStorageToContinue
    | StackOverflow
    | LimitOnStatementExecutionExceeded
    | ObjectExceedsSizeLimit
    | UndefinedOrErroneousGoto
    | IncorrectNumberOfArguments
    | LimitOnCompilationErrorsExceeded
    | ErroneousEndStatement
    | ExecutionOfStatementWithACompilationError
  deriving Show
