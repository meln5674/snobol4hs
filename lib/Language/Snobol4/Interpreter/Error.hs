{-|
Module          : Language.Snobol4.Interpreter.Error
Description     : Interpreter Errors
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Error where

-- | The result of running a program
data ProgramResult
    = 
    -- | The program ended by reaching the END statement
      NormalTermination
    -- | The program is not yet complete
    | ProgramIncomplete
    -- | The program terminated with an error, at the given line
    | ErrorTermination ProgramError Int
  deriving (Show, Eq)

-- | A program error
data ProgramError
    = 
    -- | e.g. X = X + 'A'
      IllegalDataType
    -- | Division by zero, numeric overflow, etc
    | ErrorInArithmeticOperation
    -- | Reference with a bad key or index
    | ErroneousArrayOrTableReference
    -- | Null string found where it is not allowed
    | NullStringInIllegalContext
    -- | Called a function which has not be defined
    | UndefinedFunctionOrOperation
    -- | Can't parse a prototype
    | ErroneousPrototype
    -- | Used a bad keyword
    | UnknownKeyword
    -- | e.g. Assignment to a literal
    | VariableNotPresentWhereRequired
    -- |
    | EntryPointOfFunctionNotLabel
    -- |
    | IllegalArgumentToPrimitiveFunction
    -- |
    | ReadingError
    -- |
    | IllegalIOUnit
    -- |
    | LimitOnDefinedDataTypesExceeded
    -- |
    | NegativeNumberInIllegalContext
    -- |
    | StringOverflow
    -- |
    | OverflowDuringPatternMatching
    -- |
    | ErrorInSnobol4System
    -- |
    | ReturnFromZeroLevel
    -- |
    | FailureDuringGotoEvaluation
    -- |
    | InsufficientStorageToContinue
    -- |
    | StackOverflow
    -- |
    | LimitOnStatementExecutionExceeded
    -- |
    | ObjectExceedsSizeLimit
    -- |
    | UndefinedOrErroneousGoto
    -- |
    | IncorrectNumberOfArguments
    -- |
    | LimitOnCompilationErrorsExceeded
    -- |
    | ErroneousEndStatement
    -- |
    | ExecutionOfStatementWithACompilationError
  deriving (Show, Eq)
