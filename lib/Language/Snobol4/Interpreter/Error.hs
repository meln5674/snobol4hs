module Language.Snobol4.Interpreter.Error where

-- | A program error
data ProgramError
    = 
    -- | The program ended by reaching the END statement
      NormalTermination
    -- | e.g. X = X + 'A'
    | IllegalDataType
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
