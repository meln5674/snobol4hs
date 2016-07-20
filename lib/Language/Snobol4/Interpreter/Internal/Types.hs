{-|
Module          : Language.Snobol4.Interpreter
Description     : Low level types and operations
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown


-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.Interpreter.Internal.Types where

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
import Control.Monad.Trans.State

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Shell



-- | The reason evaluation stopped
data EvalStop
    -- | The evaluation succeeded and there is nothing else to evaluate
    = EvalSuccess (Maybe Data)
    -- | The evaluation failed
    | EvalFailed
  deriving Show

-- | Address of a statement
newtype Address = Address { getAddress :: Snobol4Integer }
  deriving (Eq, Ord, Bounded, Enum, Num)

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

-- | Create a new array with upper and lower bounds with an initial value
newArray :: Snobol4Integer -> Snobol4Integer -> Data -> Snobol4Array
newArray minIx maxIx v
    = Snobol4Array 
    $ A.array (minIx,maxIx) 
    $ map (\x -> (x,v)) [minIx..maxIx]

-- | Create a new array from a list of pairs of indices and values
newArray' :: [(Snobol4Integer,Data)] -> Snobol4Array
newArray' xs = Snobol4Array $ A.array (minIx,maxIx) xs
  where
    minIx = fst $ head xs
    maxIx = fst $ last xs

-- | Get the value of an array at an index
readArray :: Snobol4Integer -> Snobol4Array -> Maybe Data
readArray ix (Snobol4Array arr)
    | minIx <= ix && ix <= maxIx = Just $ arr A.! ix
    | otherwise = Nothing
  where
    (minIx,maxIx) = A.bounds arr

-- | Se the value of an array at an index
writeArray :: Snobol4Integer -> Data -> Snobol4Array -> Snobol4Array
writeArray ix v (Snobol4Array arr) = Snobol4Array $ arr A.// [(ix,v)]

-- | An empty table
emptyTable :: Snobol4Table
emptyTable = Snobol4Table M.empty

-- | Get the value of a table
readTable :: Data -> Snobol4Table -> Maybe Data
readTable k (Snobol4Table tbl) = M.lookup k tbl

-- | Set the value of a table
writeTable :: Data -> Data -> Snobol4Table -> Snobol4Table
writeTable k v (Snobol4Table tbl) = Snobol4Table $ M.insert k v tbl
 
-- | State of the interpreter
data ProgramState m
    = ProgramState
    { 
    -- | A map of names to variables bound
      variables :: Map Snobol4String Data 
    -- | The statements in the current program
    , statements :: Vector Stmt
    -- | A map of label names to the index of their statement
    , labels :: Map Snobol4String Address
    -- | The index of the current statement
    , programCounter :: Address
    -- | The functions known to the interpreter
    , functions :: Map Snobol4String (Function m)
    -- | The call stack
    , callStack :: [CallStackNode]
    , arrays :: Map ArrayKey Snobol4Array
    , tables :: Map TableKey Snobol4Table
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

-- | Lift an evaluation stack result back into the non-evaluation stack
unliftEval :: InterpreterShell m => Evaluator m a -> Interpreter m (Either EvalStop a)
unliftEval (Evaluator m) = do
    x <- Interpreter $ lift $ runExceptT $ runExceptT m
    case x of
        Left stop -> return $ Left stop
        Right (Left err) -> programError err
        Right (Right val) -> return $ Right val

-- | Terminate the program with an error
programError :: InterpreterShell m => ProgramError -> Interpreter m a
programError = Interpreter . throwE

-- | Mark the current evaluation as failed and prohibit additional evaluation
failEvaluation :: InterpreterShell m => Evaluator m a
failEvaluation = Evaluator
               $ lift 
               $ throwE 
                 EvalFailed

-- | Mark the current evaluation as successful and prohibit additional evaluation
finishEvaluation :: InterpreterShell m => Maybe Data -> Evaluator m a
finishEvaluation = Evaluator
                 . lift 
                 . throwE
                 . EvalSuccess

-- | Get the state of the interpreter
getProgramState :: InterpreterShell m => Interpreter m (ProgramState m)
getProgramState = getsProgramState id

-- | Set the state of the interpreter
putProgramState :: InterpreterShell m => ProgramState m -> Interpreter m ()
putProgramState = modifyProgramState . const

-- | Apply an accessor function to the state of the interpreter
getsProgramState :: InterpreterShell m => (ProgramState m -> a) -> Interpreter m a
getsProgramState = Interpreter . lift . gets

-- | Apply an update function to the state of the interpreter
modifyProgramState :: InterpreterShell m 
                   => (ProgramState m
                   -> ProgramState m) 
                   -> Interpreter m ()
modifyProgramState = Interpreter . lift . modify

-- | Get the variables known to the interpreter
getVariables :: InterpreterShell m => Interpreter m (Map Snobol4String Data)
getVariables = getsProgramState variables

-- | Get the loaded program
getStatements :: InterpreterShell m => Interpreter m (Vector Stmt)
getStatements = getsProgramState statements

-- | Get the labels known to the interpreter
getLabels :: InterpreterShell m => Interpreter m (Map Snobol4String Address)
getLabels = getsProgramState labels

-- | Get the program counter from the interpreter
getProgramCounter :: InterpreterShell m => Interpreter m Address
getProgramCounter = getsProgramState programCounter

-- | Get the functions known to the interpreter
getFunctions :: InterpreterShell m => Interpreter m (Map Snobol4String (Function m))
getFunctions = getsProgramState functions

-- | Get the call stack
getCallStack :: InterpreterShell m => Interpreter m [CallStackNode]
getCallStack = getsProgramState callStack

-- | Get the arrays known to the interpreter
getArrays :: InterpreterShell m => Interpreter m (Map ArrayKey Snobol4Array)
getArrays = getsProgramState arrays

-- | Get the tables known to the interpreter
getTables :: InterpreterShell m => Interpreter m (Map TableKey Snobol4Table)
getTables = getsProgramState tables




-- | Set the variables known to the interpreter
putVariables :: InterpreterShell m => Map Snobol4String Data -> Interpreter m ()
putVariables vars = modifyProgramState $ \st -> st { variables = vars }

-- | Set the loaded program
putStatements :: InterpreterShell m => Vector Stmt -> Interpreter m ()
putStatements stmts = modifyProgramState $ \st -> st { statements = stmts }

-- | Set the labels known to the interpreter
putLabels :: InterpreterShell m => Map Snobol4String Address -> Interpreter m ()
putLabels lbls = modifyProgramState $ \st -> st { labels = lbls }

-- | Set the program counter
putProgramCounter :: InterpreterShell m => Address -> Interpreter m ()
putProgramCounter pc = modifyProgramState $ \st -> st { programCounter = pc }

-- | Set the call stack
putCallStack :: InterpreterShell m => [CallStackNode] -> Interpreter m ()
putCallStack stk = modifyProgramState $ \st -> st { callStack = stk }

-- | Set the arrays known to the interpreter
putArrays :: InterpreterShell m => Map ArrayKey Snobol4Array -> Interpreter m ()
putArrays arrs = modifyProgramState $ \st -> st { arrays = arrs }

-- | Set the tables known to the interpreter
putTables :: InterpreterShell m => Map TableKey Snobol4Table -> Interpreter m ()
putTables tbls = modifyProgramState $ \st -> st { tables = tbls }



-- | Apply a function to the variables known to the interpreter
modifyVariables :: InterpreterShell m => (Map Snobol4String Data -> Map Snobol4String Data) -> Interpreter m ()
modifyVariables f = modifyProgramState $
        \st -> st { variables = f $ variables st }

-- | Apply a function to the loaded program
modifyStatements :: InterpreterShell m => (Vector Stmt -> Vector Stmt) -> Interpreter m ()
modifyStatements f = modifyProgramState $
    \st -> st { statements = f $ statements st }

-- | Apply a function to the labels known to the interpreter
modifyLabels :: InterpreterShell m 
             => (Map Snobol4String Address -> Map Snobol4String Address)
             -> Interpreter m ()
modifyLabels f = modifyProgramState $
    \st -> st { labels = f $ labels st }

-- | Apply a function to the program counter
modifyProgramCounter :: InterpreterShell m => (Address -> Address) -> Interpreter m ()
modifyProgramCounter f = modifyProgramState $
    \st -> st { programCounter = f $ programCounter st }

-- | Apply a function to the call stack
modifyCallStack :: InterpreterShell m => ([CallStackNode] -> [CallStackNode]) -> Interpreter m ()
modifyCallStack f = modifyProgramState $
    \st -> st { callStack = f $ callStack st }

-- | Apply a function to the arrays known to the interpreter
modifyArrays :: InterpreterShell m 
             => (Map ArrayKey Snobol4Array -> Map ArrayKey Snobol4Array)
             -> Interpreter m ()
modifyArrays f = modifyProgramState $
    \st -> st { arrays = f $ arrays st }

-- | Apply a function to the tables known to the interpreter
modifyTables :: InterpreterShell m
             => (Map TableKey Snobol4Table -> Map TableKey Snobol4Table)
             -> Interpreter m ()
modifyTables f = modifyProgramState $
    \st -> st { tables = f $ tables st }

-- | Apply a function to the head of the call stack
modifyCallStackHead :: InterpreterShell m => (CallStackNode -> CallStackNode) -> Interpreter m ()
modifyCallStackHead f = modifyCallStack $ \(n:ns) -> f n:ns

-- | Push a node onto the call stack
pushCallStack :: InterpreterShell m => CallStackNode -> Interpreter m ()
pushCallStack n = modifyCallStack (n:)

-- | Pop a node off of the call stack and set the program counter accordingly
popCallStack :: InterpreterShell m => Interpreter m CallStackNode
popCallStack = do
    n <- head <$> getCallStack 
    modifyCallStack $ \(_:ns) -> ns
    putProgramCounter $ returnAddr n
    return n

-- | Fetch the next statement to execute
fetch :: InterpreterShell m => Interpreter m Stmt
fetch = (V.!) <$> getStatements <*> (getInteger . getAddress <$> getProgramCounter)



-- | Find the index of the statement with a label
labelLookup :: InterpreterShell m => Snobol4String -> Interpreter m (Maybe Address)
labelLookup lbl = M.lookup lbl <$> getLabels

-- | Retreive the value of a global variable
globalLookup :: InterpreterShell m => Snobol4String -> Interpreter m (Maybe Data)
globalLookup name = M.lookup name <$> getVariables

-- | Retreive the value of a local variable
localLookup :: InterpreterShell m => Snobol4String -> Interpreter m (Maybe Data)
localLookup name = do
    stk <- getCallStack
    case stk of
        [] -> return Nothing
        (n:_) -> return $ M.lookup name $ locals n

-- | Flag for variables as local or global
data VarType = LocalVar | GlobalVar

-- | Retreive the value of a variable, first checking locals, then globals
varLookup :: InterpreterShell m => Snobol4String -> Interpreter m (Maybe (VarType,Data))
varLookup name = do
    localResult <- localLookup name
    case localResult of
        Just localVal -> return $ Just (LocalVar, localVal)
        Nothing -> do
            globalResult <- globalLookup name
            case globalResult of
                Just globalVal -> return $ Just (GlobalVar, globalVal)
                Nothing -> return Nothing

-- | Retreive the value of a variable, first checking locals, then globals,
-- then discard the flag stating which it is
varLookup' :: InterpreterShell m => Snobol4String -> Interpreter m (Maybe Data)
varLookup' name = varLookup name >>= \case
    Nothing -> return Nothing
    Just (_,val) -> return $ Just val    
    

-- | Write the value of a global variable
globalWrite :: InterpreterShell m => Snobol4String -> Data -> Interpreter m ()
globalWrite name = modifyVariables . M.insert name

-- | Write the value of a local variable
localWrite :: InterpreterShell m => Snobol4String -> Data -> Interpreter m ()
localWrite name = modifyVariables . M.insert name

-- | Write the value of a variable, first checking if there are any locals with
-- that name, then writing as a global if there isn't
varWrite :: InterpreterShell m => Snobol4String -> Data -> Interpreter m ()
varWrite name val = do
    result <- varLookup name
    case result of
        Just (LocalVar,_) -> localWrite name val
        Just (GlobalVar,_) -> globalWrite name val
        Nothing -> globalWrite name val

-- | Look up a function by name
funcLookup :: InterpreterShell m => Snobol4String -> Interpreter m (Maybe (Function m))
funcLookup name = M.lookup name <$> getFunctions

-- | Allocate a new array with an upper and lower bound each set to an intital value
arraysNew :: InterpreterShell m => Snobol4Integer -> Snobol4Integer -> Data -> Interpreter m ArrayKey
arraysNew minIx maxIx v = do
    newKey <- (succ . fst . M.findMax) `liftM` getArrays
    modifyArrays $ M.insert newKey $ newArray minIx maxIx v
    return newKey

-- | Allocate a new array with the provided dimensions and initial value
arraysNew'' :: InterpreterShell m 
           => [(Snobol4Integer,Snobol4Integer)]
           -> Data
           -> Interpreter m Data
arraysNew'' [] val = return val
arraysNew'' ((minIx,maxIx):ds) val = do
    newKey <- (succ . fst . M.findMax) `liftM` getArrays
    xs <- forM [minIx..maxIx] $ \ix -> do
        v <- arraysNew'' ds val
        return (ix,v)
    modifyArrays $ M.insert newKey $ newArray' xs
    return $ ArrayData newKey

-- | Lookup an array
arraysLookup :: InterpreterShell m => ArrayKey -> Interpreter m (Maybe Snobol4Array)
arraysLookup k = M.lookup k <$> getArrays

-- | Apply a function to an array
arraysUpdate :: InterpreterShell m => (Snobol4Array -> Snobol4Array) -> ArrayKey -> Interpreter m ()
arraysUpdate f k = modifyArrays $ M.adjust f k

-- | Get the value of an array with the given index
arraysRead :: InterpreterShell m => Snobol4Integer -> ArrayKey -> Interpreter m (Maybe Data)
arraysRead ix k = arraysLookup k >>= \x -> return $ x >>= readArray ix

-- | Set the value of an array with the given index
arraysWrite :: InterpreterShell m => Snobol4Integer -> Data -> ArrayKey -> Interpreter m ()
arraysWrite ix v = arraysUpdate $ writeArray ix v

-- | Allocate a new table
tablesNew :: InterpreterShell m => Interpreter m TableKey
tablesNew = do
    newKey <- (succ . fst . M.findMax) `liftM` getTables
    modifyTables $ M.insert newKey emptyTable
    return newKey

-- | Lookup a table
tablesLookup :: InterpreterShell m => TableKey -> Interpreter m (Maybe Snobol4Table)
tablesLookup k = M.lookup k <$> getTables

-- | Apply a function to a table
tablesUpdate :: InterpreterShell m => (Snobol4Table -> Snobol4Table) -> TableKey -> Interpreter m ()
tablesUpdate f k = modifyTables $ M.adjust f k

-- | Get the value of a table with the given key
tablesRead :: InterpreterShell m => Data -> TableKey -> Interpreter m (Maybe Data)
tablesRead k1 k2 = tablesLookup k2 >>= \x -> return $ x >>= readTable k1

-- | Set the value of a table with the given key
tablesWrite :: InterpreterShell m => Data -> Data -> TableKey -> Interpreter m ()
tablesWrite k v = tablesUpdate $ writeTable k v



-- | Push a node onto the call stack for calling a function
pushFuncNode :: InterpreterShell m => Function m -> Interpreter m ()
pushFuncNode f = do
    pc <- getProgramCounter
    pushCallStack
        Node 
        { callName = funcName f
        , locals = M.fromList $ map (\x -> (x,StringData  nullString))
                              $ funcName f : localNames f ++ formalArgs f
        , returnAddr = pc
        }


-- | Check if a value can be turned into a string
isStringable :: Data -> Bool
isStringable (StringData _) = True
isStringable (IntegerData _) = True
isStringable (RealData _) = True
isStringable (PatternData (LiteralPattern _)) = True
isStringable (PatternData (ConcatPattern a b)) 
    = isStringable (PatternData a) && isStringable (PatternData b)
isStringable _ = False

-- | Check if data is a string
isString :: Data -> Bool
isString (StringData _) = True
isString _ = False

-- | Check if data is an integer
isInteger :: Data -> Bool
isInteger (IntegerData _) = True
isInteger _ = False

-- | Check if data is a real
isReal :: Data -> Bool
isReal (RealData _) = True
isReal _ = False

-- | Convert data to a string
-- Throws a ProgramError if this is not valid
toString :: InterpreterShell m => Data -> Evaluator m Snobol4String
toString (StringData s) = return s
toString (IntegerData i) = return $ mkString i
toString (RealData r) = return $ mkString r
toString (PatternData (LiteralPattern s)) = return s
toString (PatternData (ConcatPattern a b)) = do
    a' <- toString $ PatternData a
    b' <- toString $ PatternData b
    return $ a' <> b'
toString _ = liftEval $ programError IllegalDataType

-- | Convert data to a pattern
-- Throws a ProgramError if this is not valid
toPattern :: InterpreterShell m => Data -> Evaluator m Pattern
toPattern (PatternData p) = return p
toPattern x = LiteralPattern <$> toString x

-- | Convert data to an integer
-- Fails the evaluation if this can be turned into a string, but not into an 
-- integer
-- Throws a ProgramError if this is not valid
toInteger :: InterpreterShell m => Data -> Evaluator m Snobol4Integer
toInteger (IntegerData i) = return i
toInteger x = do
    s <- toString x
    case snobol4Read s of
        Just i -> return i
        Nothing -> failEvaluation

-- | Convert data to a real
-- Fails the evaluation if this can be turned into a string, but not into an 
-- real
-- Throws a ProgramError if this is not valid
toReal :: InterpreterShell m => Data -> Evaluator m Snobol4Real
toReal (RealData r) = return r
toReal x = do
    s <- toString x
    case snobol4Read s of
        Just r -> return r
        Nothing -> failEvaluation
    
-- | Take two arguments and cast the "lower" one on the scale of
-- String -> Int -> Real to match the "higher" one
raiseArgs :: InterpreterShell m => Data -> Data -> Evaluator m (Data, Data)
raiseArgs a b
    | isString a && isString b = return (a,b)
    | isInteger a && isInteger b = return (a,b)
    | isReal a && isReal b = return (a,b)
    
    | isString a && isInteger b = do
        a' <- toInteger a
        return (IntegerData a',b)
    | isInteger a && isString b = do
        b' <- toInteger b
        return (a,IntegerData b')
    
    | isString a && isReal b = do
        a' <- toReal a
        return (RealData a',b)
    | isReal a && isString b = do
        b' <- toReal b
        return (a,RealData b')
    
    | isInteger a && isReal b = do
        a' <- toReal a
        return (RealData a',b)
    | isReal a && isInteger b = do
        b' <- toReal b
        return (a,RealData b')
    
    | otherwise = liftEval $ programError IllegalDataType

-- | Take two arguments and cast the "higher" one on the scale of
-- String -> Int -> Real to match the "lower" one
lowerArgs :: InterpreterShell m => Data -> Data -> Evaluator m (Data, Data)
lowerArgs a b
    | isString a && isString b = return (a,b)
    | isInteger a && isInteger b = return (a,b)
    | isReal a && isReal b = return (a,b)
    
    | isString a && isInteger b = do
        b' <- toString b
        return (a,StringData b')
    | isInteger a && isString b = do
        a' <- toString a
        return (StringData a',b)
    
    | isString a && isReal b = do
        b' <- toString b
        return (a,StringData b')
    | isReal a && isString b = do
        a' <- toString a
        return (StringData a',b)
    
    | isInteger a && isReal b = do
        b' <- toInteger b
        return (a,IntegerData b')
    | isReal a && isInteger b = do
        a' <- toInteger a
        return (IntegerData a',b)
    
    | otherwise = liftEval $ programError IllegalDataType

-- | Utility function, safe lookup for arrays
arrayGet :: A.Ix i => Array i e -> i -> Maybe e
arr `arrayGet` ix
    | inBounds = Just $ arr A.! ix
    | otherwise = Nothing
  where
    inBounds = minB <= ix && ix < maxB
    (minB,maxB) = A.bounds arr 
    

-- | Assign a value using a lookup
assign :: InterpreterShell m => Lookup -> Data -> Evaluator m ()
assign (LookupId s) val = liftEval $ varWrite s val
assign (LookupAggregate name args) val = do
    let loop (ArrayData k) [IntegerData ix] = arraysWrite ix val k
        loop (ArrayData k) (IntegerData ix:as) = do
            readResult <- arraysRead ix k
            case readResult of
                Just d -> loop d as
                Nothing -> programError ErroneousArrayOrTableReference
        loop (ArrayData _) _ = programError ErroneousArrayOrTableReference
        loop (TableData k) [a] = tablesWrite a val k
        loop (TableData k) (a:as) = do
            readResult <- tablesRead a k
            case readResult of
                Just d -> loop d as
                Nothing -> programError ErroneousArrayOrTableReference
        loop (TableData _) _ = programError ErroneousArrayOrTableReference
        loop _ _ = programError ErroneousArrayOrTableReference
    base <- liftEval $ varLookup name
    liftEval $ case base of
        Just (_,baseVal) -> loop baseVal args
        Nothing -> programError ErroneousArrayOrTableReference
assign Output val = toString val >>= lift . output . show
assign Punch val = toString val >>= lift . punch . show
assign _ _ = liftEval $ programError VariableNotPresentWhereRequired

-- | Execute a lookup
execLookup :: InterpreterShell m => Lookup -> Interpreter m (Maybe Data) 
execLookup Input = (Just . StringData . mkString) <$> lift input 
execLookup Output = (Just . StringData . mkString) <$> lift lastOutput 
execLookup Punch = (Just . StringData . mkString) <$> lift lastPunch 
execLookup (LookupLiteral x) = return $ Just x 
execLookup (LookupId i) = varLookup' i
execLookup (LookupAggregate name args) = do
    base <- varLookup' name
    case base of
        Nothing -> return Nothing
        Just val -> do
            let loop (ArrayData k) (IntegerData i:as) = do
                    readResult <- arraysRead i k
                    case readResult of
                        Nothing -> return Nothing
                        Just d -> loop d as
                loop (ArrayData _) _ = return Nothing
                loop (TableData k) (a:as) = do
                    readResult <- tablesRead a k
                    case readResult of
                        Nothing -> return Nothing
                        Just d -> loop d as
                loop x [] = return $ Just x
                loop _ _ = return Nothing
            loop val args
