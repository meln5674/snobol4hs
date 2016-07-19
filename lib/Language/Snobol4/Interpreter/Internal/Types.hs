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

import Text.Read hiding (lift, String, step, get)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Array (Array)
import qualified Data.Array as A

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Shell

-- | A node of the call stack
data CallStackNode
    = Node
    { 
    -- | Local variables
      locals :: Map String Data
    -- | The index of the statement that called this function
    , returnAddr :: Int
    -- | The name of the function called
    , callName :: String
    }

-- | Information for calling a function
data Function m
    -- A user defined function
    = UserFunction
    { 
    -- | Name of the function
      funcName :: String
    -- | The names of the formal arguments of the function
    , formalArgs :: [String]
    -- | The names of the local variables of the function
    , localNames :: [String]
    -- | Index of the statement to start this function
    , entryPoint :: Int
    }
    | PrimitiveFunction
    {
    -- | Name of the function
       funcName :: String
    -- | The primitive function to call
    ,  funcPrim :: [Data] -> Evaluator m (Maybe Data)
    }

 
-- | State of the interpreter
data ProgramState m
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
    -- | The functions known to the interpreter
    , functions :: Map String (Function m)
    -- | The call stack
    , callStack :: [CallStackNode]
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
    | Scan Data [(Lookup,Data)] Int Int
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
getVariables :: InterpreterShell m => Interpreter m (Map String Data)
getVariables = getsProgramState variables

-- | Get the loaded program
getStatements :: InterpreterShell m => Interpreter m (Vector Stmt)
getStatements = getsProgramState statements

-- | Get the labels known to the interpreter
getLabels :: InterpreterShell m => Interpreter m (Map String Int)
getLabels = getsProgramState labels

-- | Get the program counter from the interpreter
getProgramCounter :: InterpreterShell m => Interpreter m Int
getProgramCounter = getsProgramState programCounter

-- | Get the functions known to the interpreter
getFunctions :: InterpreterShell m => Interpreter m (Map String (Function m))
getFunctions = getsProgramState functions

-- | Get the call stack
getCallStack :: InterpreterShell m => Interpreter m [CallStackNode]
getCallStack = getsProgramState callStack


-- | Set the variables known to the interpreter
putVariables :: InterpreterShell m => Map String Data -> Interpreter m ()
putVariables vars = modifyProgramState $ \st -> st { variables = vars }

-- | Set the loaded program
putStatements :: InterpreterShell m => Vector Stmt -> Interpreter m ()
putStatements stmts = modifyProgramState $ \st -> st { statements = stmts }

-- | Set the labels known to the interpreter
putLabels :: InterpreterShell m => Map String Int -> Interpreter m ()
putLabels lbls = modifyProgramState $ \st -> st { labels = lbls }

-- | Set the program counter
putProgramCounter :: InterpreterShell m => Int -> Interpreter m ()
putProgramCounter pc = modifyProgramState $ \st -> st { programCounter = pc }

-- | Set the call stack
putCallStack :: InterpreterShell m => [CallStackNode] -> Interpreter m ()
putCallStack stk = modifyProgramState $ \st -> st { callStack = stk }


-- | Apply a function to the variables known to the interpreter
modifyVariables :: InterpreterShell m => (Map String Data -> Map String Data) -> Interpreter m ()
modifyVariables f = modifyProgramState $
        \st -> st { variables = f $ variables st }

-- | Apply a function to the loaded program
modifyStatements :: InterpreterShell m => (Vector Stmt -> Vector Stmt) -> Interpreter m ()
modifyStatements f = modifyProgramState $
    \st -> st { statements = f $ statements st }

-- | Apply a function to the labels known to the interpreter
modifyLabels :: InterpreterShell m => (Map String Int -> Map String Int) -> Interpreter m ()
modifyLabels f = modifyProgramState $
    \st -> st { labels = f $ labels st }

-- | Apply a function to the program counter
modifyProgramCounter :: InterpreterShell m => (Int -> Int) -> Interpreter m ()
modifyProgramCounter f = modifyProgramState $
    \st -> st { programCounter = f $ programCounter st }

-- | Apply a function to the call stack
modifyCallStack :: InterpreterShell m => ([CallStackNode] -> [CallStackNode]) -> Interpreter m ()
modifyCallStack f = modifyProgramState $
    \st -> st { callStack = f $ callStack st }

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
fetch = (V.!) <$> getStatements <*> getProgramCounter

-- | Find the index of the statement with a label
labelLookup :: InterpreterShell m => String -> Interpreter m (Maybe Int)
labelLookup lbl = M.lookup lbl <$> getLabels

-- | Retreive the value of a global variable
globalLookup :: InterpreterShell m => String -> Interpreter m (Maybe Data)
globalLookup name = M.lookup name <$> getVariables

-- | Retreive the value of a local variable
localLookup :: InterpreterShell m => String -> Interpreter m (Maybe Data)
localLookup name = do
    stk <- getCallStack
    case stk of
        [] -> return Nothing
        (n:_) -> return $ M.lookup name $ locals n

-- | Flag for variables as local or global
data VarType = LocalVar | GlobalVar

-- | Retreive the value of a variable, first checking locals, then globals
varLookup :: InterpreterShell m => String -> Interpreter m (Maybe (VarType,Data))
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
varLookup' :: InterpreterShell m => String -> Interpreter m (Maybe Data)
varLookup' name = varLookup name >>= \case
    Nothing -> return Nothing
    Just (_,val) -> return $ Just val    
    

-- | Write the value of a global variable
globalWrite :: InterpreterShell m => String -> Data -> Interpreter m ()
globalWrite name = modifyVariables . M.insert name

-- | Write the value of a local variable
localWrite :: InterpreterShell m => String -> Data -> Interpreter m ()
localWrite name = modifyVariables . M.insert name

-- | Write the value of a variable, first checking if there are any locals with
-- that name, then writing as a global if there isn't
varWrite :: InterpreterShell m => String -> Data -> Interpreter m ()
varWrite name val = do
    result <- varLookup name
    case result of
        Just (LocalVar,_) -> localWrite name val
        Just (GlobalVar,_) -> globalWrite name val
        Nothing -> globalWrite name val

-- | Look up a function by name
funcLookup :: InterpreterShell m => String -> Interpreter m (Maybe (Function m))
funcLookup name = M.lookup name <$> getFunctions

-- | Push a node onto the call stack for calling a function
pushFuncNode :: InterpreterShell m => Function m -> Interpreter m ()
pushFuncNode f = do
    pc <- getProgramCounter
    pushCallStack
        Node 
        { callName = funcName f
        , locals = M.fromList $ map (\x -> (x,StringData "")) 
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

{-
isIntegerable :: Data -> Bool
isIntegerable (StringData _) = True
isIntegerable (IntegerData _) = True
isIntegerable (RealData _) = True
isIntegerable _ = False


isRealable :: Data -> Bool
isRealable (StringData _) = True
isRealable (IntegerData _) = True
isRealable (RealData _) = True
isRealable _ = False

isPatternable :: Data -> Bool
isPatternable (PatternData _) = True
isPatternable x = isStringable x
-}

-- | Convert data to a string
-- Throws a ProgramError if this is not valid
toString :: InterpreterShell m => Data -> Evaluator m String
toString (StringData s) = return s
toString (IntegerData i) = return $ show i
toString (RealData r) = return $ show r
toString (PatternData (LiteralPattern s)) = return s
toString (PatternData (ConcatPattern a b)) = do
    a' <- toString $ PatternData a
    b' <- toString $ PatternData b
    return $ a' ++ b'
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
toInteger :: InterpreterShell m => Data -> Evaluator m Int
toInteger (IntegerData i) = return i
toInteger x = do
    s <- toString x
    case readMaybe s of
        Just i -> return i
        Nothing -> failEvaluation

-- | Convert data to a real
-- Fails the evaluation if this can be turned into a string, but not into an 
-- real
-- Throws a ProgramError if this is not valid
toReal :: InterpreterShell m => Data -> Evaluator m Float
toReal (RealData r) = return r
toReal x = do
    s <- toString x
    case readMaybe s of
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
    let loop (ArrayData arr) [IntegerData i] = return $ ArrayData $ arr A.// [(i,val)]
        loop (ArrayData arr) (IntegerData i:as) = case arr `arrayGet` i of
            Just d -> do
                d' <- loop d as
                return $ ArrayData $ arr A.// [(i,d')]
            Nothing -> liftEval $ programError ErroneousArrayOrTableReference
        loop (ArrayData _) _ = liftEval $ programError ErroneousArrayOrTableReference
        loop (TableData tab) [a] = return $ TableData $ M.insert a val tab
        loop (TableData tab) (a:as) = case M.lookup a tab of
            Just d -> do
                d' <- loop d as
                return $ TableData $ M.insert a d' tab
            Nothing -> liftEval $ programError ErroneousArrayOrTableReference
        loop (TableData _) _ = liftEval $ programError ErroneousArrayOrTableReference
        loop _ _ = liftEval $ programError ErroneousArrayOrTableReference
    base <- liftEval $ varLookup name
    case base of
        Just (_,baseVal) -> do
            base' <- loop baseVal args
            liftEval $ varWrite name base'
        Nothing -> liftEval $ programError ErroneousArrayOrTableReference
assign Output val = toString val >>= lift . output
assign Punch val = toString val >>= lift . punch
assign _ _ = liftEval $ programError VariableNotPresentWhereRequired
