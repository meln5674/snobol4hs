{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.Interpreter.Internal.Types where

import Prelude hiding (toInteger)

import Text.Read hiding (lift, String, step, get)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Shell

-- | Transformer stack which represents the interpreter
newtype Interpreter m a
    = Interpreter
    { runInterpreter
        :: ExceptT ProgramError (StateT ProgramState m) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans Interpreter where
    lift m = Interpreter $ lift $ lift $ m

-- | Transformer stack for when the interpreter is evaluating a statement
newtype Evaluator m a 
    = Evaluator
    { runEvaluator
        :: ExceptT ProgramError (ExceptT EvalStop (StateT ProgramState m)) a
    
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans Evaluator where
    lift m = Evaluator $ lift $ lift $ lift $ m

-- | A paused interpreter
data PausedInterpreter
    =
    -- | An interpreter that has been paused
      Paused ProgramState
    -- | An interpreter that has been terminated
    | Terminated ProgramError



data Match
    = NoMatch
    | Replace Int Int [(Lookup,Data)]

data ScanResult
    = NoScan
    | Scan Data [(Lookup,Data)] Int Int


-- | Lift an operation from non-evaluation stack into evaluation stack
liftEval :: InterpreterShell m => Interpreter m a -> Evaluator m a
liftEval = Evaluator . ExceptT . lift . runExceptT . runInterpreter

-- | Lift an evaluation stack result back into the non-evaluation stack
unliftEval :: InterpreterShell m => Evaluator m a -> Interpreter m (Either EvalStop a)
unliftEval (Evaluator m) = do
    x <- Interpreter $ lift $ runExceptT $ runExceptT $ m
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
               $ EvalFailed

-- | Mark the current evaluation as successful and prohibit additional evaluation
finishEvaluation :: InterpreterShell m => Maybe Data -> Evaluator m a
finishEvaluation = Evaluator
                 . lift 
                 . throwE
                 . EvalSuccess

-- | Get the state of the interpreter
getProgramState :: InterpreterShell m => Interpreter m ProgramState
getProgramState = getsProgramState id

-- | Set the state of the interpreter
putProgramState :: InterpreterShell m => ProgramState -> Interpreter m ()
putProgramState = modifyProgramState . const

-- | Apply an accessor function to the state of the interpreter
getsProgramState :: InterpreterShell m => (ProgramState -> a) -> Interpreter m a
getsProgramState = Interpreter . lift . gets

-- | Apply an update function to the state of the interpreter
modifyProgramState :: InterpreterShell m 
                   => (ProgramState 
                   -> ProgramState) 
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

-- | Set the variables known to the interpreter
putVariables vars = modifyProgramState $ \st -> st { variables = vars }

-- | Set the loaded program
putStatements stmts = modifyProgramState $ \st -> st { statements = stmts }

-- | Set the labels known to the interpreter
putLabels lbls = modifyProgramState $ \st -> st { labels = lbls }

-- | Set the program counter
putProgramCounter pc = modifyProgramState $ \st -> st { programCounter = pc }

-- | Apply a function to the variables known to the interpreter
modifyVariables f = modifyProgramState $
        \st -> st { variables = f $ variables st }

-- | Apply a function to the loaded program
modifyStatements f = modifyProgramState $
    \st -> st { statements = f $ statements st }

-- | Apply a function to the labels known to the interpreter
modifyLabels f = modifyProgramState $
    \st -> st { labels = f $ labels st }

-- | Apply a function to the program counter
modifyProgramCounter f = modifyProgramState $
    \st -> st { programCounter = f $ programCounter st }

-- Fetches the next statement to execute
fetch :: InterpreterShell m => Interpreter m Stmt
fetch = (V.!) <$> getStatements <*> getProgramCounter

-- Find the index of the statement with a label
labelLookup :: InterpreterShell m => String -> Interpreter m (Maybe Int)
labelLookup lbl = M.lookup lbl <$> getLabels

-- Retreive the value of a variable
varLookup :: InterpreterShell m => String -> Interpreter m (Maybe Data)
varLookup id = M.lookup id <$> getVariables

-- Write the value of a variable
varWrite :: InterpreterShell m => String -> Data -> Interpreter m ()
varWrite id val = modifyVariables (M.insert id val)

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
toString :: InterpreterShell m => Data -> Evaluator m Data
toString (StringData s) = return $ StringData s
toString (IntegerData i) = return $ StringData $ show i
toString (RealData r) = return $ StringData $ show r
toString (PatternData (LiteralPattern s)) = return $ StringData s
toString (PatternData (ConcatPattern a b)) = do
    StringData a' <- toString $ PatternData a
    StringData b' <- toString $ PatternData b
    return $ StringData $ a' ++ b'
toString _ = liftEval $ programError ProgramError

-- | Convert data to a pattern
-- Throws a ProgramError if this is not valid
toPattern :: InterpreterShell m => Data -> Evaluator m Pattern
toPattern (PatternData p) = return p
toPattern x = do
    StringData s <- toString x
    return $ LiteralPattern $ s

-- | Convert data to an integer
-- Fails the evaluation if this can be turned into a string, but not into an 
-- integer
-- Throws a ProgramError if this is not valid
toInteger :: InterpreterShell m => Data -> Evaluator m Data
toInteger (IntegerData i) = return $ IntegerData i
toInteger x = do
    StringData s <- toString x
    case readMaybe s of
        Just i -> return $ IntegerData i
        Nothing -> failEvaluation

-- | Convert data to a real
-- Fails the evaluation if this can be turned into a string, but not into an 
-- real
-- Throws a ProgramError if this is not valid
toReal :: InterpreterShell m => Data -> Evaluator m Data
toReal (RealData i) = return $ RealData i
toReal x = do
    StringData s <- toString x
    case readMaybe s of
        Just i -> return $ RealData i
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
        return (a',b)
    | isInteger a && isString b = do
        b' <- toInteger b
        return (a,b')
    
    | isString a && isReal b = do
        a' <- toReal a
        return (a',b)
    | isReal a && isString b = do
        b' <- toReal b
        return (a,b')
    
    | isInteger a && isReal b = do
        a' <- toReal a
        return (a',b)
    | isReal a && isInteger b = do
        b' <- toReal b
        return (a,b')
    
    | otherwise = liftEval $ programError ProgramError

-- | Take two arguments and cast the "higher" one on the scale of
-- String -> Int -> Real to match the "lower" one
lowerArgs :: InterpreterShell m => Data -> Data -> Evaluator m (Data, Data)
lowerArgs a b
    | isString a && isString b = return (a,b)
    | isInteger a && isInteger b = return (a,b)
    | isReal a && isReal b = return (a,b)
    
    | isString a && isInteger b = do
        b' <- toString b
        return (a,b')
    | isInteger a && isString b = do
        a' <- toString a
        return (a',b)
    
    | isString a && isReal b = do
        b' <- toString b
        return (a,b')
    | isReal a && isString b = do
        a' <- toString a
        return (a',b)
    
    | isInteger a && isReal b = do
        b' <- toInteger b
        return (a,b')
    | isReal a && isInteger b = do
        a' <- toInteger a
        return (a',b)
    
    | otherwise = liftEval $ programError ProgramError

assign :: InterpreterShell m => Lookup -> Data -> Evaluator m ()
assign (Lookup s) val = liftEval $ varWrite s val
assign Output val = do
    StringData str <- toString val
    lift $ output str
assign Punch val = do
    StringData str <- toString val
    lift $ punch str
