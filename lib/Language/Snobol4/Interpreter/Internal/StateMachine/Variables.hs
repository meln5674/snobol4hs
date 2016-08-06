{-# LANGUAGE LambdaCase #-}
module Language.Snobol4.Interpreter.Internal.StateMachine.Variables where

import qualified Data.Map as M

import Control.Monad
import Control.Monad.Trans

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Convert
import Language.Snobol4.Interpreter.Internal.StateMachine.Error
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC
import Language.Snobol4.Interpreter.Internal.StateMachine.Arrays
import Language.Snobol4.Interpreter.Internal.StateMachine.Tables
import Language.Snobol4.Interpreter.Internal.StateMachine.CallStack
import Language.Snobol4.Interpreter.Internal.StateMachine.Patterns

noVariables :: Variables
noVariables = M.empty

-- | Get the variables known to the interpreter
getVariables :: InterpreterShell m => Interpreter m Variables
getVariables = getsProgramState variables

-- | Set the variables known to the interpreter
putVariables :: InterpreterShell m => Variables -> Interpreter m ()
putVariables vars = modifyProgramState $ \st -> st { variables = vars }

-- | Apply a function to the variables known to the interpreter
modifyVariables :: InterpreterShell m => (Variables -> Variables) -> Interpreter m ()
modifyVariables f = modifyProgramState $
        \st -> st { variables = f $ variables st }

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
    val' <- case val of
        (TempPatternData p) -> PatternData <$> patternsNew p
        x -> return x
    result <- varLookup name
    case result of
        Just (LocalVar,old) -> decRef old >> localWrite name val'
        Just (GlobalVar,old) -> decRef old >> globalWrite name val'
        Nothing -> globalWrite name val'
    incRef val'

-- | Delete a variable
clearVar :: InterpreterShell m => Snobol4String -> Interpreter m ()
clearVar n = do
    result <- varLookup n
    case result of
        Just (_,d) -> do
            decRef d
            modifyVariables $ M.delete n
        Nothing -> return ()

wipeVariables :: InterpreterShell m => Interpreter m ()
wipeVariables = putVariables $ M.empty

naturalVarNames :: InterpreterShell m => Interpreter m [Snobol4String]
naturalVarNames = liftM M.keys getVariables


incRef :: InterpreterShell m => Data -> Interpreter m ()
incRef (PatternData k) = patternsIncRef k
incRef (ArrayData k) = arraysIncRef k
incRef (TableData k) = tablesIncRef k

decRef :: InterpreterShell m => Data -> Interpreter m ()
decRef (PatternData k) = patternsDecRef k
decRef (ArrayData k) = arraysDecRef k
decRef (TableData k) = tablesDecRef k
decRef _ = return ()


    
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

