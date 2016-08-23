{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Arrays
Description     : Maintaining variables
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE LambdaCase #-}
module Language.Snobol4.Interpreter.Internal.StateMachine.Variables where

import qualified Data.Map as M
import qualified Data.Vector as V

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
import Language.Snobol4.Interpreter.Internal.StateMachine.Run

-- | Empty collection of variables
noVariables :: Variables
noVariables = Variables V.empty M.empty

-- | Get the variables known to the interpreter
getVariables :: InterpreterShell m => InterpreterGeneric program instruction m Variables
getVariables = getsProgramState variables

-- | Set the variables known to the interpreter
putVariables :: InterpreterShell m => Variables -> InterpreterGeneric program instruction m ()
putVariables vars = modifyProgramState $ \st -> st { variables = vars }

-- | Apply a function to the variables known to the interpreter
modifyVariables :: InterpreterShell m => (Variables -> Variables) -> InterpreterGeneric program instruction m ()
modifyVariables f = modifyProgramState $
        \st -> st { variables = f $ variables st }

modifyStaticVars :: InterpreterShell m => (StaticVars -> StaticVars) -> InterpreterGeneric program instruction m ()
modifyStaticVars f = modifyVariables $ \v -> v{ staticVars = f $ staticVars v }

modifyDynamicVars :: InterpreterShell m => (DynamicVars -> DynamicVars) -> InterpreterGeneric program instruction m ()
modifyDynamicVars f = modifyVariables $ \v -> v{ dynamicVars = f $ dynamicVars v }

-- | Retreive the value of a global variable
globalLookup :: InterpreterShell m => Snobol4String -> InterpreterGeneric program instruction m (Maybe Data)
globalLookup name = do
    vars <- getVariables
    return $ liftM ((staticVars vars) V.!) $ (M.lookup name $ dynamicVars vars)

-- | Retreive the value of a local variable
localLookup :: InterpreterShell m => Snobol4String -> InterpreterGeneric program instruction m (Maybe Data)
localLookup name = do
    stk <- getCallStack
    case stk of
        [] -> return Nothing
        (n:_) -> return $ M.lookup name $ locals n

-- | Flag for variables as local or global
data VarType = LocalVar | GlobalVar

-- | Retreive the value of a variable, first checking locals, then globals
varLookup :: InterpreterShell m => Snobol4String -> InterpreterGeneric program instruction m (Maybe (VarType,Data))
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
varLookup' :: InterpreterShell m => Snobol4String -> InterpreterGeneric program instruction m (Maybe Data)
varLookup' name = varLookup name >>= \case
    Nothing -> return Nothing
    Just (_,val) -> return $ Just val    
    

-- | Write the value of a global variable
globalWrite :: InterpreterShell m => Snobol4String -> Data -> InterpreterGeneric program instruction m ()
globalWrite name val = do 
    vars <- getVariables
    case M.lookup name $ dynamicVars vars of
        Nothing -> do
            modifyStaticVars $ flip V.snoc val
            modifyDynamicVars $ M.insert name $ V.length $ staticVars vars
        Just ix -> do
            modifyStaticVars $ (V.// [(ix,val)])

-- | Write the value of a local variable
localWrite :: InterpreterShell m => Snobol4String -> Data -> InterpreterGeneric program instruction m ()
localWrite = writeCallStackLocal

-- | Write the value of a variable, first checking if there are any locals with
-- that name, then writing as a global if there isn't
varWrite :: InterpreterShell m => Snobol4String -> Data -> InterpreterGeneric program instruction m ()
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
clearVar :: InterpreterShell m => Snobol4String -> InterpreterGeneric program instruction m ()
clearVar n = do
    result <- varLookup n
    case result of
        Just (_,d) -> do
            decRef d
            modifyDynamicVars $ M.delete n -- TODO: Cleanup
        Nothing -> return ()

-- | Erase all variables
wipeVariables :: InterpreterShell m => InterpreterGeneric program instruction m ()
wipeVariables = putVariables $ noVariables

-- | Get the names of the natural variables
naturalVarNames :: InterpreterShell m => InterpreterGeneric program instruction m [Snobol4String]
naturalVarNames = liftM (M.keys . dynamicVars) getVariables

-- | Take a value, if it is a reference to data maintained by the GC, increment
-- the number of references to it
incRef :: InterpreterShell m => Data -> InterpreterGeneric program instruction m ()
incRef (PatternData k) = patternsIncRef k
incRef (ArrayData k) = arraysIncRef k
incRef (TableData k) = tablesIncRef k
incRef _ = return ()

-- | Take a value, if it is a reference to data maintained by the GC, decrement
-- the number of references to it
decRef :: InterpreterShell m => Data -> InterpreterGeneric program instruction m ()
decRef (PatternData k) = patternsDecRef k
decRef (ArrayData k) = arraysDecRef k
decRef (TableData k) = tablesDecRef k
decRef _ = return ()

-- | Assign a value to the location pointed to by a lookup
assign :: InterpreterShell m => Lookup -> Data -> EvaluatorGeneric program instruction m ()
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
assign Output val = toString val >>= lift . output . unmkString
assign Punch val = toString val >>= lift . punch . unmkString
assign _ _ = liftEval $ programError VariableNotPresentWhereRequired

-- | Get the data pointed to by a lookup
execLookup :: InterpreterShell m => Lookup -> InterpreterGeneric program instruction m (Maybe Data) 
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

