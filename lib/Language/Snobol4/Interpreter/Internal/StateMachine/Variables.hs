{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Arrays
Description     : Maintaining variables
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
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
--import Language.Snobol4.Interpreter.Internal.StateMachine.CallStack
import Language.Snobol4.Interpreter.Internal.StateMachine.Patterns
import Language.Snobol4.Interpreter.Internal.StateMachine.Keywords
import Language.Snobol4.Interpreter.Internal.StateMachine.UserData
--import Language.Snobol4.Interpreter.Internal.StateMachine.Run

-- | Empty collection of variables
noVariables :: Variables expr
noVariables = Variables V.empty M.empty

-- | Get the variables known to the interpreter
getVariables :: ( InterpreterShell m
--                {-, Snobol4Machine program-}
                ) 
             => InterpreterGeneric program m (Variables (ExprType m))
getVariables = getsProgramState variables

getDynamics :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
            => InterpreterGeneric program m DynamicVars
getDynamics = liftM dynamicVars getVariables

-- | Set the variables known to the interpreter
putVariables :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
             => (Variables (ExprType m)) -> InterpreterGeneric program m ()
putVariables vars = modifyProgramState $ \st -> st { variables = vars }

-- | Apply a function to the variables known to the interpreter
modifyVariables :: ( InterpreterShell m
--                   {-, Snobol4Machine program-}
                   ) 
                => ((Variables (ExprType m)) -> (Variables (ExprType m))) -> InterpreterGeneric program m ()
modifyVariables f = modifyProgramState $
        \st -> st { variables = f $ variables st }

modifyStaticVars :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
                 => ((StaticVars (ExprType m)) -> (StaticVars (ExprType m))) -> InterpreterGeneric program m ()
modifyStaticVars f = modifyVariables $ \v -> v{ staticVars = f $ staticVars v }

modifyDynamicVars :: ( InterpreterShell m
--                     {-, Snobol4Machine program-}
                     ) 
                  => (DynamicVars -> DynamicVars) -> InterpreterGeneric program m ()
modifyDynamicVars f = modifyVariables $ \v -> v{ dynamicVars = f $ dynamicVars v }

globalLookup :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
             => Int -> InterpreterGeneric program m (Data (ExprType m))
globalLookup ix = do
    vars <- getVariables
    return $ staticVars vars V.! ix

{-
localLookup :: ( InterpreterShell m{-, Snobol4Machine program-} ) => Int -> InterpreterGeneric program m (Data (ExprType m))
localLookup ix = do
    stk <- getCallStack
    case stk of
        [] -> programError $ ErrorInSnobol4System
        (n:_) -> return $ locals n V.! ix
-}

localLookup :: ( InterpreterShell m{-, Snobol4Machine program-}, LocalVariablesClass m ) 
            => Int 
            -> InterpreterGeneric program m (Data (ExprType m))
localLookup ix = do
    result <- lift $ lookupLocal ix
    case result of
        Nothing -> programError $ ErrorInSnobol4System
        Just x -> return x

-- | Dereference a variable reference
varDeref :: ( InterpreterShell m{-, Snobol4Machine program-}, LocalVariablesClass m ) 
         => VarType 
         -> InterpreterGeneric program m (Data (ExprType m))
varDeref (LocalVar ix) = localLookup ix
varDeref (GlobalVar ix) = globalLookup ix


{-
-- | Retreive the value of a local variable
localLookup :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
            => Snobol4String -> InterpreterGeneric program m (Maybe (Data (ExprType m)))
localLookup name = do
    stk <- getCallStack
    case stk of
        [] -> return Nothing
        (n:_) -> return $ M.lookup name $ locals n
-}

{-
-- | Retreive the value of a variable, first checking locals, then globals
varLookup :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
          => Snobol4String -> InterpreterGeneric program m (Maybe (VarType,(Data (ExprType m))))
varLookup name = do
    localResult <- localLookup name
    case localResult of
        Just localVal -> return $ Just (LocalVar, localVal)
        Nothing -> do
            globalResult <- globalLookup name
            case globalResult of
                Just globalVal -> return $ Just (GlobalVar, globalVal)
                Nothing -> return Nothing
-}
-- | Retreive the value of a variable
varLookup :: ( InterpreterShell m
             {-, Snobol4Machine program-}
             , LocalVariablesClass m
             ) 
          => Snobol4String 
          -> InterpreterGeneric program m (Maybe (Data (ExprType m)))
varLookup name = do
    maybeReference <- getReference name
    mapM varDeref maybeReference
    

{-
-- | Write the value of a global variable
globalWrite :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
             => Snobol4String -> (Data (ExprType m)) -> InterpreterGeneric program m ()
globalWrite name val = do 
    vars <- getVariables
    ix <- case M.lookup name $ dynamicVars vars of
        Nothing -> do
            let ix = V.length $ staticVars vars
            modifyStaticVars $ flip V.snoc $ StringData nullString
            modifyDynamicVars $ M.insert name $ GlobalVar ix
            return ix
        Just (GlobalVar ix) -> return ix
        Just (LocalVar ix) -> 
    globalWrite' ix val
-}

globalWrite :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
            => Int 
            -> (Data (ExprType m)) 
            -> InterpreterGeneric program m ()
globalWrite ix val = modifyStaticVars $ (V.// [(ix,val)])

-- | Create a new global variable
globalAppend :: ( InterpreterShell m{-, Snobol4Machine program-} )
             => Snobol4String
             -> (Data (ExprType m))
             -> InterpreterGeneric program m ()
globalAppend name val = do
    vars <- getVariables
    let ix = V.length $ staticVars vars
    modifyStaticVars $ flip V.snoc val
    modifyDynamicVars $ M.insert name $ GlobalVar ix


-- | Write the value of a local variable
localWrite :: ( InterpreterShell m{-, Snobol4Machine program-}, LocalVariablesClass m ) 
           => Int 
           -> (Data (ExprType m)) 
           -> InterpreterGeneric program m ()
localWrite ix x = do
    result <- lift $ writeLocal ix x
    case result of
        Nothing -> programError $ ErrorInSnobol4System
        _ -> return ()


getReference :: ( InterpreterShell m
--                {-, Snobol4Machine program-}
                ) 
             => Snobol4String 
             -> InterpreterGeneric program m (Maybe VarType)
getReference name = do
    vars <- getVariables
    return $ M.lookup name $ dynamicVars vars


-- | Set a variable to reference a local or global variable
setReference :: ( InterpreterShell m
--                {-, Snobol4Machine program-}
                ) 
             => Snobol4String -> (Maybe VarType) -> InterpreterGeneric program m ()
setReference name Nothing = modifyDynamicVars $ M.delete name
setReference name (Just ref) = modifyDynamicVars $ M.insert name ref

-- | Update the value of a variable, creating a new global if needed
varWrite :: ( InterpreterShell m{-, Snobol4Machine program-}, LocalVariablesClass m ) 
         => Snobol4String 
         -> (Data (ExprType m)) 
         -> InterpreterGeneric program m ()
varWrite name val = do
    val' <- case val of
        (TempPatternData p) -> PatternData <$> patternsNew p
        x -> return x
    result <- getReference name
    case result of
        Just (LocalVar ix) -> do
            old <- localLookup ix
            decRef old
            localWrite ix val'
        Just (GlobalVar ix) -> do
            old <- globalLookup ix
            decRef old
            globalWrite ix val'
        Nothing -> globalAppend name val'
    incRef val'

-- | Delete a variable
clearVar :: ( InterpreterShell m{-, Snobol4Machine program-}, LocalVariablesClass m )
         => Snobol4String -> InterpreterGeneric program m ()
clearVar n = do
    result <- varLookup n
    case result of
        Just val -> do
            decRef val
            modifyDynamicVars $ M.delete n -- TODO: Cleanup
        Nothing -> return ()

-- | Erase all variables
wipeVariables :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
              => InterpreterGeneric program m ()
wipeVariables = putVariables $ noVariables

-- | Get the names of the natural variables
naturalVarNames :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
                => InterpreterGeneric program m [Snobol4String]
naturalVarNames = liftM (M.keys . dynamicVars) getVariables

-- | Take a value, if it is a reference to data maintained by the GC, increment
-- the number of references to it
incRef :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
       => (Data (ExprType m)) -> InterpreterGeneric program m ()
incRef (PatternData k) = patternsIncRef k
incRef (ArrayData k) = arraysIncRef k
incRef (TableData k) = tablesIncRef k
incRef _ = return ()

-- | Take a value, if it is a reference to data maintained by the GC, decrement
-- the number of references to it
decRef :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
       => (Data (ExprType m)) -> InterpreterGeneric program m ()
decRef (PatternData k) = patternsDecRef k
decRef (ArrayData k) = arraysDecRef k
decRef (TableData k) = tablesDecRef k
decRef _ = return ()

-- | Assign a value to the location pointed to by a lookup
assign :: ( InterpreterShell m
          {-, Snobol4Machine program-}
          , LocalVariablesClass m
          , Ord (ExprType m)
          ) 
       => (Lookup (ExprType m)) 
       -> (Data (ExprType m)) 
       -> InterpreterGeneric (ProgramType m) m ()
assign (LookupId s) val = varWrite s val
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
    base <- varLookup name
    case base of
        Just baseVal -> loop baseVal args
        Nothing -> programError ErroneousArrayOrTableReference
assign LookupOutput val = toString val >>= lift . output . unmkString
assign LookupPunch val = toString val >>= lift . punch . unmkString
assign (LookupKeyword sym) val = assignKeyword sym val
assign (LookupUserData key dataName ix) val = userDataModify key dataName ix val
assign _ _ = programError VariableNotPresentWhereRequired

lookup :: ( InterpreterShell m
          {-, Snobol4Machine program-}
          , LocalVariablesClass m
          , Ord (ExprType m)
          ) 
       => (Lookup (ExprType m)) 
       -> InterpreterGeneric (ProgramType m) m (Data (ExprType m))
lookup l = do
    result <- execLookup l
    case result of
        Just val -> return val
        Nothing -> return $ StringData nullString

-- | Get the data pointed to by a lookup
execLookup :: ( InterpreterShell m
              {-, Snobol4Machine program-}
              , LocalVariablesClass m
              , Ord (ExprType m)
              ) 
           => (Lookup (ExprType m)) 
           -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m))) 
execLookup LookupInput = (liftM $ StringData . mkString) <$> lift input 
execLookup LookupOutput = (Just . StringData . mkString) <$> lift lastOutput 
execLookup LookupPunch = (Just . StringData . mkString) <$> lift lastPunch 
execLookup (LookupLiteral x) = return $ Just x 
execLookup (LookupId i) = varLookup i
execLookup (LookupAggregate name args) = do
    base <- varLookup name
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
execLookup (LookupKeyword sym) = liftM Just $ lookupKeyword sym
execLookup (LookupUserData key dataName ix) = userDataSelect key dataName ix
