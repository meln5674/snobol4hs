{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Functions
Description     : Maintaining and calling functions
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Language.Snobol4.Interpreter.Internal.StateMachine.Functions where

import qualified Data.Map as M

import Control.Monad

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Error
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC
import Language.Snobol4.Interpreter.Internal.StateMachine.CallStack
import Language.Snobol4.Interpreter.Internal.StateMachine.Variables
import Language.Snobol4.Interpreter.Internal.StateMachine.Run

-- | Empty collection of functions
noFunctions :: ( InterpreterShell m, Snobol4Machine program ) => Functions program m
noFunctions = M.empty

-- | Get the functions known to the interpreter
getFunctions :: ( InterpreterShell m, Snobol4Machine program ) => InterpreterGeneric program m (Functions program m)
getFunctions = getsProgramState functions

-- | Set the functions known to the interpreter
putFunctions :: ( InterpreterShell m, Snobol4Machine program ) => Functions program m -> InterpreterGeneric program m ()
putFunctions funcs = modifyProgramState $ \st -> st { functions = funcs }

-- | Apply a function to the functions known to the interpreter
modifyFunctions :: ( InterpreterShell m, Snobol4Machine program ) => (Functions program m -> Functions program m) -> InterpreterGeneric program m ()
modifyFunctions f = modifyProgramState $
    \st -> st { functions = f $ functions st }

-- | Erase all functions known to the interpreter
clearFunc :: ( InterpreterShell m, Snobol4Machine program ) => Snobol4String-> InterpreterGeneric program m ()
clearFunc = modifyFunctions . M.delete

-- | Look up a function by name
funcLookup :: ( InterpreterShell m, Snobol4Machine program ) => Snobol4String -> InterpreterGeneric program m (Maybe (Function program m))
funcLookup name = M.lookup name <$> getFunctions

-- | Add a new function
functionsNew :: ( InterpreterShell m, Snobol4Machine program ) => UserFunction -> InterpreterGeneric program m ()
functionsNew func = modifyFunctions $ M.insert (funcName func) $ UserFunction func


callJump :: ( InterpreterShell m, Snobol4Machine program )
         => UserFunction -- ^ Name of the function
         -> [Data] -- ^ Arguments to pass
         -> InterpreterGeneric program m ()
callJump func@Function{funcName, formalArgs, localNames} evaldArgs = do
    -- Push current state onto the stack and create blank spots for
    --  args and locals
    pushFuncFrame func

    let allLocalNames = funcName : formalArgs ++ localNames

    -- Capture the references held by the names of the return variable,
    --  locals, and args
    oldReferences <- forM allLocalNames $ \name -> do
        ref <- getReference name
        return (name, ref)
    modifyCallStackHead $ \n -> n{ oldReferences }
    -- Set the function name, arguments, and local variables to
    --  reference the appropriate spot in the new stack frame
    mapM_ (uncurry setReference) $ zip allLocalNames $ map LocalVar [0..]
    mapM_ (uncurry varWrite) $ zip formalArgs evaldArgs
    -- Jump to the entry point of the function and execute the passed function
    putProgramCounter $ entryPoint func



returnJump :: ( InterpreterShell m, Snobol4Machine program )
           => ExecResult
           -> InterpreterGeneric program m (Maybe Data)
returnJump result = do
    -- Get the return value, and if the the function failed or not
    toReturn <- case result of
        Return -> liftM Just $ localLookup 0
        FReturn -> return Nothing
    Frame{oldReferences} <- popCallStack
    -- Reset the references captured earlier
    forM_ oldReferences $ \case
        (name, Nothing) -> return ()
        (name, Just ref) -> setReference name ref
    return toReturn

-- | Call a function
callFunction :: ( InterpreterShell m, Snobol4Machine program ) 
             => Snobol4String -- ^ Name of the function
             -> [Data] -- ^ Arguments to pass
             -> InterpreterGeneric program m ExecResult -- ^ Action to be performed between
                                         -- pushing arguments and popping result
             -> InterpreterGeneric program m (Maybe Data)
callFunction name evaldArgs f = do
    lookupResult <- funcLookup name
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
            
        Just PrimitiveFunction{funcPrim=action} -> do
            catchEval (action evaldArgs) $ const $ return Nothing
        
        Just (UserFunction func) -> do
            callJump func evaldArgs
            result <- f
            returnJump result

