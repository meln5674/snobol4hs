{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Functions
Description     : Maintaining and calling functions
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.Functions where

import qualified Data.Map as M

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
noFunctions :: InterpreterShell m => Functions m
noFunctions = M.empty

-- | Get the functions known to the interpreter
getFunctions :: InterpreterShell m => Interpreter m (Functions m)
getFunctions = getsProgramState functions

-- | Set the functions known to the interpreter
putFunctions :: InterpreterShell m => Functions m -> Interpreter m ()
putFunctions funcs = modifyProgramState $ \st -> st { functions = funcs }

-- | Apply a function to the functions known to the interpreter
modifyFunctions :: InterpreterShell m => (Functions m -> Functions m) -> Interpreter m ()
modifyFunctions f = modifyProgramState $
    \st -> st { functions = f $ functions st }

-- | Erase all functions known to the interpreter
clearFunc :: InterpreterShell m => Snobol4String-> Interpreter m ()
clearFunc = modifyFunctions . M.delete

-- | Look up a function by name
funcLookup :: InterpreterShell m => Snobol4String -> Interpreter m (Maybe (Function m))
funcLookup name = M.lookup name <$> getFunctions

-- | Add a new function
functionsNew :: InterpreterShell m => Function m -> Interpreter m ()
functionsNew func = modifyFunctions $ M.insert (funcName func) func

-- | Call a function
callFunction :: InterpreterShell m 
             => Snobol4String -- ^ Name of the function
             -> [Data] -- ^ Arguments to pass
             -> Interpreter m ExecResult -- ^ Action to be performed between
                                         -- pushing arguments and popping result
             -> Interpreter m (Maybe Data)
callFunction name evaldArgs f = do
    lookupResult <- funcLookup name
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just func@UserFunction{formalArgs=argNames} -> do
            pushFuncNode func
            mapM_ (uncurry varWrite) $ zip argNames evaldArgs
            putProgramCounter $ entryPoint func
            result <- f
            toReturn <- case result of
                Return -> varLookup' $ name
                FReturn -> return Nothing
            _ <- popCallStack
            return toReturn
        Just PrimitiveFunction{funcPrim=action} -> do
            catchEval (action evaldArgs) $ const $ return Nothing
