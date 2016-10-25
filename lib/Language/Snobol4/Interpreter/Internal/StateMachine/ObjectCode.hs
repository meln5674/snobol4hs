{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.ObjectCode
Description     : Maintaining object code
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.ObjectCode where

import qualified Data.Map as M

import Control.Monad

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC

-- | Empty collection of object code
noCodes :: Codes
noCodes = M.empty

-- | Get the object code known to the interpreter
getCodes :: InterpreterShell m => InterpreterGeneric program m Codes
getCodes = getsProgramState codes

-- | Apply a function to the object code known to the interpreter
modifyCodes :: InterpreterShell m
            => (Codes -> Codes)
            -> InterpreterGeneric program m ()
modifyCodes f = modifyProgramState $
    \st -> st { codes = f $ codes st }

-- | Get the next key availible for referencing an object code
codesNextKey :: InterpreterShell m => InterpreterGeneric program m CodeKey
codesNextKey = liftM (maybe (toEnum 0) (succ . fst . fst) . M.maxViewWithKey) getCodes

-- | Create a new object code
codesNew :: InterpreterShell m => Snobol4Code -> InterpreterGeneric program m CodeKey
codesNew code = do
    newKey <- codesNextKey
    modifyCodes $ M.insert newKey $ newRef code
    return newKey

-- | Lookup an object code
codesLookup :: InterpreterShell m => CodeKey -> InterpreterGeneric program m (Maybe Snobol4Code)
codesLookup k = fmap getRefItem <$> M.lookup k <$> getCodes

-- | Apply a function to an object code
codesUpdate :: InterpreterShell m => (Snobol4Code -> Snobol4Code) -> CodeKey -> InterpreterGeneric program m ()
codesUpdate f k = modifyCodes $ M.adjust (fmap f) k

-- | Increment the number of references to an object code
codesIncRef :: InterpreterShell m => CodeKey -> InterpreterGeneric program m ()
codesIncRef k = modifyCodes $ M.adjust incRefCount k

-- | Decrement the number of references to an object code
codesDecRef :: InterpreterShell m => CodeKey -> InterpreterGeneric program m ()
codesDecRef k = modifyCodes $ M.update decRefCount k
