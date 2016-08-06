module Language.Snobol4.Interpreter.Internal.StateMachine.ObjectCode where

import qualified Data.Map as M

import Control.Monad

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC

noCodes :: Codes
noCodes = M.empty

getCodes :: InterpreterShell m => Interpreter m Codes
getCodes = getsProgramState codes

modifyCodes :: InterpreterShell m
            => (Codes -> Codes)
            -> Interpreter m ()
modifyCodes f = modifyProgramState $
    \st -> st { codes = f $ codes st }

codesNew :: InterpreterShell m => Snobol4Code -> Interpreter m CodeKey
codesNew code = do
    newKey <- (succ . fst . M.findMax) `liftM` getCodes
    modifyCodes $ M.insert newKey $ newRef code
    return newKey

codesLookup :: InterpreterShell m => CodeKey -> Interpreter m (Maybe Snobol4Code)
codesLookup k = fmap getRefItem <$> M.lookup k <$> getCodes

codesUpdate :: InterpreterShell m => (Snobol4Code -> Snobol4Code) -> CodeKey -> Interpreter m ()
codesUpdate f k = modifyCodes $ M.adjust (fmap f) k

codesIncRef :: InterpreterShell m => CodeKey -> Interpreter m ()
codesIncRef k = modifyCodes $ M.adjust incRefCount k

codesDecRef :: InterpreterShell m => CodeKey -> Interpreter m ()
codesDecRef k = modifyCodes $ M.update decRefCount k
