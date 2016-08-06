module Language.Snobol4.Interpreter.Internal.StateMachine.Patterns where

import qualified Data.Map as M

import Control.Monad

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC

noPatterns :: Patterns
noPatterns = M.empty

getPatterns :: InterpreterShell m => Interpreter m Patterns
getPatterns = getsProgramState patterns

putPatterns :: InterpreterShell m => Patterns -> Interpreter m ()
putPatterns pats = modifyProgramState $ \st -> st { patterns = pats }

modifyPatterns :: InterpreterShell m
               => (Patterns -> Patterns)
               -> Interpreter m ()
modifyPatterns f = modifyProgramState $
    \st -> st { patterns = f $ patterns st }

patternsNew :: InterpreterShell m => Pattern -> Interpreter m PatternKey
patternsNew pat = do
    newKey <- (succ . fst . M.findMax) `liftM` getPatterns
    modifyPatterns $ M.insert newKey $ newRef pat
    return newKey

patternsLookup :: InterpreterShell m => PatternKey -> Interpreter m (Maybe Pattern)
patternsLookup k = fmap getRefItem <$> M.lookup k <$> getPatterns

patternsUpdate :: InterpreterShell m => (Pattern -> Pattern) -> PatternKey -> Interpreter m ()
patternsUpdate f k = modifyPatterns $ M.adjust (fmap f) k

patternsIncRef :: InterpreterShell m => PatternKey -> Interpreter m ()
patternsIncRef k = modifyPatterns $ M.adjust incRefCount k

patternsDecRef :: InterpreterShell m => PatternKey -> Interpreter m ()
patternsDecRef k = modifyPatterns $ M.update decRefCount k
