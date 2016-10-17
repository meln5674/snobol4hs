{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Patterns
Description     : Maintaining patterns
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.Patterns where

import qualified Data.Map as M

import Control.Monad

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC

-- | Empty collection of patterns
noPatterns :: (Patterns expr)
noPatterns = M.empty

-- | Get the patterns known to the interpreter
getPatterns :: InterpreterShell m => InterpreterGeneric program m (Patterns (ExprType m))
getPatterns = getsProgramState patterns

-- | Set the patterns known to the interpreter
putPatterns :: InterpreterShell m => (Patterns (ExprType m)) -> InterpreterGeneric program m ()
putPatterns pats = modifyProgramState $ \st -> st { patterns = pats }

-- | Apply a function to the patterns known to the interpreter
modifyPatterns :: InterpreterShell m
               => ((Patterns (ExprType m)) -> (Patterns (ExprType m)))
               -> InterpreterGeneric program m ()
modifyPatterns f = modifyProgramState $
    \st -> st { patterns = f $ patterns st }

-- | Create a new pattern
patternsNew :: InterpreterShell m => (Pattern (ExprType m)) -> InterpreterGeneric program m PatternKey
patternsNew pat = do
    newKey <- (succ . fst . M.findMax) `liftM` getPatterns
    modifyPatterns $ M.insert newKey $ newRef pat
    return newKey

-- | Lookup a pattern
patternsLookup :: InterpreterShell m => PatternKey -> InterpreterGeneric program m (Maybe (Pattern (ExprType m)))
patternsLookup k = fmap getRefItem <$> M.lookup k <$> getPatterns

-- | Apply a function to a pattern
patternsUpdate :: InterpreterShell m => ((Pattern (ExprType m)) -> (Pattern (ExprType m))) -> PatternKey -> InterpreterGeneric program m ()
patternsUpdate f k = modifyPatterns $ M.adjust (fmap f) k

-- | Increment the number of references to a pattern
patternsIncRef :: InterpreterShell m => PatternKey -> InterpreterGeneric program m ()
patternsIncRef k = modifyPatterns $ M.adjust incRefCount k

-- | Decrement the number of references to a pattern
patternsDecRef :: InterpreterShell m => PatternKey -> InterpreterGeneric program m ()
patternsDecRef k = modifyPatterns $ M.update decRefCount k
