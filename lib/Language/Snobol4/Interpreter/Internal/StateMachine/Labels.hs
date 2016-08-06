module Language.Snobol4.Interpreter.Internal.StateMachine.Labels where

import qualified Data.Map as M

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC

noLabels :: Labels
noLabels = M.empty
 
 -- | Get the labels known to the interpreter
getLabels :: InterpreterShell m => Interpreter m Labels
getLabels = getsProgramState labels

-- | Set the labels known to the interpreter
putLabels :: InterpreterShell m => Labels -> Interpreter m ()
putLabels lbls = modifyProgramState $ \st -> st { labels = lbls }

-- | Apply a function to the labels known to the interpreter
modifyLabels :: InterpreterShell m 
             => (Labels -> Labels)
             -> Interpreter m ()
modifyLabels f = modifyProgramState $
    \st -> st { labels = f $ labels st }

-- | Find the index of the statement with a label
labelLookup :: InterpreterShell m => Snobol4String -> Interpreter m (Maybe Label)
labelLookup lbl = M.lookup lbl <$> getLabels


