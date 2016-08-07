module Language.Snobol4.Interpreter.Internal.StateMachine.Labels where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector as V

import Language.Snobol4.Syntax.AST

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.Statements
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

scanForLabels :: InterpreterShell m
              => Interpreter m ()
scanForLabels = do
    stmts <- getStatements
    let lbls = flip V.imap stmts $ \ix stmt -> case stmt of
            Stmt (Just lbl) _ _ _ _ -> Just (mkString lbl,Label $ Address $ mkInteger ix)
            _ -> Nothing
        lbls' = V.filter isJust lbls
        Just lbls'' = sequenceA lbls'
        lblMap = M.fromList $ V.toList lbls''
    modifyLabels $ M.union lblMap

-- | Find the index of the statement with a label
labelLookup :: InterpreterShell m => Snobol4String -> Interpreter m (Maybe Label)
labelLookup lbl = M.lookup lbl <$> getLabels


