{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Labels
Description     : Maintaining labels
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.Labels where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector as V

import Language.Snobol4.Syntax.AST hiding (getProgram)

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.Statements
import Language.Snobol4.Interpreter.Internal.StateMachine.GC

-- | Empty collection of labels
noLabels :: Labels
noLabels = M.empty
 
-- | Get the labels known to the interpreter
getLabels :: InterpreterShell m => InterpreterGeneric program m Labels
getLabels = getsProgramState labels

-- | Set the labels known to the interpreter
putLabels :: InterpreterShell m => Labels -> InterpreterGeneric program m ()
putLabels lbls = modifyProgramState $ \st -> st { labels = lbls }

-- | Apply a function to the labels known to the interpreter
modifyLabels :: InterpreterShell m 
             => (Labels -> Labels)
             -> InterpreterGeneric program m ()
modifyLabels f = modifyProgramState $
    \st -> st { labels = f $ labels st }

-- | Scan the loaded program for labels, and then add them
scanForLabels :: InterpreterShell m
              => InterpreterGeneric Statements m ()
scanForLabels = do
    stmts <- getProgram
    let lbls = flip V.imap (getStatements stmts) $ \ix stmt -> case stmt of
            Stmt (Just lbl) _ _ _ _ -> Just (mkString lbl,Label $ Address $ mkInteger ix)
            _ -> Nothing
        lbls' = V.filter isJust lbls
        Just lbls'' = sequenceA lbls'
        lblMap = M.fromList $ V.toList lbls''
    modifyLabels $ M.union lblMap

-- | Find the index of the statement with a label
labelLookup :: InterpreterShell m => Snobol4String -> InterpreterGeneric program m (Maybe Label)
labelLookup lbl = M.lookup lbl <$> getLabels


