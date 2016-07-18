{-|
Module          : Language.Snobol4.Interpreter.Scanner
Description     : Interface to the scanner
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown


Interface to the SNOBOL4 pattern scanner
-}

module Language.Snobol4.Interpreter.Scanner ( scanPattern ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe

import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.Types
import Language.Snobol4.Interpreter.Scanner.Internal


-- | Invoke the scanner on a string with a pattern
scanPattern :: InterpreterShell m
            => String
            -> Pattern
            -> Evaluator m ScanResult
scanPattern s pat = do
    result <- runMaybeT 
            $ flip runStateT (startState s) 
            $ runScanner 
            $ matchPat pat
    case result of
        Just (x, st) -> do
            let ScannerState{assignments=as,startPos=a,endPos=b} = st
            return $ Scan (StringData x) as a b
        Nothing -> return NoScan
