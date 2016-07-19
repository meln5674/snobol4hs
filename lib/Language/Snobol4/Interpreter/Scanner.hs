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
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.Types
import Language.Snobol4.Interpreter.Scanner.Internal

scanPattern :: InterpreterShell m
            => Snobol4String
            -> Pattern
            -> Evaluator m ScanResult
scanPattern toScan pat = do
    result <- runExceptT 
            $ flip runStateT (startState toScan)
            $ runScanner 
            $ match pat return nullString
    case result of
        Right (matchResult, st) -> do
            let ScannerState
                 { assignments=toAssign
                 , startPos=matchStart
                 , endPos=matchEnd
                 } = st
            return $ Scan (StringData matchResult) toAssign matchStart matchEnd
        Left _ -> return NoScan
