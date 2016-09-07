{-|
Module          : Language.Snobol4.Interpreter.Scanner
Description     : Interface to the scanner
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown


Interface to the SNOBOL4 pattern scanner
-}

{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Snobol4.Interpreter.Scanner ( scanPattern ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Internal.StateMachine
import Language.Snobol4.Interpreter.Scanner.Internal

-- | Match a string against a pattern
scanPattern :: ( InterpreterShell m
               , Snobol4Machine program
               )
            => Snobol4String
            -> Pattern
            -> EvaluatorGeneric program (EvaluationError program) m ScanResult
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
