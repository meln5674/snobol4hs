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
{-# LANGUAGE FlexibleContexts #-}
module Language.Snobol4.Interpreter.Scanner ( scanPattern ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Internal.StateMachine
import Language.Snobol4.Interpreter.Scanner.Internal

-- | Match a string against a pattern
scanPattern :: ( InterpreterShell m
               , NewSnobol4Machine m
               , LocalVariablesClass m
               , Ord (ExprType m)
               )
            => Snobol4String
            -> Pattern (ExprType m)
            -> Bool
            -> InterpreterGeneric (ProgramType m) m (ScanResult (ExprType m))
scanPattern toScan pat anchor = loop 0 $ if anchor 
    then 0
    else unmkInteger $ snobol4Length toScan
  where
    loop skip maxSkip
        | skip > maxSkip = return NoScan
        | otherwise = do
            result <- runExceptT 
                    $ flip runStateT (startState toScan skip)
                    $ runScanner 
                    $ match pat return nullString
            case result of
                Right (matchResult, st) -> do
                    let ScannerState
                         { assignments=toAssign
                         , startPos=matchStart
                         , endPos=matchEnd
                         } = st
                    return $ Scan (StringData matchResult) toAssign (skip + matchStart) (skip + matchEnd)
                Left _ -> loop (skip+1) maxSkip
        
    
