{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Snobol4.Interpreter.Scanner.New.FullScan where


import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Scanner.New.Types
import Language.Snobol4.Interpreter.Scanner.New.Common


runFullScan :: (ResolveClass expr m)
            => FullScanPath expr
            -> ScannerContT expr m Snobol4String
runFullScan (ScanConcat a b _) next = do
    runFullScan a $ do
        prevMatch <- getPrevMatch
        runFullScan b $ do
            prevMatch' <- getPrevMatch
            putPrevMatch (prevMatch <> prevMatch')
            next
runFullScan (ScanChoice a b _) next = catchScan (runFullScan a next) (runFullScan b next)
runFullScan (ScanImmediateAssign pat l _) next = runFullScan pat $ do
    prev <- getPrevMatch
    immediateAssign l $ StringData prev
    next












runFullScan' :: (ResolveClass expr m)
              => {-ScannerEnv expr m
              -> -}FullScanPath expr
              -> Snobol4String
              -> Snobol4Integer
              -> Bool
              -> m (ScanResult expr)
runFullScan' {-env-} pat toMatch offset anchorMode = do
    let st = ScannerState toMatch "" "" [] offset
        retry = case snobol4Uncons toMatch of
            Just (_,nextTry) -> if anchorMode
                then return NoScan
                else runFullScan' {-env-} pat nextTry (offset + 1) anchorMode
            _ -> return NoScan
    (result, st') <- {-flip runReaderT env
            $ runScannerEnvT
            $-} flip runStateT st
            $ runExceptT 
            $ runScannerT
            $ runFullScan pat succeed
    
    case result of
        Left Abort -> return NoScan
        Left Backtrack -> retry
        Left NotEnoughCharacters -> retry
        Right _ -> return $ Scan (StringData $ matched st') 
                                 (assignments st') 
                                 offset 
                                 (offset + snobol4Length (matched st'))


fullscan :: (ResolveClass expr m)
          => Pattern expr
          -> Snobol4String
          -> Bool
          {--> (expr -> m (Maybe (Pattern expr)))
          -> (expr -> m (Maybe Snobol4Integer))
          -> (expr -> m (Maybe Snobol4String))
          -> (Lookup expr -> Data expr -> m ())-}
          -> m (ScanResult expr)
fullscan pat toMatch anchorMode {-toPat toInt toStr set-} = do
    let len = snobol4Length toMatch
        {-env = ScannerEnv anchorMode True toPat toInt toStr set-}
        path = buildFullScanPath pat
    runFullScan' {-env-} path toMatch 0 anchorMode



