{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Snobol4.Interpreter.Scanner.New.Common where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except



import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Scanner.New.Types


getToMatch :: ResolveClass expr m => ScannerT expr m Snobol4String
getToMatch = ScannerT $ lift $ gets toMatch

putToMatch :: ResolveClass expr m => Snobol4String -> ScannerT expr m ()
putToMatch x = ScannerT $ lift $ modify $ \st -> st { toMatch = x}

getMatched :: ResolveClass expr m => ScannerT expr m Snobol4String
getMatched = ScannerT $ lift $ gets matched

putMatched :: ResolveClass expr m => Snobol4String -> ScannerT expr m ()
putMatched x = ScannerT $ lift $ modify $ \st -> st { matched = x}

getPrevMatch :: ResolveClass expr m => ScannerT expr m Snobol4String
getPrevMatch = ScannerT $ lift $ gets prevMatch

putPrevMatch :: ResolveClass expr m => Snobol4String -> ScannerT expr m ()
putPrevMatch x = ScannerT $ lift $ modify $ \st -> st { prevMatch = x}

addAssignment :: ResolveClass expr m => Lookup expr -> Data expr -> ScannerT expr m ()
addAssignment l x = ScannerT $ lift $ modify $ \st -> st { assignments = (l,x) : assignments st }

getOffset :: ResolveClass expr m => ScannerT expr m Snobol4Integer
getOffset = ScannerT $ lift $ gets offset

incOffset :: ResolveClass expr m => ScannerT expr m ()
incOffset = ScannerT $ lift $ modify $ \st -> st { offset = 1 + offset st }

-- | Apply one of two functions, depending on if a lazy value has been evaluated or not
buildLazy :: (expr -> b) -> (a -> b) -> Lazy expr a -> b
buildLazy f _ (Thunk expr) = f expr
buildLazy _ g (EvaluatedThunk x) = g x

-- | Build a fullscan tree from a pattern structure
buildFullScanPath :: Pattern expr -> FullScanPath expr
buildFullScanPath (AssignmentPattern pat l) = 
    let pat' = buildLazy UnevaluatedPattern buildFullScanPath pat
    in  ScanAssign pat' l ()
buildFullScanPath (ImmediateAssignmentPattern pat l) = 
    let pat' = buildLazy UnevaluatedPattern buildFullScanPath pat
    in  ScanImmediateAssign pat' l ()
buildFullScanPath (LiteralPattern s) = ScanNode (ScanLiteral s) ()
buildFullScanPath (LengthPattern i) = ScanNode (ScanLen i) ()
buildFullScanPath (AlternativePattern a b) = 
    let a' = buildLazy UnevaluatedPattern buildFullScanPath a
        b' = buildLazy UnevaluatedPattern buildFullScanPath b
    in  ScanChoice a' b' ()
buildFullScanPath (ConcatPattern a b) =
    let a' = buildLazy UnevaluatedPattern buildFullScanPath a
        b' = buildLazy UnevaluatedPattern buildFullScanPath b
    in  ScanConcat a' b' ()
buildFullScanPath EverythingPattern = ScanNode (ScanRemainder) ()
buildFullScanPath (HeadPattern l) = ScanNode (ScanHead l) ()
buildFullScanPath (SpanPattern cs) = ScanNode (ScanSpan cs) ()
buildFullScanPath (BreakPattern cs) = ScanNode (ScanBreak cs) ()
buildFullScanPath (AnyPattern cs) = ScanNode (ScanAny cs) ()
buildFullScanPath (NotAnyPattern cs) = ScanNode (ScanNotAny cs) ()
buildFullScanPath (TabPattern i) = ScanNode (ScanTab i) ()
buildFullScanPath (RTabPattern i) = ScanNode (ScanRTab i) ()
buildFullScanPath (PosPattern i) = ScanNode (ScanPos i) ()
buildFullScanPath (RPosPattern i) = ScanNode (ScanRPos i) ()
buildFullScanPath (FailPattern) = ScanNode ScanFail ()
buildFullScanPath (FencePattern) = ScanNode ScanFence ()
buildFullScanPath (AbortPattern) = ScanNode ScanAbort ()
buildFullScanPath (ArbPattern) = ScanNode ScanArb ()
buildFullScanPath (ArbNoPattern pat) = 
    let pat' = buildLazy UnevaluatedPattern buildFullScanPath pat
    in  ScanArbNo pat' ()
buildFullScanPath BalPattern = ScanNode ScanBal ()
buildFullScanPath SucceedPattern = ScanNode ScanSucceed ()

-- | Attempt to consume a string
consumeString :: (ResolveClass expr m)
              => Snobol4String
              -> ScannerT expr m ()
consumeString toConsume = do
    toMatch <- getToMatch
    matched <- getMatched
    let toBeConsumed = snobol4Take (snobol4Length toConsume) toMatch
        toMatch' = snobol4Drop (snobol4Length toConsume) toMatch
        matched' = matched <> toConsume
    if toConsume == toBeConsumed
        then do
            putToMatch toMatch'
            putMatched matched'
            putPrevMatch toConsume
        else if snobol4Length toBeConsumed < snobol4Length toConsume
            then notEnoughCharacters
            else backtrack

consumeN :: ResolveClass expr m
         => Snobol4Integer
         -> ScannerT expr m ()
consumeN n = do
    toMatch <- getToMatch
    matched <- getMatched
    let toMatch' = snobol4Drop n toMatch
        consumed = snobol4Take n toMatch
        matched' = matched <> consumed
    if snobol4Length consumed == n
        then do
            putToMatch toMatch'
            putMatched matched'
            putPrevMatch consumed
        else notEnoughCharacters

consumeAny :: (ResolveClass expr m)
           => Snobol4String
           -> ScannerT expr m ()
consumeAny cs = do
    toMatch <- getToMatch
    matched <- getMatched
    case snobol4Uncons toMatch of
        Nothing -> notEnoughCharacters
        Just (c,toMatch') -> do
            if c `snobol4Elem` cs
                then do
                    putToMatch toMatch'
                    putMatched (matched <> c)
                    putPrevMatch c
                else backtrack

consumeNotAny :: (ResolveClass expr m)
           => Snobol4String
           -> ScannerT expr m ()
consumeNotAny cs  = do
    toMatch <- getToMatch
    matched <- getMatched
    case snobol4Uncons toMatch of
        Nothing -> notEnoughCharacters
        Just (c,toMatch') -> do
            if not $ c `snobol4Elem` cs
                then do
                    putToMatch toMatch'
                    putMatched (matched <> c)
                    putPrevMatch c
                else backtrack


many1 :: (ResolveClass expr m)
      => ScannerT expr m a
      -> ScannerContT expr m b
many1 f next = f >> go
  where
    go = do
        prevMatch <- getPrevMatch
        catchScan' f 
                        (\_ -> do { prevMatch' <- getPrevMatch
                                  ; putPrevMatch (prevMatch <> prevMatch')
                                  ; go
                                  }
                        )
                        next
many :: (ResolveClass expr m)
      => ScannerT expr m a
      -> ScannerContT expr m b
many f next = go
  where
    go = do
        prevMatch <- getPrevMatch
        catchScan' f 
                   (\_ -> do { prevMatch' <- getPrevMatch
                             ; putPrevMatch (prevMatch <> prevMatch')
                             ; go
                             }
                   )
                   next

repeat :: (ResolveClass expr m)
       => Snobol4Integer
       -> ScannerT expr m a
       -> ScannerContT expr m b
repeat n f next = putPrevMatch "" >> go n
  where
   go n | n <= 0 = next
        | otherwise = do
            prevMatch <- getPrevMatch
            f
            prevMatch' <- getPrevMatch
            putPrevMatch (prevMatch <> prevMatch')
            go (n-1)

withLazyStr :: ResolveClass expr m
            => (LazyString expr) 
            -> (Snobol4String -> ScannerT expr m a) 
            -> ScannerT expr m a
withLazyStr (Thunk expr) f = do
    s <- resolveString expr
    case s of
        Nothing -> backtrack
        Just s -> f s
withLazyStr (EvaluatedThunk s) f = f s


withLazyInt :: ResolveClass expr m 
            => (LazyInteger expr) 
            -> (Snobol4Integer -> ScannerT expr m a) 
            -> ScannerT expr m a
withLazyInt (Thunk expr) f = do
    s <- resolveInteger expr
    case s of
        Nothing -> backtrack
        Just s -> f s
withLazyInt (EvaluatedThunk s) f = f s



failScan :: ResolveClass expr m => ScanFailure -> ScannerT expr m a
failScan = ScannerT . throwE

-- | Backtrack to the nearest checkpoint
backtrack :: (ResolveClass expr m) => ScannerT expr m a
backtrack = failScan Backtrack

-- | Abort the scan
abort :: (ResolveClass expr m) => ScannerT expr m a
abort = failScan Abort

-- | Backtrack to the nearest checkpoint, noting that there were not enough
-- characters to match the pattern
notEnoughCharacters :: ResolveClass expr m => ScannerT expr m a
notEnoughCharacters = failScan NotEnoughCharacters

-- | Complete the scan with success
succeed :: ResolveClass expr m => ScannerT expr m Snobol4String
succeed = getMatched

-- | Perform an action, with a backup contination to take if it fails
catchScan :: (ResolveClass expr m)
               => ScannerT expr m a -- ^ Action to attempt
               -> ScannerT expr m a -- ^ Action to take if it fails
               -> ScannerT expr m a
catchScan try catch = do
    st <- ScannerT $ lift get
    {-env <- ScannerT $ lift $ lift $ ScannerEnvT $ ask-}
    (result, st') <- lift 
            {-$ flip runReaderT env
            $ runScannerEnvT-}
            $ flip runStateT st
            $ runExceptT 
            $ runScannerT
            $ try
    case result of
        Right value -> do
            ScannerT $ lift $ put st'
            return value
        Left Abort -> abort
        Left Backtrack -> catch
        Left NotEnoughCharacters -> catch


catchScan' :: (ResolveClass expr m)
               => ScannerT expr m a -- ^ Action to attempt
               -> (a -> ScannerT expr m b) -- ^ Action to take if successful 
               -> ScannerT expr m b -- ^ Action to take if it fails
               -> ScannerT expr m b
catchScan' try then_ catch = do
    st <- ScannerT $ lift get
    {-env <- ScannerT $ lift $ lift $ ScannerEnvT $ ask-}
    (result, st') <- lift 
            {-$ flip runReaderT env
            $ runScannerEnvT-}
            $ flip runStateT st 
            $ runExceptT 
            $ runScannerT
            $ try
    case result of
        Right value -> do
            ScannerT $ lift $ put st'
            then_ value
        Left Abort -> abort
        Left Backtrack -> catch
        Left NotEnoughCharacters -> catch



