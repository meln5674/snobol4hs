{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Snobol4.Interpreter.Scanner.New.QuickScan where


import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Scanner.New.Types
import Language.Snobol4.Interpreter.Scanner.New.Common


-- | A quickscan path, nodes are annotated with an optional value, which will
-- later be filled with the minimum number of characters to match that node
type EmptyQuickScanPath expr = ScanPath expr (Maybe Snobol4Integer)

-- | A quickscan path, nodes are annotated with an optional value which has
-- been filled with the minimum number of characters to match that node
type AnnotatedQuickScanPath expr = ScanPath expr (Maybe Snobol4Integer)

-- | A complete quickscan path, nodes are annotated with a mandatory count
-- of the minimum number of characters required to match it
type QuickScanPath expr = ScanPath expr Snobol4Integer




-- | Annotate a fullscan path with absent optional values to create an incomplete
-- quickscan tree
makeQuickScanPath :: FullScanPath expr -> EmptyQuickScanPath expr
makeQuickScanPath (ScanConcat a b _) = ScanConcat (makeQuickScanPath a) (makeQuickScanPath b) Nothing
makeQuickScanPath (ScanChoice a b _) = ScanChoice (makeQuickScanPath a) (makeQuickScanPath b) Nothing
makeQuickScanPath (ScanAssign pat l _) = ScanAssign (makeQuickScanPath pat) l Nothing
makeQuickScanPath (ScanImmediateAssign pat l _) = ScanImmediateAssign (makeQuickScanPath pat) l Nothing
makeQuickScanPath (ScanArbNo pat _) = ScanArbNo (makeQuickScanPath pat) Nothing
makeQuickScanPath (ScanNode pat _) = ScanNode pat Nothing
makeQuickScanPath (UnevaluatedPattern expr) = UnevaluatedPattern expr 

-- | Get the minimum number of characters to match a leaf pattern
getPatternLength :: ScanPattern expr -> Snobol4Integer
getPatternLength (ScanLiteral s) = snobol4Length s
getPatternLength (ScanAny _) = 1
getPatternLength (ScanNotAny _) = 1
getPatternLength (ScanSpan _) = 1
getPatternLength (ScanBreak _) = 0
getPatternLength (ScanLen _) = 0
getPatternLength ScanRemainder = 0
getPatternLength (ScanHead _) = 0
getPatternLength (ScanTab _) = 0
getPatternLength (ScanRTab _) = 0
getPatternLength (ScanPos _) = 0
getPatternLength (ScanRPos _) = 0
getPatternLength ScanFail = 0
getPatternLength ScanFence = 0
getPatternLength ScanAbort = 0
getPatternLength ScanArb = 0
getPatternLength ScanBal = 0
getPatternLength ScanSucceed = 0

-- | Traverse an incomplete quickscan path, computing the minimum number of
-- characters necessary to match each node, and then annotate the node with that
-- value
getQuickScanLength :: EmptyQuickScanPath expr -> (AnnotatedQuickScanPath expr, Snobol4Integer)
getQuickScanLength x@(ScanConcat a b (Just count)) = (x, count)
getQuickScanLength (ScanConcat a b Nothing) =
    let (a', aCount) = getQuickScanLength a
        (b', bCount) = getQuickScanLength b
        count = aCount + bCount
    in  (ScanConcat a' b' (Just count), count)
getQuickScanLength x@(ScanChoice a b (Just count)) = (x,count)
getQuickScanLength (ScanChoice a b Nothing) =
    let (a', aCount) = getQuickScanLength a
        (b', bCount) = getQuickScanLength b
        count = min aCount bCount
    in  (ScanChoice a' b' (Just count), count)
getQuickScanLength x@(ScanAssign pat l (Just count)) = (x, count)
getQuickScanLength (ScanAssign pat l Nothing) =
    let (pat', count) = getQuickScanLength pat
    in  (ScanAssign pat' l (Just count), count)
getQuickScanLength x@(ScanImmediateAssign pat l (Just count)) = (x, count)
getQuickScanLength (ScanImmediateAssign pat l Nothing) =
    let (pat', count) = getQuickScanLength pat
    in  (ScanImmediateAssign pat' l (Just count), count)
getQuickScanLength x@(ScanArbNo pat (Just count)) = (x, count)
getQuickScanLength (ScanArbNo pat Nothing) =
    let (pat', count) = getQuickScanLength pat
    in  (ScanArbNo pat' (Just 0), 0)
getQuickScanLength x@(ScanNode pat (Just count)) = (x, count)
getQuickScanLength (ScanNode pat Nothing) = 
    let count = getPatternLength pat
    in  (ScanNode pat (Just count), count)
getQuickScanLength x@(UnevaluatedPattern expr) = (x, 1)

getQuickScanLength' :: QuickScanPath expr -> Snobol4Integer
getQuickScanLength' (ScanConcat _ _ count) = count
getQuickScanLength' (ScanChoice _ _ count) = count
getQuickScanLength' (ScanAssign _ _ count) = count
getQuickScanLength' (ScanImmediateAssign _ _ count) = count
getQuickScanLength' (ScanArbNo _ count) = count
getQuickScanLength' (ScanNode _ count) = count
getQuickScanLength' (UnevaluatedPattern _) = 1

-- | Traverse an annotated quickscan tree and replace the optional counts with
-- mandatory ones
labelQuickScanPath :: AnnotatedQuickScanPath expr -> QuickScanPath expr
labelQuickScanPath (ScanConcat a b (Just count)) =
    let a' = labelQuickScanPath a
        b' = labelQuickScanPath b
    in  ScanConcat a' b' count
labelQuickScanPath (ScanChoice a b (Just count)) =
    let a' = labelQuickScanPath a
        b' = labelQuickScanPath b
    in  ScanChoice a' b' count
labelQuickScanPath (ScanAssign pat l (Just count)) = 
    let pat' = labelQuickScanPath pat
    in  ScanAssign pat' l count
labelQuickScanPath (ScanImmediateAssign pat l (Just count)) = 
    let pat' = labelQuickScanPath pat
    in  ScanImmediateAssign pat' l count
labelQuickScanPath (ScanArbNo pat (Just count)) = 
    let pat' = labelQuickScanPath pat
    in  ScanArbNo pat' count
labelQuickScanPath (ScanNode pat (Just count)) = ScanNode pat count
labelQuickScanPath (UnevaluatedPattern expr) = UnevaluatedPattern expr 
labelQuickScanPath DeadNode = DeadNode
labelQuickScanPath x@(ScanConcat _ _ Nothing) = error "ScanConcat"
labelQuickScanPath x@(ScanChoice _ _ Nothing) = error "ScanChoice"
labelQuickScanPath x@(ScanAssign _ _ Nothing) = error "ScanAssign"
labelQuickScanPath x@(ScanImmediateAssign _ _ Nothing) = error "ScanImmediateAssign"
labelQuickScanPath x@(ScanArbNo _ Nothing) = error "ScanArbNo"
labelQuickScanPath x@(ScanNode _ Nothing) = error "ScanNode"

-- | Traverse a quickscan tree looking for unevaluated patterns. Evaluate them
-- and then build quickscan trees from them. If a tree would fit in the character
-- limits, replace the unevaluated node with it. If a tree would not fit, replace
-- the unevaluated node with a dead node
expandQuickScanPath :: ResolveClass expr m
                    => Snobol4Integer
                    -> QuickScanPath expr 
                    -> {-ScannerEnvT expr-} m (QuickScanPath expr, Snobol4Integer)
expandQuickScanPath limit (ScanConcat a b count) = do
    --(b', bCount') <- expandQuickScanPath limit b
    let b' = b
        bCount' = getQuickScanLength' b
    if bCount' <= limit
        then do
            (a', aCount') <- expandQuickScanPath (limit - bCount') a
            let count' = aCount' + bCount'
            if count' <= limit
                then return (ScanConcat a' b' count', count')
                else return (DeadNode, count')
        else return (DeadNode, bCount')
expandQuickScanPath limit (ScanChoice a b count) = do
    (a', aCount') <- expandQuickScanPath limit a
    (b', bCount') <- expandQuickScanPath limit b
    let count' = min aCount' bCount'
    if count' <= limit
        then return (ScanChoice a' b' count', count')
        else return (DeadNode, count')
expandQuickScanPath limit (ScanAssign pat l count) = do
    (pat', count') <- expandQuickScanPath limit pat
    if count' <= limit
        then return (ScanAssign pat' l count, count)
        else return (DeadNode, count')
expandQuickScanPath limit (ScanImmediateAssign pat l count) = do
    (pat', count') <- expandQuickScanPath limit pat
    if count' <= limit
        then return (ScanImmediateAssign pat' l count, count)
        else return (DeadNode, count')
expandQuickScanPath limit (ScanArbNo pat count) = do
    (pat', count') <- expandQuickScanPath limit pat
    if count' <= limit
        then return (ScanArbNo pat' count', count')
        else return (DeadNode, count')
expandQuickScanPath limit (ScanNode pat count) = return (ScanNode pat count, count)
expandQuickScanPath limit (UnevaluatedPattern expr) = do
    pat <- resolvePattern expr
    let path = fmap (getQuickScanLength . makeQuickScanPath . buildFullScanPath) pat
    case path of
        Nothing -> return (DeadNode, 0)
        Just (ScanNode{}, count) -> return (UnevaluatedPattern expr, 1)
        Just (fullPath, count)
            | count <= limit -> do
                let quickPath = labelQuickScanPath fullPath
                expandQuickScanPath limit quickPath
            | otherwise -> return (DeadNode, count)


-- | Match a leaf pattern
matchQuickScanPattern :: (ResolveClass expr m)
                      => ScanPattern expr
                      -> ScannerContT expr m Snobol4String
matchQuickScanPattern (ScanLiteral toConsume) next = consumeString toConsume >> next
matchQuickScanPattern (ScanAny thunk) next = withLazyStr thunk $ 
    \allowedChars -> consumeAny allowedChars >> next
matchQuickScanPattern (ScanNotAny thunk) next = withLazyStr thunk $ 
    \disallowedChars -> consumeNotAny disallowedChars >> next
matchQuickScanPattern (ScanSpan thunk) next = withLazyStr thunk $
    \allowedChars -> many1 (consumeAny allowedChars) next
matchQuickScanPattern (ScanBreak thunk) next = do
    -- BREAK must hit a break character, if there are no more remaining
    -- characters, this cannot happen
    toMatch <- getToMatch
    when (snobol4Null toMatch) backtrack
    withLazyStr thunk $
        \disallowedChars -> many (consumeNotAny disallowedChars) $ do
            -- BREAK must hit a break character, if the entire input was read,
            -- this didn't happen
            toMatch' <- getToMatch
            when (snobol4Null toMatch') backtrack
            next
matchQuickScanPattern (ScanLen thunk) next = withLazyInt thunk $
    \len -> consumeN len >> next
matchQuickScanPattern ScanRemainder next = getToMatch >>= consumeString >> next
matchQuickScanPattern (ScanHead l) next = do
    matched <- getMatched
    offset <- getOffset
    immediateAssign l $ IntegerData $ offset + snobol4Length matched
    putPrevMatch ""
    next
matchQuickScanPattern (ScanTab thunk) next = withLazyInt thunk $ \col -> do
    offset <- getOffset
    matched <- getMatched
    let len = col - (offset + snobol4Length matched)
    if 0 <= len
        then consumeN len >> next
        else backtrack
matchQuickScanPattern (ScanRTab thunk) next = withLazyInt thunk $ \rcol -> do
    toMatch <- getToMatch
    let len = snobol4Length toMatch - rcol
    if 0 <= len
        then consumeN len >> next
        else backtrack
matchQuickScanPattern (ScanPos thunk) next = withLazyInt thunk $ \col -> do
    offset <- getOffset
    matched <- getMatched
    let len = col - (offset + snobol4Length matched)
    if len == 0
        then do
            putPrevMatch ""
            next
        else backtrack
matchQuickScanPattern (ScanRPos thunk) next = withLazyInt thunk $ \rcol -> do
    toMatch <- getToMatch
    let len = snobol4Length toMatch - rcol
    if len == 0
        then do
            putPrevMatch ""
            next
        else backtrack
matchQuickScanPattern ScanFail _ = backtrack
matchQuickScanPattern ScanFence next = catchScan next abort
matchQuickScanPattern ScanAbort _ = abort
matchQuickScanPattern ScanArb next = go 0
  where
    go n = catchQuickScanArb (consumeN n >> next) (go (n+1))
matchQuickScanPattern ScanBal next = undefined
matchQuickScanPattern ScanSucceed next = 
    catchScan (consumeN 0 >> next) 
                   (matchQuickScanPattern ScanSucceed next)

catchQuickScanArb :: ResolveClass expr m
                  => ScannerT expr m a
                  -> ScannerT expr m a
                  -> ScannerT expr m a
catchQuickScanArb try catch = do
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
        Left NotEnoughCharacters -> notEnoughCharacters

-- | Match a branch pattern
runQuickScan :: forall expr m 
              . (ResolveClass expr m)
             => QuickScanPath expr
             -> ScannerContT expr m Snobol4String
{-
runQuickScan (ScanConcat a@(UnevaluatedPattern expr) b count) next = getToMatch >>= \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> do
        let bCount = getQuickScanLength' b
            limit = snobol4Length toMatch
        (a', aCount') <- expandQuickScanPath (limit - bCount) a
        if limit < aCount'
            then notEnoughCharacters
            else do
                let count' = aCount' + bCount
                runQuickScan (ScanConcat a' b count') next
      | otherwise -> notEnoughCharacters
-}
runQuickScan (ScanConcat a b count) next = getToMatch >>= \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> do
        runQuickScan a $ do
            prevMatch <- getPrevMatch
            runQuickScan b $ do
                prevMatch' <- getPrevMatch
                putPrevMatch (prevMatch <> prevMatch')
                next
      | otherwise -> notEnoughCharacters
runQuickScan (ScanChoice a b count) next = getToMatch >>= \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> catchScan (runQuickScan a next) (runQuickScan b next)
      | otherwise -> notEnoughCharacters
runQuickScan (ScanAssign pat l count) next = getToMatch >>= \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> do
        runQuickScan pat $ do
            prev <- getPrevMatch
            addAssignment l $ StringData prev
            next
      | otherwise -> notEnoughCharacters
runQuickScan (ScanImmediateAssign pat l count) next = getToMatch >>= \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> do
        runQuickScan pat $ do
            prev <- getPrevMatch
            immediateAssign l $ StringData prev
            next
      | otherwise -> notEnoughCharacters
runQuickScan (ScanArbNo pat count) next = getToMatch >>= \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> go 0
      | otherwise -> notEnoughCharacters
  where
    go :: Int -> ScannerT expr m Snobol4String
    go x = do
        catchQuickScanArb (go2 x) (go $ x+1)
      where
        go2 :: Int -> ScannerT expr m Snobol4String
        go2 0 = next
        go2 n = do
          prevMatch <- getPrevMatch 
          runQuickScan pat $ do
              prevMatch' <- getPrevMatch
              putPrevMatch $ (prevMatch <> prevMatch')
              go2 $ n-1
runQuickScan (ScanNode pat count) next = getToMatch >>= \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> matchQuickScanPattern pat next
      | otherwise -> notEnoughCharacters
runQuickScan (UnevaluatedPattern expr) next = do
    pat <- resolvePattern expr
    toMatch <- getToMatch
    let limit = snobol4Length toMatch
    let path = fmap (labelQuickScanPath . fst . getQuickScanLength . makeQuickScanPath . buildFullScanPath) pat
    case path of
        Just path{-@ScanNode{}-} -> do
            (path', _) <- expandQuickScanPath limit path
            runQuickScan path' next
        Nothing -> backtrack
        --Just path@_ -> error "AAAAAAA"
runQuickScan DeadNode _ = notEnoughCharacters


-- | Run the quick scanner, advancing the anchor as neccessary
runQuickScan' :: (ResolveClass expr m)
              => {-ScannerEnv expr m
              -> -}QuickScanPath expr
              -> Snobol4String
              -> Snobol4Integer
              -> Bool
              -> m (ScanResult expr)
runQuickScan' {-env-} pat toMatch offset anchorMode = do
    let st = ScannerState toMatch "" "" [] offset
        retry = case snobol4Uncons toMatch of
            Just (_,nextTry) -> if anchorMode
                then return NoScan
                else runQuickScan' {-env-} pat nextTry (offset + 1) anchorMode
            _ -> return NoScan
    (result, st') <- {-flip runReaderT env
            $ runScannerEnvT
            $-} flip runStateT st
            $ runExceptT 
            $ runScannerT
            $ runQuickScan pat succeed
    case result of
        Left Abort -> return NoScan
        Left Backtrack -> retry
        Left NotEnoughCharacters -> return NoScan
        Right _ -> return $ Scan (StringData $ matched st') 
                                 (assignments st') 
                                 offset 
                                 (offset + snobol4Length (matched st'))


-- | Build a quickscan tree from a pattern and then try to match it
quickscan :: (ResolveClass expr m)
          => Pattern expr
          -> Snobol4String
          -> Bool
          {--> (expr -> m (Maybe (Pattern expr)))
          -> (expr -> m (Maybe Snobol4Integer))
          -> (expr -> m (Maybe Snobol4String))
          -> (Lookup expr -> Data expr -> m ())-}
          -> m (ScanResult expr)
quickscan pat toMatch anchorMode {-toPat toInt toStr set-} = do
    let len = snobol4Length toMatch
        {-env = ScannerEnv anchorMode False toPat toInt toStr set-}
        fullPath = buildFullScanPath pat
        incompleteQuickPath = makeQuickScanPath fullPath
        (annotatedQuickPath, _) = getQuickScanLength incompleteQuickPath
        labeledQuickPath = labelQuickScanPath annotatedQuickPath
        path = labelQuickScanPath annotatedQuickPath
    (path, _) <- {-flip runReaderT env 
               $ runScannerEnvT
               $-} expandQuickScanPath len labeledQuickPath
    runQuickScan' {-env-} path toMatch 0 anchorMode
