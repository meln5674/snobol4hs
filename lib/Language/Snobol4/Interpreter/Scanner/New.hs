{-|
Module          : Language.Snobol4.Interpreter.Scanner.New
Description     : The SNOBOL4 Interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

Temporary home for the quickscan experiment.

The functions in this module make up the experimental quickscan scanner. This
    scanner uses hueristics to avoid attempting matches which are impossible.
    This also results in eliminating some cases of left-recursion.

Quickscanning is performed in the following steps:

1) Convert a pattern to a "Fullscan path", which separates out pattern
    combinators (such as |, ARBNO, etc) from leaves (such as literals, BREAK,
    etc). This allows the scanner to have more context about alternatives.
    Unevaluated patterns and parameters are left unevaulated.

2) Convert the Fullscan path to an incomplete quickscan path. A incomplete
    quickscan path is the same as a fullscan path, but each node is labled with
    an optional (initially absent) count of how many characters, at minimum, that
    node will require to be matched.

3) Traverse the incomplete quickscan path and compute the minimum characters for
    each node. 

4) Clean up the quickscan tree by converting the optional counters to mandatory
    ones, forming a complete quickscan tree.

5) Traverse the completed tree and search for unevaluated patterns. If one is
    found, evaluate it, and perform steps 1-4 to transform it into a quickscan
    path. If that path requires more characters than is allowed, replace the
    unevaluated pattern with a dead node which will always cause failure. If
    the path has an acceptable length, repeat this process to fully expand it.
    Once the path has been fully expanded, replace the unevaluated pattern with
    the new path.

6) The path is now ready for matching. Use a continuation passing scheme to
    descend into pattern, backtracking as necessary.
-}


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.Interpreter.Scanner.New where

import Prelude hiding (repeat)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import Language.Snobol4.Interpreter.Data

-- | A reason for scanner failure
data ScanFailure
    = 
    -- | The scanner encountered a dead end, and needs to try an alternative
    -- path
      Backtrack 
    -- | The scanner encountered an error it could not recover from
    | Abort 
    -- | The scanner ecnountered a dead end by finding a node that required too
    -- many characters
    | NotEnoughCharacters

-- | A "leaf" pattern, one that does not reference any other patterns and simply
-- matches a string
data ScanPattern expr
    = 
    -- | Matches the given string exactly
      ScanLiteral Snobol4String
    -- | Matches any one character in the given string
    | ScanAny (LazyString expr)
    -- | Matches any one character not in the given string
    | ScanNotAny (LazyString expr)
    -- | Matches the longest string containing only characters in the given string
    | ScanSpan (LazyString expr)
    -- | Matches the longest string containing none of the characters in the given string
    | ScanBreak (LazyString expr)
    -- | Matches the given number of characters
    | ScanLen (LazyInteger expr)
    -- | Matches rest of input
    | ScanRemainder
    -- | Matches null string, assigns cursor position to variable
    | ScanHead (Lookup expr)
    | ScanTab (LazyInteger expr)
    | ScanRTab (LazyInteger expr)
    | ScanPos (LazyInteger expr)
    | ScanRPos (LazyInteger expr)
    | ScanFail
    | ScanFence
    | ScanAbort
    | ScanArb
    | ScanBal
    | ScanSucceed
  deriving Show

-- | A "branch" battern, one that is made up of one or more other patterns
-- along with an annotation
data ScanPath expr ann
    = 
    -- | Matches the first pattern, then the second
      ScanConcat (ScanPath expr ann) (ScanPath expr ann) ann
    -- | Matches the first pattern, if it fails, it matches the second
    | ScanChoice (ScanPath expr ann) (ScanPath expr ann) ann
    -- | Matches the given pattern, then marks the matched string to be assigned
    -- to the given variable once scanning is complete
    | ScanAssign (ScanPath expr ann) (Lookup expr) ann
    -- Matches the given pattern, then immediately assigns the matched string to
    -- the given variable
    | ScanImmediateAssign (ScanPath expr ann) (Lookup expr) ann
    -- | Matches the given pattern any number of times
    | ScanArbNo (ScanPath expr ann) ann
    -- | Matches the given leaf pattern
    | ScanNode (ScanPattern expr) ann
    -- | An pattern which has not yet been evaluated
    | UnevaluatedPattern expr
    -- | A dead node, if encountered, the scanner should fail
    | DeadNode
  deriving Show

-- | A fullscan path, nodes have no annotation
type FullScanPath expr = ScanPath expr ()

-- | A quickscan path, nodes are annotated with an optional value, which will
-- later be filled with the minimum number of characters to match that node
type EmptyQuickScanPath expr = ScanPath expr (Maybe Snobol4Integer)

-- | A quickscan path, nodes are annotated with an optional value which has
-- been filled with the minimum number of characters to match that node
type AnnotatedQuickScanPath expr = ScanPath expr (Maybe Snobol4Integer)

-- | A complete quickscan path, nodes are annotated with a mandatory count
-- of the minimum number of characters required to match it
type QuickScanPath expr = ScanPath expr Snobol4Integer

data ScannerState expr = ScannerState
    { toMatch :: Snobol4String
    , matched :: Snobol4String
    , prevMatch :: Snobol4String
    , assignments :: [(Lookup expr, Data expr)]
    , offset :: Snobol4Integer
    }

data ScannerEnv expr m = ScannerEnv
    { anchorMode :: Bool
    , fullscanMode :: Bool
    , resolvePatternFunc :: expr -> m (Maybe (Pattern expr))
    , resolveIntegerFunc :: expr -> m (Maybe Snobol4Integer)
    , resolveStringFunc :: expr -> m (Maybe Snobol4String)
    , assignFunc :: Lookup expr -> Data expr -> m ()
    }

newtype ScannerEnvT expr m a = ScannerEnvT
    { runScannerEnvT :: ReaderT (ScannerEnv expr m) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (ScannerEnvT expr) where
    lift = ScannerEnvT . lift

newtype ScannerT expr m a = ScannerT
    { runScannerT :: ExceptT ScanFailure 
                    (StateT (ScannerState expr)
                    (ScannerEnvT expr
                     m)) a 
    }
  deriving (Functor, Applicative, Monad, MonadIO)

getToMatch :: Monad m => ScannerT expr m Snobol4String
getToMatch = ScannerT $ lift $ gets toMatch

putToMatch :: Monad m => Snobol4String -> ScannerT expr m ()
putToMatch x = ScannerT $ lift $ modify $ \st -> st { toMatch = x}

getMatched :: Monad m => ScannerT expr m Snobol4String
getMatched = ScannerT $ lift $ gets matched

putMatched :: Monad m => Snobol4String -> ScannerT expr m ()
putMatched x = ScannerT $ lift $ modify $ \st -> st { matched = x}

getPrevMatch :: Monad m => ScannerT expr m Snobol4String
getPrevMatch = ScannerT $ lift $ gets prevMatch

putPrevMatch :: Monad m => Snobol4String -> ScannerT expr m ()
putPrevMatch x = ScannerT $ lift $ modify $ \st -> st { prevMatch = x}

addAssignment :: Monad m => Lookup expr -> Data expr -> ScannerT expr m ()
addAssignment l x = ScannerT $ lift $ modify $ \st -> st { assignments = (l,x) : assignments st }

getOffset :: Monad m => ScannerT expr m Snobol4Integer
getOffset = ScannerT $ lift $ gets offset

incOffset :: Monad m => ScannerT expr m ()
incOffset = ScannerT $ lift $ modify $ \st -> st { offset = 1 + offset st }

instance MonadTrans (ScannerT expr) where
    lift = ScannerT . lift . lift . lift


class Monad m => ResolveClass m where
    type Resolvable m
    resolvePattern :: Resolvable m -> m (Maybe (Pattern (Resolvable m)))
    resolveInteger :: Resolvable m -> m (Maybe Snobol4Integer)
    resolveString :: Resolvable m -> m (Maybe Snobol4String)
    assign :: Lookup (Resolvable m) -> Data (Resolvable m) -> m ()

instance Monad m => ResolveClass (ScannerEnvT expr m) where
    type Resolvable (ScannerEnvT expr m) = expr
    resolvePattern expr = (ScannerEnvT $ asks resolvePatternFunc) >>= lift . ($ expr)
    resolveInteger expr = (ScannerEnvT $ asks resolveIntegerFunc) >>= lift . ($ expr)
    resolveString expr = (ScannerEnvT $ asks resolveStringFunc) >>= lift . ($ expr)
    assign l x = (ScannerEnvT $ asks assignFunc) >>= lift . ($ (l,x)) . uncurry

instance Monad m => ResolveClass (ScannerT expr m) where
    type Resolvable (ScannerT expr m) = expr
    resolvePattern = ScannerT . lift . lift . resolvePattern
    resolveInteger = ScannerT . lift . lift . resolveInteger
    resolveString = ScannerT . lift . lift . resolveString
    assign l = ScannerT . lift . lift . assign l


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
    in  (ScanAssign pat l (Just count), count)
getQuickScanLength x@(ScanImmediateAssign pat l (Just count)) = (x, count)
getQuickScanLength (ScanImmediateAssign pat l Nothing) =
    let (pat', count) = getQuickScanLength pat
    in  (ScanImmediateAssign pat l (Just count), count)
getQuickScanLength (ScanNode pat Nothing) = 
    let count = getPatternLength pat
    in  (ScanNode pat (Just count), count)
getQuickScanLength x@(ScanArbNo pat (Just count)) = (x, count)
getQuickScanLength (ScanArbNo pat Nothing) =
    let (pat', count) = getQuickScanLength pat
    in  (ScanArbNo pat' (Just 0), 0)
getQuickScanLength x@(ScanNode pat (Just count)) = (x, count)
getQuickScanLength x@(UnevaluatedPattern expr) = (x, 1)

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

-- | Traverse a quickscan tree looking for unevaluated patterns. Evaluate them
-- and then build quickscan trees from them. If a tree would fit in the character
-- limits, replace the unevaluated node with it. If a tree would not fit, replace
-- the unevaluated node with a dead node
expandQuickScanPath :: Monad m
                    => Snobol4Integer
                    -> QuickScanPath expr 
                    -> ScannerEnvT expr m (QuickScanPath expr, Snobol4Integer)
expandQuickScanPath limit (ScanConcat a b count) = do
    (b', bCount') <- expandQuickScanPath limit b
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
        then return (ScanAssign pat l count, count)
        else return (DeadNode, count')
expandQuickScanPath limit (ScanImmediateAssign pat l count) = do
    (pat', count') <- expandQuickScanPath limit pat
    if count' <= limit
        then return (ScanImmediateAssign pat l count, count)
        else return (DeadNode, count')
expandQuickScanPath limit (ScanArbNo pat count) = do
    (pat', count') <- expandQuickScanPath limit pat
    if count' <= limit
        then return (ScanArbNo pat count', count')
        else return (DeadNode, count')
expandQuickScanPath limit (ScanNode pat count) = return (ScanNode pat count, count)
expandQuickScanPath limit (UnevaluatedPattern expr) = do
    pat <- resolvePattern expr
    case pat of
        Nothing -> return (DeadNode, 0)
        Just pat -> do
            let (fullPath, count) = getQuickScanLength $ makeQuickScanPath $ buildFullScanPath pat
            if count <= limit
                then do
                    let quickPath = labelQuickScanPath fullPath
                    expandQuickScanPath limit quickPath
                else
                    return (DeadNode, count)



-- | Continuation monad for the quickscanner
type QuickScanCont expr m a = ScannerT expr m a -> ScannerT expr m a

-- | Attempt to consume a string
consumeString :: (Monad m)
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

consumeN :: Monad m
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

consumeAny :: (Monad m)
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

consumeNotAny :: (Monad m)
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


many1 :: (Monad m)
      => ScannerT expr m a
      -> QuickScanCont expr m b
many1 f next = f >> go
  where
    go = do
        prevMatch <- getPrevMatch
        catchQuickScan' f 
                        (\_ -> do { prevMatch' <- getPrevMatch
                                  ; putPrevMatch (prevMatch <> prevMatch')
                                  ; go
                                  }
                        )
                        next
many :: (Monad m)
      => ScannerT expr m a
      -> QuickScanCont expr m b
many f next = go
  where
    go = do
        prevMatch <- getPrevMatch
        catchQuickScan' f 
                        (\_ -> do { prevMatch' <- getPrevMatch
                                  ; putPrevMatch (prevMatch <> prevMatch')
                                  ; go
                                  }
                        )
                        next

repeat :: (Monad m)
       => Snobol4Integer
       -> ScannerT expr m a
       -> QuickScanCont expr m b
repeat n f next = putPrevMatch "" >> go n
  where
   go n | n <= 0 = next
        | otherwise = do
            prevMatch <- getPrevMatch
            f
            prevMatch' <- getPrevMatch
            putPrevMatch (prevMatch <> prevMatch')
            go (n-1)

{-
withToMatch :: (Snobol4String -> QuickScanContPrim expr m a) -> QuickScanContPrim expr m a
withToMatch f = \toPat toInt toStr toMatch matched prevMatch -> f toMatch toPat toInt toStr toMatch matched prevMatch

withMatched :: (Snobol4String -> QuickScanContPrim expr m a) -> QuickScanContPrim expr m a
withMatched f = \toPat toInt toStr toMatch matched prevMatch -> f matched toPat toInt toStr toMatch matched prevMatch

withToStr :: ((expr -> m Snobol4String) -> QuickScanContPrim expr m a) -> QuickScanContPrim expr m a
withToStr f = \toPat toInt toStr toMatch matched prevMatch -> f toStr toPat toInt toStr toMatch matched prevMatch
-}

withLazyStr :: Monad m 
            => (LazyString expr) 
            -> (Snobol4String -> ScannerT expr m a) 
            -> ScannerT expr m a
withLazyStr (Thunk expr) f = do
    s <- resolveString expr
    case s of
        Nothing -> backtrack
        Just s -> f s
withLazyStr (EvaluatedThunk s) f = f s


withLazyInt :: Monad m 
            => (LazyInteger expr) 
            -> (Snobol4Integer -> ScannerT expr m a) 
            -> ScannerT expr m a
withLazyInt (Thunk expr) f = do
    s <- resolveInteger expr
    case s of
        Nothing -> backtrack
        Just s -> f s
withLazyInt (EvaluatedThunk s) f = f s


-- | Match a leaf pattern
matchQuickScanPattern :: (Monad m)
                      => ScanPattern expr
                      -> QuickScanCont expr m Snobol4String
matchQuickScanPattern (ScanLiteral toConsume) next = consumeString toConsume >> next
matchQuickScanPattern (ScanAny thunk) next = withLazyStr thunk $ 
    \allowedChars -> consumeAny allowedChars >> next
matchQuickScanPattern (ScanNotAny thunk) next = withLazyStr thunk $ 
    \disallowedChars -> consumeNotAny disallowedChars >> next
matchQuickScanPattern (ScanSpan thunk) next = withLazyStr thunk $
    \allowedChars -> many1 (consumeAny allowedChars) next
matchQuickScanPattern (ScanBreak thunk) next = withLazyStr thunk $
    \disallowedChars -> many (consumeNotAny disallowedChars) next
matchQuickScanPattern (ScanLen thunk) next = withLazyInt thunk $
    \len -> consumeN len >> next
matchQuickScanPattern ScanRemainder next = getToMatch >>= consumeString >> next
matchQuickScanPattern (ScanHead l) next = do
    toMatch <- getToMatch
    assign l $ IntegerData $ snobol4Length toMatch
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
matchQuickScanPattern ScanFence next = catchQuickScan next abort
matchQuickScanPattern ScanAbort _ = abort
matchQuickScanPattern ScanArb next = go 0
  where
    go n = catchQuickScanArb (consumeN n >> next) (go (n+1))
matchQuickScanPattern ScanBal next = undefined
matchQuickScanPattern ScanSucceed next = 
    catchQuickScan (consumeN 0 >> next) 
                   (matchQuickScanPattern ScanSucceed next)

{-
-- | Perform an action that ignores the passed values
ignoreCont :: (Monad m) => ExceptT ScanFailure m a -> QuickScanCont expr m a
ignoreCont f _ _ _ _ _ _ = f
-}

failScan :: Monad m => ScanFailure -> ScannerT expr m a
failScan = ScannerT . throwE

-- | Backtrack to the nearest checkpoint
backtrack :: (Monad m) => ScannerT expr m a
backtrack = failScan Backtrack

-- | Abort the scan
abort :: (Monad m) => ScannerT expr m a
abort = failScan Abort

-- | Backtrack to the nearest checkpoint, noting that there were not enough
-- characters to match the pattern
notEnoughCharacters :: Monad m => ScannerT expr m a
notEnoughCharacters = failScan NotEnoughCharacters

-- | Complete the scan with success
succeed :: Monad m => ScannerT expr m Snobol4String
succeed = getMatched

-- | Perform an action, with a backup contination to take if it fails
catchQuickScan :: (Monad m)
               => ScannerT expr m a -- ^ Action to attempt
               -> ScannerT expr m a -- ^ Action to take if it fails
               -> ScannerT expr m a
catchQuickScan try catch = do
    st <- ScannerT $ lift get
    env <- ScannerT $ lift $ lift $ ScannerEnvT $ ask
    (result, st') <- lift 
            $ flip runReaderT env
            $ runScannerEnvT
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

catchQuickScanArb :: Monad m
                  => ScannerT expr m a
                  -> ScannerT expr m a
                  -> ScannerT expr m a
catchQuickScanArb try catch = do
    st <- ScannerT $ lift get
    env <- ScannerT $ lift $ lift $ ScannerEnvT $ ask
    (result, st') <- lift 
            $ flip runReaderT env
            $ runScannerEnvT
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

catchQuickScan' :: (Monad m)
               => ScannerT expr m a -- ^ Action to attempt
               -> (a -> ScannerT expr m b) -- ^ Action to take if successful 
               -> ScannerT expr m b -- ^ Action to take if it fails
               -> ScannerT expr m b
catchQuickScan' try then_ catch = do
    st <- ScannerT $ lift get
    env <- ScannerT $ lift $ lift $ ScannerEnvT $ ask
    (result, st') <- lift 
            $ flip runReaderT env
            $ runScannerEnvT
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



-- | Match a branch pattern
runQuickScan :: (Monad m)
             => QuickScanPath expr
             -> QuickScanCont expr m Snobol4String
runQuickScan (ScanConcat a b count) next = getToMatch >>= \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> runQuickScan a (runQuickScan b next)
      | otherwise -> notEnoughCharacters
runQuickScan (ScanChoice a b count) next = getToMatch >>= \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> catchQuickScan (runQuickScan a next) (runQuickScan b next)
      | otherwise -> notEnoughCharacters
runQuickScan (ScanImmediateAssign pat l count) next = getToMatch >>= \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> do
        runQuickScan pat $ do
            prev <- getPrevMatch
            assign l $ StringData prev
            next
      | otherwise -> notEnoughCharacters
runQuickScan (ScanArbNo pat count) next = getToMatch >>= \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> go 0
      | otherwise -> notEnoughCharacters
  where
    go x = do
        catchQuickScanArb (go2 x) (go $ x+1)
      where
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
runQuickScan (UnevaluatedPattern _) _ = error "Found an unevaluated pattern in quick scan mode"
runQuickScan DeadNode _ = notEnoughCharacters


-- | Run the quick scanner, advancing the anchor as neccessary
runQuickScan' :: (Monad m)
              => ScannerEnv expr m
              -> QuickScanPath expr
              -> Snobol4String
              -> Snobol4Integer
              -> Bool
              -> m (Maybe Snobol4String)
runQuickScan' env pat toMatch offset anchorMode = do
    let st = ScannerState toMatch "" "" [] offset
    (result, st') <- flip runReaderT env
            $ runScannerEnvT
            $ flip runStateT st
            $ runExceptT 
            $ runScannerT
            $ runQuickScan pat succeed
    case result of
        Left Abort -> return Nothing
        Left Backtrack -> do
            case snobol4Uncons toMatch of
                Just (_,nextTry) -> if anchorMode
                    then return Nothing
                    else runQuickScan' env pat nextTry (offset + 1) anchorMode
                _ -> return Nothing
        Left NotEnoughCharacters -> return Nothing
        Right matched -> return $ Just matched


-- | Build a quickscan tree from a pattern and then try to match it
quickscan' :: (Monad m)
          => Pattern expr
          -> Snobol4String
          -> Bool
          -> (expr -> m (Maybe (Pattern expr)))
          -> (expr -> m (Maybe Snobol4Integer))
          -> (expr -> m (Maybe Snobol4String))
          -> (Lookup expr -> Data expr -> m ())
          -> m (Maybe Snobol4String)
quickscan' pat toMatch anchorMode toPat toInt toStr set = do
    let len = snobol4Length toMatch
        env = ScannerEnv anchorMode False toPat toInt toStr set
        fullPath = buildFullScanPath pat
        incompleteQuickPath = makeQuickScanPath fullPath
        (annotatedQuickPath, _) = getQuickScanLength incompleteQuickPath
        labeledQuickPath = labelQuickScanPath annotatedQuickPath
    (path, _) <- flip runReaderT env 
               $ runScannerEnvT 
               $ expandQuickScanPath len labeledQuickPath
    runQuickScan' env path toMatch 0 anchorMode
