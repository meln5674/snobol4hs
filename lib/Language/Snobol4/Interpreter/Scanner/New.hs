{-# LANGUAGE LambdaCase #-}
module Language.Snobol4.Interpreter.Scanner.New where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Data

data ScanFailure = Backtrack | Abort | NotEnoughCharacters

data ScanPattern expr
    = ScanLiteral Snobol4String
    | ScanAny (LazyString expr)
    | ScanNotAny (LazyString expr)
    | ScanScan (LazyString expr)
    | ScanBreak (LazyString expr)
    | ScanLen (LazyString expr)
    -- ...
  deriving Show

data ScanPath expr count
    = ScanConcat (ScanPath expr count) (ScanPath expr count) count
    | ScanChoice (ScanPath expr count) (ScanPath expr count) count
    -- | ScanAssign (ScanPath expr count) count lookup
    -- | ScanImmediateAssign (ScanPath expr count) lookup
    | ScanNode (ScanPattern expr) count
    | UnevaluatedPattern expr
    | DeadNode
  deriving Show

type FullScanPath expr = ScanPath expr ()
type IncompleteQuickScanPath expr = ScanPath expr (Maybe Snobol4Integer)
type QuickScanPath expr = ScanPath expr Snobol4Integer

buildLazy f _ (Thunk expr) = f expr
buildLazy _ g (EvaluatedThunk x) = g x

buildFullScanPath :: Pattern expr -> FullScanPath expr
buildFullScanPath (AssignmentPattern pat l) = undefined
buildFullScanPath (ImmediateAssignmentPattern pat l) = undefined
buildFullScanPath (LiteralPattern s) = ScanNode (ScanLiteral s) ()
buildFullScanPath (AlternativePattern a b) = 
    let a' = buildLazy UnevaluatedPattern buildFullScanPath a
        b' = buildLazy UnevaluatedPattern buildFullScanPath b
    in  ScanChoice a' b' ()
buildFullScanPath (ConcatPattern a b) =
    let a' = buildLazy UnevaluatedPattern buildFullScanPath a
        b' = buildLazy UnevaluatedPattern buildFullScanPath b
    in  ScanConcat a' b' ()
-- ...


makeQuickScanPath :: FullScanPath expr -> IncompleteQuickScanPath expr
makeQuickScanPath (ScanConcat a b _) = ScanConcat (makeQuickScanPath a) (makeQuickScanPath b) Nothing
makeQuickScanPath (ScanChoice a b _) = ScanChoice (makeQuickScanPath a) (makeQuickScanPath b) Nothing
makeQuickScanPath (ScanNode pat _) = ScanNode pat Nothing
makeQuickScanPath (UnevaluatedPattern expr) = UnevaluatedPattern expr 

getPatternLength :: ScanPattern expr -> Snobol4Integer
getPatternLength (ScanLiteral s) = snobol4Length s
getPatternLength _ = undefined

getQuickScanLength :: IncompleteQuickScanPath expr -> (IncompleteQuickScanPath expr, Snobol4Integer)
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
getQuickScanLength (ScanNode pat Nothing) = 
    let count = getPatternLength pat
    in  (ScanNode pat (Just count), count)
getQuickScanLength x@(ScanNode pat (Just count)) = (x, count)
getQuickScanLength x@(UnevaluatedPattern expr) = (x, 1)

labelQuickScanPath :: IncompleteQuickScanPath expr -> QuickScanPath expr
labelQuickScanPath (ScanConcat a b (Just count)) =
    let a' = labelQuickScanPath a
        b' = labelQuickScanPath b
    in  ScanConcat a' b' count
labelQuickScanPath (ScanChoice a b (Just count)) =
    let a' = labelQuickScanPath a
        b' = labelQuickScanPath b
    in  ScanChoice a' b' count
labelQuickScanPath (ScanNode pat (Just count)) = ScanNode pat count
labelQuickScanPath (UnevaluatedPattern expr) = UnevaluatedPattern expr 

expandQuickScanPath :: Monad m
                    => (expr -> m (Pattern expr))
                    -> (expr -> m Snobol4Integer)
                    -> (expr -> m Snobol4String)
                    -> Snobol4Integer
                    -> QuickScanPath expr 
                    -> m (QuickScanPath expr, Snobol4Integer)
expandQuickScanPath toPat toInt toStr limit (ScanConcat a b count) = do
    (b', bCount') <- expandQuickScanPath toPat toInt toStr limit b
    if bCount' <= limit
        then do
            (a', aCount') <- expandQuickScanPath toPat toInt toStr (limit - bCount') a
            let count' = aCount' + bCount'
            if count' <= limit
                then return (ScanConcat a' b' count', count')
                else return (DeadNode, count')
        else return (DeadNode, bCount')
expandQuickScanPath toPat toInt toStr limit (ScanChoice a b count) = do
    (a', aCount') <- expandQuickScanPath toPat toInt toStr limit a
    (b', bCount') <- expandQuickScanPath toPat toInt toStr limit b
    let count' = min aCount' bCount'
    if count' <= limit
        then return (ScanChoice a' b' count', count')
        else return (DeadNode, count')
expandQuickScanPath toPat toInt toStr limit (ScanNode pat count) = return (ScanNode pat count, count)
expandQuickScanPath toPat toInt toStr limit (UnevaluatedPattern expr) = do
    pat <- toPat expr
    let (fullPath, count) = getQuickScanLength $ makeQuickScanPath $ buildFullScanPath pat
    if count <= limit
        then do
            let quickPath = labelQuickScanPath fullPath
            expandQuickScanPath toPat toInt toStr limit quickPath
        else
            return (DeadNode, count)

type QuickScanContPrim expr m a
    =  (expr -> m (Pattern expr)) -- ^ Pattern resolver
    -> (expr -> m Snobol4Integer) -- ^ Integer resolver
    -> (expr -> m Snobol4String)  -- ^ String resolver
    -> Snobol4String -- ^ String to be matched
    -> Snobol4String -- ^ String matched so far
    -> Snobol4String -- ^ String matched by previous pattern
    -> a

type QuickScanCont expr m a = QuickScanContPrim expr m (ExceptT ScanFailure m a)


consumeString :: Snobol4String -> QuickScanCont expr m a -> QuickScanCont expr m a
consumeString newlyMatched next = \toPat toInt toStr toMatch matched prevMatch -> do
    let toMatch' = (snobol4Drop (snobol4Length newlyMatched) toMatch)
        matched' = matched <> newlyMatched
    next toPat toInt toStr toMatch' matched' prevMatch

withToMatch :: (Snobol4String -> QuickScanContPrim expr m a) -> QuickScanContPrim expr m a
withToMatch f = \toPat toInt toStr toMatch matched prevMatch -> f toMatch toPat toInt toStr toMatch matched prevMatch

withMatched :: (Snobol4String -> QuickScanContPrim expr m a) -> QuickScanContPrim expr m a
withMatched f = \toPat toInt toStr toMatch matched prevMatch -> f matched toPat toInt toStr toMatch matched prevMatch

matchQuickScanPattern :: (Monad m)
                      => ScanPattern expr
                      -> QuickScanCont expr m Snobol4String
                      -> QuickScanCont expr m Snobol4String
matchQuickScanPattern (ScanLiteral toConsume) next = withToMatch $ \toMatch -> do
    let toBeConsumed = snobol4Take (snobol4Length toConsume) toMatch
    if toConsume == toBeConsumed
        then consumeString toConsume next
        else backtrack
    
matchQuickScanPattern _ next = undefined

ignoreCont :: (Monad m) => ExceptT ScanFailure m a -> QuickScanCont expr m a
ignoreCont f _ _ _ _ _ _ = f

backtrack :: (Monad m) => QuickScanCont expr m a
backtrack = ignoreCont $ throwE Backtrack

abort :: (Monad m) => QuickScanCont expr m a
abort = ignoreCont $ throwE Abort

notEnoughCharacters :: (Monad m) => QuickScanCont expr m a
notEnoughCharacters = ignoreCont $ throwE NotEnoughCharacters

succeed :: (Monad m) => QuickScanCont expr m Snobol4String
succeed = withMatched $ \matched -> ignoreCont $ return matched

catchQuickScan :: (Monad m)
               => QuickScanCont expr m Snobol4String
               -> QuickScanCont expr m Snobol4String
               -> QuickScanCont expr m Snobol4String
catchQuickScan a b = \toPat toInt toStr toMatch matched prevMatch -> do
    catchE (a toPat toInt toStr toMatch matched prevMatch) $ \case
        Abort -> abort toPat toInt toStr toMatch matched prevMatch
        Backtrack -> b toPat toInt toStr toMatch matched prevMatch
        NotEnoughCharacters -> b toPat toInt toStr toMatch matched prevMatch

runQuickScan :: (Monad m)
             => QuickScanPath expr 
             -> QuickScanCont expr m Snobol4String
             -> QuickScanCont expr m Snobol4String
runQuickScan (ScanConcat a b count) next = withToMatch $ \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> runQuickScan a (runQuickScan b next)
      | otherwise -> notEnoughCharacters
runQuickScan (ScanChoice a b count) next = withToMatch $ \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> catchQuickScan (runQuickScan a next) (runQuickScan b next)
      | otherwise -> notEnoughCharacters
runQuickScan (ScanNode pat count) next = withToMatch $ \toMatch -> case () of
    _ | count <= snobol4Length toMatch -> matchQuickScanPattern pat next
      | otherwise -> notEnoughCharacters
runQuickScan (UnevaluatedPattern _) _ = error "Found an unevaluated pattern in quick scan mode"
runQuickScan DeadNode _ = notEnoughCharacters



runQuickScan' :: (Monad m)
              => (expr -> m (Pattern expr)) 
              -> (expr -> m Snobol4Integer) 
              -> (expr -> m Snobol4String)
              -> QuickScanPath expr
              -> Snobol4String
              -> m (Maybe Snobol4String)
runQuickScan' toPat toInt toStr pat toMatch = do
    result <- runExceptT $ runQuickScan pat succeed toPat toInt toStr toMatch "" ""
    case result of
        Left Abort -> return Nothing
        Left Backtrack -> do
            case snobol4Uncons toMatch of
                Just (_,nextTry) -> runQuickScan' toPat toInt toStr pat nextTry
                _ -> return Nothing
        Left NotEnoughCharacters -> return Nothing
        Right matched -> return $ Just matched



quickscan' :: (Monad m)
          => Pattern expr
          -> Snobol4String
          -> (expr -> m (Pattern expr)) 
          -> (expr -> m Snobol4Integer) 
          -> (expr -> m Snobol4String)
          -> m (Maybe Snobol4String)
quickscan' pat toMatch toPat toInt toStr = do
    let len = snobol4Length toMatch
        fullPath = buildFullScanPath pat
        quickPath = makeQuickScanPath fullPath
        (annotatedQuickPath, _) = getQuickScanLength quickPath
        labeledQuickPath = labelQuickScanPath annotatedQuickPath
    (path, _) <- expandQuickScanPath toPat toInt toStr len labeledQuickPath
    runQuickScan' toPat toInt toStr path toMatch
