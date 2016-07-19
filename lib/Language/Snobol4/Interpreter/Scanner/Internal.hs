{-|
Module          : Language.Snobol4.Interpreter.Scanner.Internal
Description     : Internal functions of the scanner
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

Overview of the scanner:

The scanner works similarly to the evaluator or interpreter in that actions by
it are represented by a newtype hiding a stack of transformers.

The scanner's stack consists of an Evaluator at the bottom, with a MaybeT used
to catch failures, and a StateT on top which contains the input yet to be
scanned, the assignments to be performed after scanning, and the range of input
that has been scanned.

The scanner operates by finding all alternatives at the current position, then
attempting each one. If an alternative fails, represented by the Nothing value
in the MaybeT transformer, the state is rewound and the next alternative is
tried.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.Interpreter.Scanner.Internal where

import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Types
import {-# SOURCE #-} Language.Snobol4.Interpreter.Evaluator
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.Types

-- | The state of the scanner
data ScannerState
    = ScannerState
    { 
    -- | The input yet to scan
      inputStr :: Snobol4String
    -- | The list of assignments to perform after the scan succeeds
    , assignments :: [(Lookup,Data)]
    -- | The index in the input where matching began
    , startPos :: Snobol4Integer
    -- | The number of characters scanned so far
    , endPos :: Snobol4Integer
    }

-- | The scanner type
newtype Scanner m a
    = Scanner
    { runScanner
        :: StateT ScannerState (ExceptT FailType (Evaluator m)) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

data FailType = BackTrack | Abort

-- | Cause the scanner to fail, jumping back to the most recent call to catchScan
throwScan :: Monad m => FailType -> Scanner m a
throwScan = Scanner . lift . throwE

backtrack :: Monad m => Scanner m a
backtrack = throwScan BackTrack

abort :: Monad m => Scanner m a
abort = throwScan Abort

-- | Perform a scanner action, catching a failure and resetting the state and
-- performing the seconc action instead
catchScan :: Monad m => Scanner m a -> Scanner m a -> Scanner m a
catchScan try catch = do
    st <- Scanner get
    result <- Scanner 
            $ lift
            $ lift
            $ runExceptT 
            $ flip runStateT st
            $ runScanner 
              try
    case result of
        Right (x,st') -> do
            Scanner $ put st'
            return x
        Left BackTrack -> do
            Scanner $ put st
            catch
        Left Abort -> abort

-- | Get the input yet to be scanned
getInput :: Monad m => Scanner m Snobol4String
getInput = Scanner $ gets inputStr

-- | Peek at the next character to be scanned
nextChar :: Monad m => Scanner m Snobol4String
nextChar = Scanner $ gets $ snobol4Head . inputStr

-- | Set the input yet to be scanned
setInput :: Monad m => Snobol4String -> Scanner m ()
setInput s = Scanner $ modify $ \st -> st{inputStr = s}

-- | Increment the number of characters scanned
incEndPos :: Monad m => Snobol4Integer -> Scanner m ()
incEndPos len = Scanner $ modify $ \st -> st{endPos = endPos st + mkInteger len}

-- | Get the position of the cursor
getCursorPos :: Monad m => Scanner m Snobol4Integer
getCursorPos = Scanner $ gets endPos

-- | Get the distance of the cursor from the end of input
getRCursorPos :: Monad m => Scanner m Snobol4Integer
getRCursorPos = snobol4Length <$> getInput

-- | Add an assignment to be performed after success
addAssignment :: Monad m => Lookup -> Data -> Scanner m ()
addAssignment l d = Scanner $ modify $ \st -> st{ assignments = (l,d):assignments st}

-- | Immediately assign a value
immediateAssignment :: InterpreterShell m => Lookup -> Data -> Scanner m ()
immediateAssignment l = Scanner . lift . lift . assign l

-- | Attempt to consume a string from input, failing if the start of the input
-- does not match thet provided string
consume :: Monad m => Snobol4String -> Scanner m Snobol4String
consume s = do
    str <- getInput
    let prefix = snobol4Take (snobol4Length s) str
    if prefix == s
        then do
            setInput $ snobol4Drop (snobol4Length s) str
            incEndPos (snobol4Length s)
            return prefix
    else backtrack

-- | Consume the first N characters, failing if that many characters are not present
consumeN :: Monad m => Snobol4Integer -> Scanner m Snobol4String
consumeN len = do
    str <- getInput
    let prefix = snobol4Take len str
    if len <= snobol4Length str
        then do
            setInput $ snobol4Drop len str
            incEndPos len
            return prefix
        else backtrack

-- | Consume the rest of the string
consumeAll :: Monad m => Scanner m Snobol4String
consumeAll = do
    str <- getInput
    setInput nullString
    incEndPos (snobol4Length str)
    return str

consumeAny :: Monad m => Snobol4String -> Scanner m Snobol4String
consumeAny cs = do
    c <- nextChar
    if c `snobol4Elem` cs
        then consumeN 1
        else backtrack

consumeNotAny :: Monad m => Snobol4String -> Scanner m Snobol4String
consumeNotAny cs = do
    c <- nextChar
    if c `snobol4NotElem` cs
        then consumeN 1
        else backtrack

{-
-- | Given a pattern, find all alternatives at the current position
getAlternatives :: InterpreterShell m => Pattern -> Scanner m [Scanner m String]
getAlternatives (AssignmentPattern p l) = map after `liftM` getAlternatives p
  where
    after m = do
        result <- m
        addAssignment l (StringData result)
        return result
getAlternatives (ImmediateAssignmentPattern p l) = map after `liftM` getAlternatives p
  where
    after m = do
        result <- m
        immediateAssignment l $ StringData result
        return result
getAlternatives (LiteralPattern s) = return $ (:[]) $ consume s
getAlternatives (AlternativePattern p1 p2) = (++) <$> getAlternatives p1 <*> getAlternatives p2
getAlternatives (ConcatPattern p1 p2) = do
    as1 <- getAlternatives p1
    let x = forM as1 $ \a1 -> do
            a1' <- a1
            as2 <- getAlternatives p2
            return $ flip map as2 $ \a2 -> do
                a2' <- a2
                return $ a1' ++ a2'
    concat <$> x
getAlternatives (LengthPattern l) = return $ (:[]) $ consumeN l
getAlternatives EverythingPattern = return $ (:[]) consumeAll
getAlternatives (UnevaluatedExprPattern expr) = do 
    patResult <- Scanner $ lift $ lift $ do
        result <- liftEval $ catchEval (Just <$> evalExpr expr) $ \_ -> return Nothing
        case result of
            Just val -> Just <$> toPattern val
            Nothing -> return Nothing
    case patResult of
        Just pat -> getAlternatives pat
        Nothing -> return $ (:[]) throwScan
getAlternatives (HeadPattern l) = return $ (:[]) $ do
    pos <- getCursorPos
    immediateAssignment l $ IntegerData pos
    return ""
getAlternatives (SpanPattern cs) = do
    str <- getInput
    let (longest,_) = span (`elem` cs) str
        matches = reverse $ inits longest
    return $ map consume matches
getAlternatives (BreakPattern cs) = do
    str <- getInput
    let (longest,_) = span (`notElem` cs) str
        matches = reverse $ tails longest
    return $ map consume matches
getAlternatives (AnyPattern cs) = return $ (:[]) $ consumeAny cs
getAlternatives (NotAnyPattern cs) = return $ (:[]) $ consumeNotAny cs
getAlternatives (TabPattern pos) = return $ (:[]) $ do
    pos' <- getCursorPos
    if pos <= pos'
        then return ""
        else throwScan
getAlternatives (RTabPattern pos) = return $ (:[]) $ do
    pos' <- getRCursorPos
    if pos' <= pos
        then return ""
        else throwScan
getAlternatives (PosPattern pos) = return $ (:[]) $ do
    pos' <- getCursorPos
    if pos' == pos
        then return ""
        else throwScan
getAlternatives (RPosPattern pos) = return $ (:[]) $ do
    pos' <- getRCursorPos
    if pos' == pos
        then return ""
        else throwScan
getAlternatives FailPattern = return $ (:[]) throwScan
getAlternatives FencePattern = undefined
getAlternatives AbortPattern = undefined
getAlternatives ArbPattern = do
    str <- getInput
    return $ map consume $ tails str
--getAlternatives (ArbNoPattern p) = return $ flip map [0..] $ liftM concat . replicateM (getAlternatives
getAlternatives (ArbNoPattern _) 
    = Scanner 
    $ lift 
    $ lift 
    $ liftEval 
    $ programError 
      ErrorInSnobol4System

{-
-- | Match a pattern
matchPat :: InterpreterShell m => Pattern -> Scanner m String
matchPat (AssignmentPattern p l) = do
    result <- matchPat p
    addAssignment l (StringData result)
    return result
matchPat (ImmediateAssignmentPattern p l) = do
    result <- matchPat p
    immediateAssignment l $ StringData result
    return result
matchPat (LiteralPattern s) = consume s
matchPat (AlternativePattern p1 p2) = catchScan (matchPat p1) (matchPat p2)
matchPat (ConcatPattern (AlternativePattern p1 p2) p3) = do
    let try1 = do
            r1 <- matchPat p1
            r2 <- matchPat p3
            return $ r1 ++ r2
        try2 = do
            r1 <- matchPat p2
            r2 <- matchPat p3
            return $ r1 ++ r2
    catchScan try1 try2
matchPat (ConcatPattern p1 p2) = do
    r1 <- matchPat p1
    r2 <- matchPat p2
    return $ r1 ++ r2
matchPat (LengthPattern len) = consumeN len
matchPat (UnevaluatedExprPattern expr) = do 
    pat <- Scanner $ lift $ lift $ do
        result <- liftEval $ catchEval (Just <$> evalExpr expr) $ \_ -> return Nothing
        case result of
            Just result -> Just <$> toPattern result
            Nothing -> return Nothing
    case pat of
        Just pat -> matchPat pat
        Nothing -> throwScan
matchPat (HeadPattern l) = do
    pos <- getCursorPos
    immediateAssignment l $ IntegerData pos
    return ""
matchPat (SpanPattern chars) = do
    let loop buf = do
            c <- nextChar
            if c `elem` chars
                then catchScan (consumeN 1 >> (loop $ buf ++ [c])) (
matchPat (BreakPattern chars) = undefined
matchPat (AnyPattern chars) = do
    c <- nextChar
    if c `elem` chars
        then consumeN 1
        else throwScan
matchPat (NotAnyPattern chars) = do
    c <- nextChar
    if c `notelem` chars
        then consumeN 1
        else throwScan
matchPat (TabPattern pos) = do
    pos' <- getCursorPos
    if pos <= pos'
        then return ""
        else throwScan
matchPat (RTabPAttern pos) = do
    pos' <- getCursorPos
    if pos' <= pos
        then return ""
        else throwScan 
matchPat (PosPattern pos) = do
    pos' <- getCursorPos
    if pos == pos'
        then return ""
        else throwScan
matchPat (RPosPattern pos) = do
    pos' <- getRCursorPos
    if pos == pos'
        then return ""
        else throwScan
matchPat FailPattern = throwScan
matchPat FencePattern = undefined
matchPat AbortPattern = undefined  
matchPat EverythingPattern = consumeAll
-}    

-}    
-- | Create a start state from input
startState :: Snobol4String -> ScannerState
startState s = ScannerState
             { inputStr = s
             , assignments = []
             , startPos = 0
             , endPos = 0
             }
{-
-- | Try each of a set of alternatives, backtracking if any fail and trying the
-- next
evaluateAlternatives :: InterpreterShell m  
                     => [Scanner m a]
                     -> Scanner m a
evaluateAlternatives = foldr catchScan throwScan

-- | Run the scanner
matchPat :: InterpreterShell m => Pattern -> Scanner m String
matchPat p = do
    st <- Scanner get
    as <- getAlternatives p
    Scanner $ put st
    evaluateAlternatives as
-}




func f next = \s1 -> f >>= \s2 -> next (s1 <> s2)

bar f v = f v >> return v



match :: InterpreterShell m 
      => Pattern 
      -> (Snobol4String -> Scanner m Snobol4String)
      -> (Snobol4String -> Scanner m Snobol4String)
match (AssignmentPattern p l) next = match p $ \s -> do
    addAssignment l $ StringData s
    next s
match (ImmediateAssignmentPattern p l) next = match p $ \s -> do
    immediateAssignment l $ StringData s
    next s
match (LiteralPattern lit) next = func (consume $ mkString lit) next
match (AlternativePattern p1 p2) next = \s -> catchScan (match p1 next s) (match p2 next s)
match (ConcatPattern p1 p2) next = match p1 $ match p2 next
match (LengthPattern len) next = func (consumeN len) next
match EverythingPattern next = func consumeAll next
match (UnevaluatedExprPattern expr) next = \s -> do
    patResult <- Scanner $ lift $ lift $ do
        result <- liftEval $ catchEval (Just <$> evalExpr expr) $ \_ -> return Nothing
        case result of
            Just val -> Just <$> toPattern val
            Nothing -> return Nothing
    case patResult of
        Just pat -> match pat next s
        Nothing -> backtrack
match (HeadPattern l) next = \s -> do
    pos <- getCursorPos
    immediateAssignment l $ IntegerData pos
    next s
    
match (SpanPattern cs) next = \s1 -> catchScan
    (consumeAny cs >>= \s2 -> match (SpanPattern cs) next (s1 <> s2))
    (next s1)
match (BreakPattern cs) next = \s1 -> catchScan
    (consumeNotAny cs >>= \s2 -> match (BreakPattern cs) next (s1 <> s2)) 
    (next s1)
match (AnyPattern cs) next = func (consumeAny cs) next
match (NotAnyPattern cs) next = func (consumeNotAny cs) next
match (TabPattern col) next = \s -> do
    pos <- getCursorPos
    if pos <= col
        then next s
        else backtrack
match (RTabPattern col) next = \s -> do
    pos <- getRCursorPos
    if pos < col
        then next s
        else backtrack
match (PosPattern col) next = \s -> do
    pos <- getCursorPos
    if pos == col
        then next s
        else backtrack
match (RPosPattern col) next = \s -> do
    pos <- getRCursorPos
    if pos == col
        then next s
        else backtrack
match FailPattern next = \s -> backtrack
match FencePattern next = \s -> catchScan (next s) abort
match AbortPattern next = \s -> abort
match ArbPattern next = \s1 -> catchScan
    (consumeN 1 >>= \s2 -> match ArbPattern next (s1 <> s2))
    (next s1)
match (ArbNoPattern p) next = \s1 -> catchScan
    (match p (match (ArbNoPattern p) next) s1)
    (next s1)


