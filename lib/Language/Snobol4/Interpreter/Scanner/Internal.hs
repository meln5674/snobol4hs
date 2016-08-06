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

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except


import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell
import {-# SOURCE #-} Language.Snobol4.Interpreter.Internal
import Language.Snobol4.Interpreter.Internal.StateMachine

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

-- | The type of failure in the scanner
data FailType
    -- | The specific path failed, backtrack to the last checkpoint
    = BackTrack 
    -- | The entire scan failed
    | Abort

-- | Cause the scanner to fail, jumping back to the most recent call to catchScan
throwScan :: Monad m => FailType -> Scanner m a
throwScan = Scanner . lift . throwE

-- | Cause the scanner to fail, jumping back to the most recent call to
-- catchScan, and continue with the next path
backtrack :: Monad m => Scanner m a
backtrack = throwScan BackTrack

-- | Cause the scanner to fail completely
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

-- | Consume any of the characters in the given string, otherwise fail
consumeAny :: Monad m => Snobol4String -> Scanner m Snobol4String
consumeAny cs = do
    c <- nextChar
    if c `snobol4Elem` cs
        then consumeN 1
        else backtrack

-- | Consume any of the characters not in the given string, otherwise fail
consumeNotAny :: Monad m => Snobol4String -> Scanner m Snobol4String
consumeNotAny cs = do
    c <- nextChar
    if c `snobol4NotElem` cs
        then consumeN 1
        else backtrack

-- | Create a start state from input
startState :: Snobol4String -> ScannerState
startState s = ScannerState
             { inputStr = s
             , assignments = []
             , startPos = 0
             , endPos = 0
             }

-- | ???
func :: Monad m 
     => Scanner m Snobol4String 
     -> (Snobol4String -> Scanner m a) 
     -> (Snobol4String -> Scanner m a)
func f next = \s1 -> f >>= \s2 -> next (s1 <> s2)

-- | ???
bar :: Monad m => (a -> Scanner m b) -> a -> Scanner m a
bar f v = f v >> return v

-- | Main scanner function
-- `match p next` attempts to match the pattern `p`, and if successful, calls
-- `next` with the string scanned so far.
-- The restult of this function is a function which itself takes the string
-- scanned so far.
-- The top level call of this function should be called with the empty string.
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
match FailPattern _ = const backtrack
match FencePattern next = \s -> catchScan (next s) abort
match AbortPattern _ = const abort
match ArbPattern next = \s1 -> catchScan
    (consumeN 1 >>= \s2 -> match ArbPattern next (s1 <> s2))
    (next s1)
match (ArbNoPattern p) next = \s1 -> catchScan
    (match p (match (ArbNoPattern p) next) s1)
    (next s1)


