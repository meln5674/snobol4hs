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

The scanner works using continuation passing, the operation to be performed
next is passed along with the pattern to match. When a pattern can fail and
try an alternative, it passes the next operation to that alternative along with
the current string to be matched, and runs it. If that fails, it repeats this
process for each remaining alternative.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Language.Snobol4.Interpreter.Scanner.Internal where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except


import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Internal.StateMachine

-- | The state of the scanner
data ScannerState expr
    = ScannerState
    { 
    -- | The input yet to scan
      inputStr :: Snobol4String
    -- | The list of assignments to perform after the scan succeeds
    , assignments :: [(Lookup expr, Data expr)]
    -- | The index in the input where matching began
    , startPos :: Snobol4Integer
    -- | The number of characters scanned so far
    , endPos :: Snobol4Integer
    -- | The number of characters that were skipped in non-anchor mode
    , anchorPos :: Snobol4Integer
    -- | True if the scanner is in fullscan mode
    , fullscan :: Bool
    }

-- | The scanner type
newtype ScannerGeneric program expr {-error-} m a
    = Scanner
    { runScanner
        :: StateT (ScannerState expr) (ExceptT FailType (InterpreterGeneric program m)) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Lift an action into the scanner
instance MonadTrans (ScannerGeneric program expr) where
    lift = Scanner . lift . lift . lift

-- | Lift an interpreter action into the scanner
liftInterpreter :: ( Monad m 
                   ) 
                => InterpreterGeneric program m a 
                -> ScannerGeneric program expr m a
liftInterpreter = Scanner . lift . lift

-- | The type of failure in the scanner
data FailType
    -- | The specific path failed, backtrack to the last checkpoint
    = BackTrack 
    -- | The entire scan failed
    | Abort
    -- | In quickscan mode, the number of characters is insufficient to match the pattern
    | InsufficientCharacters

-- | Cause the scanner to fail, jumping back to the most recent call to catchScan
throwScan :: Monad m => FailType -> ScannerGeneric program expr {-error-} m a
throwScan = Scanner . lift . throwE

-- | Cause the scanner to fail, jumping back to the most recent call to
-- catchScan, and continue with the next path
backtrack :: Monad m => ScannerGeneric (ProgramType m) expr {-error-} m a
backtrack = throwScan BackTrack

quickscanFail :: Monad m => ScannerGeneric (ProgramType m) expr {-error-} m a
quickscanFail = throwScan InsufficientCharacters

-- | Cause the scanner to fail completely
abort :: Monad m => ScannerGeneric (ProgramType m) expr {-error-} m a
abort = throwScan Abort

-- | Perform a scanner action, catching a failure and resetting the state and
-- performing the seconc action instead
catchScan :: Monad m 
          => ScannerGeneric (ProgramType m) expr {-error-} m a 
          -> ScannerGeneric (ProgramType m) expr {-error-} m a 
          -> ScannerGeneric (ProgramType m) expr {-error-} m a
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
        Left InsufficientCharacters -> do
            Scanner $ put st
            catch
        Left Abort -> abort

-- | Get the input yet to be scanned
getInput :: Monad m => ScannerGeneric (ProgramType m) expr {-error-} m Snobol4String
getInput = Scanner $ gets inputStr

-- | Peek at the next character to be scanned
nextChar :: Monad m => ScannerGeneric (ProgramType m) expr {-error-} m Snobol4String
nextChar = do
    str <- Scanner $ gets inputStr
    case snobol4Uncons str of
        Just (x,xs) -> return x
        Nothing -> backtrack

-- | Set the input yet to be scanned
setInput :: Monad m => Snobol4String -> ScannerGeneric (ProgramType m) expr {-error-} m ()
setInput s = Scanner $ modify $ \st -> st{inputStr = s}

-- | Increment the number of characters scanned
incEndPos :: Monad m => Snobol4Integer -> ScannerGeneric (ProgramType m) expr {-error-} m ()
incEndPos len = Scanner $ modify $ \st -> st{endPos = endPos st + mkInteger len}

-- | Get the position of the cursor
getCursorPos :: Monad m => ScannerGeneric (ProgramType m) expr {-error-} m Snobol4Integer
getCursorPos = Scanner $ gets $ \st -> endPos st + anchorPos st

-- | Get the distance of the cursor from the end of input
getRCursorPos :: Monad m => ScannerGeneric (ProgramType m) expr {-error-} m Snobol4Integer
getRCursorPos = snobol4Length <$> getInput

-- | Add an assignment to be performed after success
addAssignment :: Monad m => (Lookup (ExprType m)) -> (Data (ExprType m)) -> ScannerGeneric (ProgramType m) (ExprType m) {-error-} m ()
addAssignment l d = Scanner $ modify $ \st -> st{ assignments = (l,d):assignments st}

-- | Immediately assign a value
immediateAssignment :: ( InterpreterShell m
                       , NewSnobol4Machine m
                       , LocalVariablesClass m
                       , Ord (ExprType m)
                       ) 
                    => (Lookup (ExprType m)) 
                    -> (Data (ExprType m)) 
                    -> ScannerGeneric (ProgramType m) (ExprType m) {-(EvaluationError program)-} m ()
immediateAssignment l = Scanner . lift . lift . assign l

-- | Try to match a balanced pair of parenthesis
consumeBal :: Monad m => ScannerGeneric (ProgramType m) (ExprType m) m Snobol4String
consumeBal = liftM mkString $ loop 0 id
  where
    loop :: Monad m 
         => Int                 -- ^ Depth of parenthesis
         -> (String -> String)  -- ^ Function to apply to the string consumed
                                --   afterwards, allows O(1) string concatenation
         -> ScannerGeneric (ProgramType m) (ExprType m) m String
    loop depth strf = do
        c <- liftM unmkString nextChar
        case c of
            "(" -> consumeN 1 >> loop (depth+1) (\s -> strf ('(':s))
            ")"
                | depth == 1 -> consumeN 1 >> return (strf ")")
                | otherwise -> consumeN 1 >> loop (depth-1) (\s -> strf ('(':s))
            [c] -> consumeN 1 >> loop depth (\s -> strf (c:s))
            _ -> Scanner $ lift $ lift $ programError ErrorInSnobol4System

-- | Attempt to consume a string from input, failing if the start of the input
-- does not match thet provided string
consume :: Monad m => Snobol4String 
                   -> ScannerGeneric (ProgramType m) (ExprType m) {-(EvaluationError program)-} m Snobol4String
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
consumeN :: Monad m => Snobol4Integer -> ScannerGeneric (ProgramType m) (ExprType m) {-(EvaluationError program)-} m Snobol4String
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
consumeAll :: Monad m => ScannerGeneric (ProgramType m) (ExprType m) {-(EvaluationError program)-} m Snobol4String
consumeAll = do
    str <- getInput
    setInput nullString
    incEndPos (snobol4Length str)
    return str

-- | Consume any of the characters in the given string, otherwise fail
consumeAny :: Monad m => Snobol4String -> ScannerGeneric (ProgramType m) (ExprType m) {-(EvaluationError program)-} m Snobol4String
consumeAny cs = do
    c <- nextChar
    if c `snobol4Elem` cs
        then consumeN 1
        else backtrack

-- | Fail if the next character is not one of the supplied characters, but don't
-- consume it if it is
peekAny :: Monad m => Snobol4String -> ScannerGeneric (ProgramType m) (ExprType m) m Snobol4String
peekAny cs = do
    c <- nextChar
    if c `snobol4Elem` cs
        then return c
        else backtrack

-- | Consume any of the characters not in the given string, otherwise fail
consumeNotAny :: Monad m => Snobol4String -> ScannerGeneric (ProgramType m) (ExprType m) {-(EvaluationError program)-} m Snobol4String
consumeNotAny cs = do
    c <- nextChar
    if c `snobol4NotElem` cs
        then consumeN 1
        else backtrack

-- | Create a start state from input
startState :: Snobol4String -> Snobol4Integer -> Bool -> ScannerState expr
startState s skip fullscan = ScannerState
             { inputStr = snobol4Drop skip s
             , assignments = []
             , startPos = 0
             , endPos = 0
             , anchorPos = skip
             , fullscan = fullscan
             }

-- | I haven't thought up a name for this yet
func :: Monad m 
     => ScannerGeneric (ProgramType m) (ExprType m) {-(EvaluationError program)-} m Snobol4String 
     -> ScannerCont m Snobol4String
     -> ScannerCont m Snobol4String
func f next = \s1 _ -> f >>= \s2 -> next (s1 <> s2) s2

-- | I haven't thought up a name for this either
bar :: Monad m => (a -> ScannerGeneric (ProgramType m) (ExprType m) {-(EvaluationError program)-} m b) -> a -> ScannerGeneric (ProgramType m) (ExprType m) {-(EvaluationError program)-} m a
bar f v = f v >> return v

-- | Continuation type used by the scanner.
--
-- First argument is the entire string matched so far
--
-- Second argument is the string matched by the previously matched pattern
type ScannerCont m a = Snobol4String -> Snobol4String -> ScannerGeneric (ProgramType m) (ExprType m) m a


-- | `foo first second handler` is almost equivalent to
-- `catchScan (first >>= second) handler`, except that only `first` may fail
-- if `second` fails, the computation fails as normal
foo :: Monad m
    => ScannerGeneric (ProgramType m) (ExprType m)  m a 
    -> (a -> ScannerGeneric (ProgramType m) (ExprType m) m b)
    -> ScannerGeneric (ProgramType m) (ExprType m) m b 
    -> ScannerGeneric (ProgramType m) (ExprType m) m b
foo first second handler = do
    try <- catchScan (liftM Just first) (return Nothing)
    case try of
        Just result -> second result
        Nothing -> handler

minChars' :: (a -> Snobol4Integer)
          -> Lazy expr a
          -> Snobol4Integer
minChars' _ (Thunk _) = 1
minChars' f (EvaluatedThunk x) = f x

minStringChars :: LazyString expr
               -> Snobol4Integer
minStringChars = minChars' snobol4Length

minPatChars :: LazyPattern expr
            -> Snobol4Integer
            -> Snobol4Integer
            -> Snobol4Integer
minPatChars pat pos rpos = minChars' (\pat -> minChars pat pos rpos) pat


minIntChars :: LazyInteger expr
            -> Snobol4Integer
minIntChars = minChars' id


minChars :: Pattern expr
         -> Snobol4Integer
         -> Snobol4Integer
         -> Snobol4Integer
minChars (AssignmentPattern pat _) pos rpos = minPatChars pat pos rpos
minChars (ImmediateAssignmentPattern pat _) pos rpos = minPatChars pat pos rpos
minChars (LiteralPattern s) _ _ = snobol4Length s
minChars (AlternativePattern patA patB) pos rpos = min (minPatChars patA pos rpos) (minPatChars patB pos rpos)
minChars (ConcatPattern patA patB) pos rpos = (+) (minPatChars patA pos rpos) (minPatChars patB pos rpos)
minChars (LengthPattern len) _ _ = minIntChars len
minChars EverythingPattern _ rpos = rpos
minChars HeadPattern{} _ _ = 0
minChars SpanPattern{} _ _ = 1
minChars BreakPattern{} _ _ = 0
minChars AnyPattern{} _ _ = 1
minChars NotAnyPattern{} _ _ = 1
minChars (TabPattern col) pos _ =
    let col' = minIntChars col
    in min 0 (col'-pos)
minChars (RTabPattern rcol) _ rpos =
    let col' = minIntChars rcol
    in min 0 (rpos-col')
minChars PosPattern{} _ _ = 0
minChars RPosPattern{} _ _ = 0
minChars FailPattern _ _ = 0
minChars FencePattern _ _ = 0
minChars AbortPattern _ _ = 0
minChars ArbPattern _ _ = 0
minChars ArbNoPattern{} _ _ = 0
minChars BalPattern _ _ = 0
minChars SucceedPattern _ _ = 0

expandRecursive :: ( InterpreterShell m
                , NewSnobol4Machine m
                , LocalVariablesClass m
            )
         => Pattern (ExprType m)
         -> Snobol4Integer
         -> ScannerGeneric (ProgramType m) (ExprType m) m (Pattern (ExprType m))
expandRecursive p n
    | n <= 0 = return p
expandRecursive (ConcatPattern (Thunk x) y) limit = do
    x' <- liftInterpreter (force $ Thunk x) >>= maybe backtrack return
    let p' = ConcatPattern (EvaluatedThunk x') y
    taken <- getCursorPos
    remaining <- getRCursorPos
    let needed = minChars p' taken remaining
    if needed < limit
        then expandRecursive p' limit
        else return p'
{-
expandRecursive (ConcatPattern x (Thunk y)) limit = do
    y' <- liftInterpreter (force $ Thunk y) >>= maybe backtrack return
    let p' = ConcatPattern x (EvaluatedThunk y')
    needed <- minChars p'
    if needed < limit
        then expandRecursive p' limit
        else return p'
-}
expandRecursive (ConcatPattern (EvaluatedThunk x) y) limit = do
    taken <- getCursorPos
    remaining <- getRCursorPos
    let yMin = minPatChars y taken remaining
    x' <- expandRecursive x $ limit - yMin
    return $ ConcatPattern (EvaluatedThunk x') y
expandRecursive (AlternativePattern (EvaluatedThunk x) y) limit = do
    x' <- expandRecursive x limit
    return $ AlternativePattern (EvaluatedThunk x') y
expandRecursive p _ = return p

-- | Force the evaluation of a lazy value inside a scanner continuation,
-- backtracking if evaluation fails
scannerForce :: ( InterpreterShell m
                , NewSnobol4Machine m
                , LocalVariablesClass m
--                , Ord (ExprType m)
                , Forceable a m
                )
             => Lazy (ExprType m) a
             -> (a -> ScannerCont m b)
             -> ScannerCont m b
scannerForce x f s prev = liftInterpreter (force x) >>= maybe backtrack (\y -> f y s prev)

quickscan :: ( InterpreterShell m
                , NewSnobol4Machine m
                , LocalVariablesClass m
             )
          => ( Pattern (ExprType m) -> ScannerCont m a)
          -> Pattern (ExprType m)
          -> ScannerCont m a
quickscan f pat s prev = do
    quickscanEnabled <- Scanner $ gets $ not . fullscan
    if quickscanEnabled
        then do
            taken <- getCursorPos
            remaining <- getRCursorPos
            pat' <- expandRecursive pat remaining
            let needed = minChars pat' taken remaining
            if needed <= remaining
                then f pat' s prev
                else quickscanFail
        else f pat s prev

-- | Main scanner function
-- `match p next` attempts to match the pattern `p`, and if successful, calls
-- `next` with the string scanned so far.
-- The result of this function is a function which itself takes the string
-- scanned so far.
-- The top level call of this function should be called with the empty string.
match :: ( InterpreterShell m 
         , NewSnobol4Machine m
         , LocalVariablesClass m
         , Ord (ExprType m)
         )
      => Pattern (ExprType m)
      -> ScannerCont m Snobol4String
      -> ScannerCont m Snobol4String
match (AssignmentPattern pThunk l) next = scannerForce pThunk $ quickscan $ \p -> match p $ \s prev -> do
    addAssignment l $ StringData prev
    next s prev
match (ImmediateAssignmentPattern pThunk l) next = scannerForce pThunk $ quickscan $ \p -> match p $ \s prev -> do
    immediateAssignment l $ StringData prev
    next s prev 
match (LiteralPattern lit) next = func (consume $ mkString lit) next
match (AlternativePattern pThunk1 pThunk2) next =
    \s prev -> catchScan (scannerForce pThunk1 (quickscan (\p1 -> match p1 next)) s prev) 
                         (scannerForce pThunk2 (quickscan (\p2 -> match p2 next)) s prev)
match (ConcatPattern pThunk1 pThunk2) next = 
    scannerForce pThunk1 $ quickscan $ \p1 ->
    match p1 (scannerForce pThunk2 $ quickscan $ \p2 -> 
        \s1 prev1 -> 
            match p2 (\s2 prev2 -> next s2 (prev1 <> prev2)) s1 prev1
    ) 
match (LengthPattern lenThunk) next = scannerForce lenThunk $ \len -> func (consumeN len) next
match EverythingPattern next = func consumeAll next
{-
match (UnevaluatedExprPattern expr) next = \s prev -> do
    patResult <- do
        result <- Scanner $ lift $ lift $ eval expr
        case result of
            Just val -> Scanner $ lift $ lift $ toPattern val
            Nothing -> backtrack
    match patResult next s prev
-}
match (HeadPattern l) next = \s prev -> do
    pos <- getCursorPos
    immediateAssignment l $ IntegerData pos
    next s prev
    
match (SpanPattern csThunk) next = scannerForce csThunk $ \cs -> 
    let loop loopNext = \s1 prev -> foo
            (consumeAny cs)
            (\s2 -> loop loopNext (s1 <> s2) (prev <> s2))
            (loopNext s1 prev)
    in \s1 _ -> (consumeAny cs >>= \s2 -> loop next (s1 <> s2) s2)
match (BreakPattern csThunk) next = scannerForce csThunk $ \cs -> 
    let loop loopNext = \s1 prev -> foo 
            (consumeNotAny cs) 
            (\s2 -> loop loopNext (s1 <> s2) (prev <> s2))
            (peekAny cs >> loopNext s1 prev)
    in \s1 _ -> loop next s1 ""
match (AnyPattern csThunk) next = scannerForce csThunk $ \case
    "" -> \_ _ -> liftInterpreter $ programError NullStringInIllegalContext
    cs -> func (consumeAny cs) next
match (NotAnyPattern csThunk) next = scannerForce csThunk $ \cs -> func (consumeNotAny cs) next
match (TabPattern colThunk) next = scannerForce colThunk $ \col -> 
    \s prev -> do
        pos <- getCursorPos
        if pos <= col
            then func (consumeN $ col - pos) next s prev
            else backtrack
match (RTabPattern colThunk) next = scannerForce colThunk $ \col -> 
    \s prev -> do
        pos <- getRCursorPos
        if pos > col
            then func (consumeN $ pos - col) next s prev
            else backtrack
match (PosPattern colThunk) next = scannerForce colThunk $ \col -> 
    \s _ -> do
        pos <- getCursorPos
        if pos == col
            then next s ""
            else backtrack
match (RPosPattern colThunk) next = scannerForce colThunk $ \col -> 
    \s _ -> do
        pos <- getRCursorPos
        if pos == col
            then next s ""
            else backtrack
match FailPattern _ = \_ _ -> backtrack
match FencePattern next = \s _ -> catchScan (next s "") abort
match AbortPattern _ = \_ _ -> abort
match ArbPattern next =  \s1 _ -> (consumeN 1 >>= \s2 -> loop next (s1 <> s2) s2)
  where
    loop loopNext = \s1 prev -> catchScan
        (consumeN 1 >>= \s2 -> loop loopNext (s1 <> s2) (prev <> s2))
        (loopNext s1 prev)

match (ArbNoPattern pThunk) next = scannerForce pThunk $ \p -> 
    let loop loopNext = \s1 prev -> 
            catchScan (match p loopNext s1 prev) (loopNext s1 prev)
    in  \s1 _ -> catchScan (loop next s1 "") (next s1 "")
match BalPattern next = undefined
match SucceedPattern next = \s1 _ -> catchScan (next s1 "") (match SucceedPattern next s1 "")
