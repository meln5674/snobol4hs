{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.Interpreter.Scanner.Internal where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe

import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Evaluator
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.Types

-- | The state of the scanner
data ScannerState
    = ScannerState
    { 
    -- | The input yet to scan
      inputStr :: String
    -- | The list of assignments to perform after the scan succeeds
    , assignments :: [(Lookup,Data)]
    -- | The index in the input where matching began
    , startPos :: Int
    -- | The number of characters scanned so far
    , endPos :: Int
    }

-- | The scanner type
newtype Scanner m a
    = Scanner
    { runScanner
        :: StateT ScannerState (MaybeT (Evaluator m)) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Cause the scanner to fail, jumping back to the most recent call to catchScan
throwScan :: Monad m => Scanner m a
throwScan = Scanner $ lift $ MaybeT $ return Nothing

-- | Perform a scanner action, catching a failure and resetting the state and
-- performing the seconc action instead
catchScan :: Monad m => Scanner m a -> Scanner m a -> Scanner m a
catchScan try catch = do
    st <- Scanner get
    result <- Scanner 
            $ lift
            $ lift
            $ runMaybeT 
            $ flip runStateT st
            $ runScanner 
            $ try
    case result of
        Just (x,st') -> do
            Scanner $ put st'
            return x
        Nothing -> catch

-- | Get the input yet to be scanned
getInput :: Monad m => Scanner m String
getInput = Scanner $ gets inputStr

-- | Set the input yet to be scanned
setInput :: Monad m => String -> Scanner m ()
setInput s = Scanner $ modify $ \st -> st{inputStr = s}

-- | Increment the number of characters scanned
incEndPos :: Monad m => Int -> Scanner m ()
incEndPos len = Scanner $ modify $ \st -> st{endPos = endPos st + len}

-- | Add an assignment to be performed after success
addAssignment :: Monad m => Lookup -> Data -> Scanner m ()
addAssignment l d = Scanner $ modify $ \st -> st{ assignments = (l,d):assignments st}

-- | Immediately assign a value
immediateAssignment :: InterpreterShell m => Lookup -> Data -> Scanner m ()
immediateAssignment l d = Scanner $ lift $ lift $ assign l $ d

-- | Attempt to consume a string from input, failing if the start of the input
-- does not match thet provided string
consume :: Monad m => String -> Scanner m String
consume s = do
    str <- getInput
    let prefix = take (length s) str
    if prefix == s
        then do
            setInput $ drop (length s) str
            incEndPos (length s)
            return prefix
    else throwScan

-- | Consume the first N characters, failing if that many characters are not present
consumeN :: Monad m => Int -> Scanner m String
consumeN len = do
    str <- getInput
    let prefix = take len str
    if len < length str
        then do
            setInput $ drop len str
            incEndPos len
            return prefix
        else throwScan

-- | Consume the rest of the string
consumeAll :: Monad m => Scanner m String
consumeAll = do
    str <- getInput
    setInput ""
    incEndPos (length str)
    return str

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

    
matchPat EverythingPattern = consumeAll
    
-- | Create a start state from input
startState :: String -> ScannerState
startState s = ScannerState
             { inputStr = s
             , assignments = []
             , startPos = 0
             , endPos = 0
             }
