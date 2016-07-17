{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.Interpreter.Scanner where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe

import Text.Parsec ( (<|>), ParsecT, runParserT )
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P


import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.Types


data ScannerState
    = ScannerState
    { inputStr :: String
    , assignments :: [(Lookup,Data)]
    , startPos :: Int
    , endPos :: Int
    }

newtype Scanner m a
    = Scanner
    { runScanner
        :: StateT ScannerState (MaybeT (Evaluator m)) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

throwScan :: Monad m => Scanner m a
throwScan = Scanner $ lift $ MaybeT $ return Nothing

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

getInput :: Monad m => Scanner m String
getInput = Scanner $ gets inputStr

setInput :: Monad m => String -> Scanner m ()
setInput s = Scanner $ modify $ \st -> st{inputStr = s}

incEndPos :: Monad m => Int -> Scanner m ()
incEndPos len = Scanner $ modify $ \st -> st{endPos = endPos st + len}

addAssignment :: Monad m => Lookup -> Data -> Scanner m ()
addAssignment l d = Scanner $ modify $ \st -> st{ assignments = (l,d):assignments st}

immediateAssignment :: InterpreterShell m => Lookup -> Data -> Scanner m ()
immediateAssignment l d = Scanner $ lift $ lift $ assign l $ d

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

consumeAll :: Monad m => Scanner m String
consumeAll = do
    str <- getInput
    setInput ""
    incEndPos (length str)
    return str


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
matchPat EverythingPattern = consumeAll
    


startState :: String -> ScannerState
startState s = ScannerState
             { inputStr = s
             , assignments = []
             , startPos = 0
             , endPos = 0
             }

scanPattern :: InterpreterShell m
            => String
            -> Pattern
            -> Evaluator m ScanResult
scanPattern s pat = do
    result <- runMaybeT $ flip runStateT (startState s) $ runScanner $ matchPat pat
    case result of
        Just (x, st) -> do
            let ScannerState{assignments=as,startPos=a,endPos=b} = st
            return $ Scan (StringData x) as a b
        Nothing -> return NoScan
