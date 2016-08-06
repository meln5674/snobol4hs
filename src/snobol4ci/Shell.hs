{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Shell where

import Data.Time

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import System.Console.Haskeline

import Language.Snobol4.Interpreter.Shell
    ( InterpreterShell
    , InterpreterShellRun
    )
import qualified Language.Snobol4.Interpreter.Shell
    ( InterpreterShell (..)
    , InterpreterShellRun (..)
    )

newtype HaskelineShellTime = HaskelineShellTime { getHaskelineShellTime :: UTCTime }

data HaskelineShellState = HaskelineShellState
    { lastInput :: String
    , lastOutput :: String
    , lastPunch :: String
    , startTime :: HaskelineShellTime
    }

-- | A shell for the interpreter that uses Haskeline for INPUT, OUTPUT, and
-- PUNCH
newtype HaskelineShell a
    = HaskelineShell
    { runHaskelineShell
        :: StateT HaskelineShellState (InputT IO) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance InterpreterShell HaskelineShell where
    input = HaskelineShell $ do
        i <- lift $ getInputLine "INPUT>>"
        let i' = maybe "" id i
        modify $ \s -> s{lastInput= i'}
        return i'
    output o = HaskelineShell $ do
        lift $ outputStrLn o
        modify $ \s -> s{lastOutput=o}
    punch p = HaskelineShell $ do
        lift $ outputStrLn p
        modify $ \s -> s{lastPunch=p}
    lastOutput = HaskelineShell $ gets lastOutput
    lastPunch = HaskelineShell $ gets lastPunch
    date = getHaskelineShellDateString <$> getCurrentHaskelineShellTime
    time = do
        t1 <- HaskelineShell $ gets startTime
        t2 <- getCurrentHaskelineShellTime
        return $ haskelineShellDateDiff t2 t1

instance InterpreterShellRun HaskelineShell IO where
    start = do
        currentTime <- getCurrentHaskelineShellTime
        HaskelineShell $ modify $ \s -> s{startTime = currentTime}
    shell = runInputT defaultSettings 
          . flip evalStateT (HaskelineShellState "" "" "" undefined)
          . runHaskelineShell

getCurrentHaskelineShellTime :: HaskelineShell HaskelineShellTime
getCurrentHaskelineShellTime = liftM HaskelineShellTime $ liftIO $ getCurrentTime
    
getHaskelineShellDateString :: HaskelineShellTime -> String
getHaskelineShellDateString (HaskelineShellTime utctime) = [m1,m2,'/',d1,d2,'/',y1,y2,y3,y4]
  where
    [y1,y2,y3,y4,_,m1,m2,_,d1,d2] = showGregorian $ utctDay utctime
    
haskelineShellDateDiff :: HaskelineShellTime -> HaskelineShellTime -> Int
haskelineShellDateDiff (HaskelineShellTime t2) (HaskelineShellTime t1) = floor secs
  where
    picoSecs = diffUTCTime t2 t1
    secs = picoSecs * 1000000000000

