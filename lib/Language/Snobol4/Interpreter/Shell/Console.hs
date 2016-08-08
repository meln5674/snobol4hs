{-|
Module          : Language.Snobol4.Interpreter.Shell.Console
Description     : Console implementation of the InterpreterShell typeclass
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown


This module contains an example of the 'InterpreterShell' instance.

The ConsoleShell type uses stdin for INPUT, stdout for OUTPUT, and stderr for
PUNCH. It also uses a StateT transformer to hold the last values of each.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Snobol4.Interpreter.Shell.Console 
    ( ConsoleShell
    ) where

import System.IO

import Data.Time

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

import Language.Snobol4.Interpreter.Shell
    ( InterpreterShell
    , InterpreterShellRun
    )
import qualified Language.Snobol4.Interpreter.Shell
    ( InterpreterShell (..)
    , InterpreterShellRun (..)
    )

-- | A wrapper around whatever type is used to track time
newtype ConsoleShellTime = ConsoleShellTime { getConsoleShellTime :: UTCTime }

-- | The state of the shell
data ConsoleShellState
    = ConsoleShellState
    { lastInput :: String
    , lastOutput :: String
    , lastPunch :: String
    , startTime :: ConsoleShellTime
    }

--  Get the last string inputted
getLastInput :: ConsoleShell String
getLastInput = ConsoleShell $ gets lastInput

--  Get the last string outputted
getLastOutput :: ConsoleShell String
getLastOutput = ConsoleShell $ gets lastOutput

--  Get the last string punched
getLastPunch :: ConsoleShell String
getLastPunch = ConsoleShell $ gets lastPunch

-- | Set the last string inputted
putLastInput :: String -> ConsoleShell ()
putLastInput s = ConsoleShell $ modify $ \st -> st{ lastInput = s }

-- | Set the last string outputted
putLastOutput :: String -> ConsoleShell ()
putLastOutput s = ConsoleShell $ modify $ \st -> st{ lastOutput = s }

-- | Set the last string punched
putLastPunch :: String -> ConsoleShell ()
putLastPunch s = ConsoleShell $ modify $ \st -> st{ lastPunch = s }

-- | Initial state of the shell
emptyState :: ConsoleShellState
emptyState = ConsoleShellState "" "" "" undefined

-- | Get the current time
getCurrentConsoleShellTime :: ConsoleShell ConsoleShellTime
getCurrentConsoleShellTime = liftM ConsoleShellTime $ liftIO $ getCurrentTime
    
-- | Convert whatever time type is used to a string with format mm/dd/yyyy
getConsoleShellDateString :: ConsoleShellTime -> String
getConsoleShellDateString (ConsoleShellTime utctime) = [m1,m2,'/',d1,d2,'/',y1,y2,y3,y4]
  where
    [y1,y2,y3,y4,_,m1,m2,_,d1,d2] = showGregorian $ utctDay utctime

-- | Get the difference between two times
consoleShellDateDiff :: ConsoleShellTime -> ConsoleShellTime -> Int
consoleShellDateDiff (ConsoleShellTime t2) (ConsoleShellTime t1) = floor secs
  where
    picoSecs = diffUTCTime t2 t1
    secs = picoSecs * 1000000000000
    

-- | A monad for running the interpreter using the console for IO
newtype ConsoleShell a 
    = ConsoleShell 
    { runConsoleShell
        :: StateT ConsoleShellState IO a 
    }
    deriving (Functor, Applicative, Monad, MonadIO)

-- |
instance InterpreterShell ConsoleShell where
    input = do
        str <- liftIO $ hGetLine stdin
        putLastInput str
        return str
    output str = do
        putLastOutput str
        liftIO $ hPutStrLn stdout str
    punch str = do
        putLastPunch str
        liftIO $ hPutStrLn stderr str
    lastOutput = getLastOutput
    lastPunch = getLastPunch
    date = getConsoleShellDateString <$> getCurrentConsoleShellTime
    time = do
        t1 <- ConsoleShell $ gets startTime
        t2 <- getCurrentConsoleShellTime
        return $ consoleShellDateDiff t2 t1

-- |
instance InterpreterShellRun ConsoleShell IO where
    start = do
        currentTime <- getCurrentConsoleShellTime
        ConsoleShell $ modify $ \s -> s{ startTime = currentTime }
    shell = flip evalStateT emptyState . runConsoleShell
