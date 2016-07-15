{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Snobol4.Interpreter.Shell.Console 
    ( ConsoleShell
    ) where

import System.IO

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

data ConsoleShellState
    = ConsoleShellState
    { lastInput :: String
    , lastOutput :: String
    , lastPunch :: String
    }

putLastInput s = ConsoleShell $ modify $ \st -> st{ lastInput = s }
putLastOutput s = ConsoleShell $ modify $ \st -> st{ lastOutput = s }
putLastPunch s = ConsoleShell $ modify $ \st -> st{ lastPunch = s }

getLastInput = ConsoleShell $ gets lastInput
getLastOutput = ConsoleShell $ gets lastOutput
getLastPunch = ConsoleShell $ gets lastPunch

emptyState = ConsoleShellState "" "" ""

newtype ConsoleShell a 
    = ConsoleShell 
    { runConsoleShell
        :: StateT ConsoleShellState IO a 
    }
    deriving (Functor, Applicative, Monad, MonadIO)

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

instance InterpreterShellRun ConsoleShell IO where
    start = ConsoleShell $ put $ emptyState
    shell = flip evalStateT emptyState . runConsoleShell
