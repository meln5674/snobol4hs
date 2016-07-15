{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Snobol4.Interpreter.Shell where

class Monad m => InterpreterShell m where
    input :: m String
    output :: String -> m ()
    punch :: String -> m ()
    lastOutput :: m String
    lastPunch :: m String

class InterpreterShell m => InterpreterShellRun m base where
    start :: m ()
    shell :: m a -> base a
