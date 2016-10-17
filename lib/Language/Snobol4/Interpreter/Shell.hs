{-|
Module          : Language.Snobol4.Interpreter.Shell
Description     : Abstract interface for the interpreter shell
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

This module provides two typeclasses which are used to abstract the details of
how the INPUT, OUTPUT, and PUNCH variables are used, as well as how the DATE
and TIME primitves are implemented. An instance of 'InterpreterShell' provides
these, while an instance of 'InterpreterShellRun' provides functions for
running the interpreter inside of another monad.

Once an interpreter action is ready to be executed, the 'shell' method of
'InterpreterShellRun' can be used to do so.

See "Language.Snobol4.Interpreter.Shell.Console" for an example.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Snobol4.Interpreter.Shell where

-- | Class of monads which can do input, output, and punching, as well as
-- retreive the last values sent to output and punch
class Monad m => InterpreterShell m where
    -- | Grab a line from input
    input :: m String
    -- | Send a line to output
    output :: String -> m ()
    -- | Send a line to punch
    punch :: String -> m ()
    -- | Get the string last sent to output
    lastOutput :: m String
    -- | Get the string last sent to punch
    lastPunch :: m String
    -- | Get a string representing the date
    date :: m String
    -- | Get the number of seconds ellapsed since starting the program
    time :: m Int
    

-- | Class of a pair monads, the first is an instance of 'InterpreterShell', and
-- the second is a monad which the first can be run inside
class InterpreterShell m => InterpreterShellRun m where
    type BaseMonad m :: * -> *
    -- | Perform any initial actions
    start :: m ()
    -- | Run the shell monad inside the base monad
    shell :: m a -> (BaseMonad m) a
