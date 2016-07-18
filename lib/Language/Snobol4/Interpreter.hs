{-|
Module          : Language.Snobol4.Interpreter
Description     : The SNOBOL4 Interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown


Public interface to the SNOBOL4 interpreter
-}

module Language.Snobol4.Interpreter 
    ( PausedInterpreter
    , load
    , step
    , eval
    , exec
    , run
    , isTerminated
    , module Language.Snobol4.Interpreter.Types
    ) where

import Control.Monad
import Control.Monad.Trans

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Types

import Language.Snobol4.Interpreter.Internal
    ( PausedInterpreter (..)
    , Interpreter
    , emptyState
    , ExecResult(..)
    )
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.State
import qualified Language.Snobol4.Interpreter.Internal as I

-- | Run an interpreter action using an empty starting state
startInterpreter :: InterpreterShell m 
                 => Interpreter m a 
                 -> m (PausedInterpreter m, Maybe a)
startInterpreter = resumeInterpreter (Paused emptyState)

-- | Run an interpreter action using an empty starting state, and discard the 
-- result 
startInterpreter' :: InterpreterShell m => Interpreter m a -> m (PausedInterpreter m)
startInterpreter' = liftM fst . startInterpreter

-- | Run an interpreter action in a paused interpreter
resumeInterpreter :: InterpreterShell m 
                  => PausedInterpreter m
                  -> Interpreter m a 
                  -> m (PausedInterpreter m, Maybe a)
resumeInterpreter (Terminated err) _ = return (Terminated err, Nothing)
resumeInterpreter (Paused st) m = do
    result <- I.interpret st $ do
        x <- m
        st' <- I.getProgramState
        return (st', Just x)
    case result of
        Right (st', x) -> return (Paused st', x)
        Left err -> return (Terminated err, Nothing)

-- | Run an interpreter action in a paused interpreter, and discard the result
resumeInterpreter' :: InterpreterShell m
                   => PausedInterpreter m
                   -> Interpreter m a
                   -> m (PausedInterpreter m)
resumeInterpreter' st = liftM fst . resumeInterpreter st
    
-- | Run a SNOBOL4 program
run :: InterpreterShell m => Program -> m ProgramError
run code = do
    result <- I.interpret emptyState $ I.load code >> I.run
    case result of
        Right result -> return result


-- | Load a program into the interpreter and then pause it
load :: InterpreterShell m => Program -> m (PausedInterpreter m)
load code = startInterpreter' $ I.load code
    

-- | Take a paused interpreter and execute the next statement
step :: InterpreterShell m => PausedInterpreter m -> m (PausedInterpreter m)
step (Terminated err) = return $ Terminated err
step (Paused st) = do
    result <- I.interpret st $ do
        I.step
        I.getProgramState
    case result of
        Left err -> return $ Terminated err
        Right st' -> return $ Paused st'

-- | Evaluate an expression in a paused interpreter
eval :: InterpreterShell m 
     => Expr
     -> PausedInterpreter m
     -> m (PausedInterpreter m, Maybe Data)
eval _ (Terminated err) = return (Terminated err, Nothing)
eval expr (Paused st) = do
    result <- I.interpret st $ do
        result <- I.unliftEval $ I.evalExpr expr
        st' <- I.getProgramState
        return $ case result of
            Left stop -> (Paused st', Nothing)
            Right val -> (Paused st', Just val)
    return $ case result of
        Left err -> (Terminated err, Nothing)
        Right x -> x

-- | Execute a statement in a paused interpreter
exec :: InterpreterShell m
     => Stmt
     -> PausedInterpreter m
     -> m (PausedInterpreter m, Maybe Data)
exec _ (Terminated err) = return (Terminated err, Nothing)
exec stmt (Paused st) = do
    result <- I.interpret st $ do
        result <- I.exec stmt
        st <- I.getProgramState
        return (st, result)
    case result of
        Left err -> return (Terminated err, Nothing)
        Right (st, StmtResult x) -> return (Paused st, x)
        Right (_, Return) -> return (Terminated ReturnFromZeroLevel, Nothing)
        Right (_, FReturn) -> return (Terminated ReturnFromZeroLevel, Nothing)

-- | Check if a paused interpreter is terminated, and if so, return the error
isTerminated :: PausedInterpreter m -> Maybe ProgramError
isTerminated (Terminated err) = Just err
isTerminated _ = Nothing
