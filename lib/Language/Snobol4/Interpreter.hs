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

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Types

import Language.Snobol4.Interpreter.Internal
    ( PausedInterpreter (..)
    , Interpreter
    , ExecResult(..)
    )
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.State
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Internal (emptyState)
import qualified Language.Snobol4.Interpreter.Internal as I
import Language.Snobol4.Interpreter.Internal.StateMachine.Types

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
        Left err -> return 
            ( Terminated $ ErrorTermination err $ unmkInteger $ getAddress $ programCounter st
            , Nothing
            )

-- | Run an interpreter action in a paused interpreter, and discard the result
resumeInterpreter' :: InterpreterShell m
                   => PausedInterpreter m
                   -> Interpreter m a
                   -> m (PausedInterpreter m)
resumeInterpreter' st = liftM fst . resumeInterpreter st
    
-- | Run a SNOBOL4 program
run :: InterpreterShell m => Program -> m ProgramResult
run code = do
    result <- I.interpret emptyState $ I.load code >> I.run
    case result of
        Right result -> return result
        Left _ -> return $ ErrorTermination ErrorInSnobol4System $ -1

-- | Load a program into the interpreter and then pause it
load :: InterpreterShell m => Program -> m (PausedInterpreter m)
load code = startInterpreter' $ I.load code
    

-- | Take a paused interpreter and execute the next statement
step :: InterpreterShell m => PausedInterpreter m -> m (PausedInterpreter m)
step (Terminated err) = return $ Terminated err
step (Paused st) = do
    result <- I.interpret st $ do
        result <- I.step
        st' <- I.getProgramState
        return (st', result)
    return $ case result of
        Right (st', ProgramIncomplete) -> Paused st'
        Right (_, NormalTermination) -> Terminated $ NormalTermination
        Right (_, err) -> Terminated err
        Left _ -> Terminated $ ErrorTermination ErrorInSnobol4System $ -1

-- | Evaluate an expression in a paused interpreter
eval :: InterpreterShell m 
     => Expr
     -> PausedInterpreter m
     -> m (PausedInterpreter m, Maybe Data)
eval _ (Terminated err) = return (Terminated err, Nothing)
eval expr (Paused st) = do
    result <- I.interpret st $ do
        result <- I.eval expr
        st' <- I.getProgramState
        return (st', result)
    return $ case result of
        Right (st', (ProgramIncomplete, val)) -> (Paused st', val)
        Right (_, (ErrorTermination err addr, _)) -> (Terminated $ ErrorTermination err addr, Nothing)
        Left _ -> (Terminated $ ErrorTermination ErrorInSnobol4System $ -1, Nothing)

-- | Execute a statement in a paused interpreter
exec :: InterpreterShell m
     => Stmt
     -> PausedInterpreter m
     -> m (PausedInterpreter m, Maybe Data)
exec _ (Terminated err) = return (Terminated err, Nothing)
exec stmt (Paused st) = do
    result <- I.interpret st $ do
        result <- I.exec stmt
        st' <- I.getProgramState
        return (st', result)
    return $ case result of
        Right (st', (ProgramIncomplete, x)) -> (Paused st', x)
        Right (_, (NormalTermination, x)) -> (Terminated $ NormalTermination, x)
        Right (_, (ErrorTermination err addr,_)) -> (Terminated $ ErrorTermination err addr, Nothing)
        Left _ -> (Terminated $ ErrorTermination ErrorInSnobol4System $ -1, Nothing)

-- | Check if a paused interpreter is terminated, and if so, return the error
isTerminated :: PausedInterpreter m -> Maybe ProgramResult
isTerminated (Terminated result) = Just result
isTerminated _ = Nothing
