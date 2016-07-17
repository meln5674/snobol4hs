{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module          : Language.Snobol4.Interpreter.Internal
Description     : Interpreter internals
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

An overview of the interpreter:

Actions by the interpreter are represented by the 'Interpreter' type, which is a 
monad transformer, hiding a stack of ExceptT and StateT. The exceptions thrown 
are program terminations, either due to an illegal operation or the end of the 
program being reached. The state held contains the variables bound, the 
statements loaded, the index of the index to execute/being executed, and the 
bound labels.

The interpreter works on the REPL princible, it first fetches the statement 
pointed to by the program counter, evaluates it, updates variables as needed, 
then sets the program counter to point to either the next statement or the 
statement pointed to by a goto.

In evaluating a statement, the Interpreter defers to the 'Evaluator' type, which 
again is a monad transformer, hiding a stack of two ExceptT's and a StateT. The 
outer ExceptT and the StateT are the same as in 'Interpreter'. The inner ExceptT 
handles exception of type 'EvalStop'. These are thrown when the evaluation needs 
to be halted, and signals success or failure. If a statement has no subject, 
this is thrown immediately. If an evaluation should fail in a handlable way, 
such as a pattern exhaustively failed, such an exception is thrown signalling 
failure. These are caught, and used to determine which, if any, of the goto's 
are to be used. The 'Evaluator' can throw program termination exceptions just as 
'Interpreter' can, and work the same way. Once the evaluation is complete, 
control is transfered back to the 'Interpreter' stack.
-}

module Language.Snobol4.Interpreter.Internal 
    ( module Language.Snobol4.Interpreter.Internal
    , module Language.Snobol4.Interpreter.Internal.Types
    , module Language.Snobol4.Interpreter.Scanner
    , module Language.Snobol4.Interpreter.Evaluator
    ) where

import Prelude hiding (toInteger)

import Text.Read hiding (lift, String, step, get)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Evaluator
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.Types
import Language.Snobol4.Interpreter.Scanner


-- Execute a subject and return the lookup for it
execSub :: InterpreterShell m => Expr -> Evaluator m Lookup
execSub expr@(LitExpr _) = LookupLiteral <$> evalExpr expr
execSub (IdExpr "INPUT") = return $ Input
execSub (IdExpr "OUTPUT") = return $ Output
execSub (IdExpr "PUNCH") = return $ Punch
execSub (IdExpr s) = return $ LookupId s
execSub (PrefixExpr Dollar expr) = do
    expr' <- evalExpr expr
    StringData s <- toString expr'
    return $ LookupId s
execSub (RefExpr s args) = LookupAggregate s <$> mapM evalExpr args
execSub _ = liftEval $ programError ProgramError


-- Execute a pattern, and return the pattern structure for it
execPat :: InterpreterShell m => Expr -> Evaluator m Pattern
execPat = evalExpr >=> toPattern

-- Execute a replacement on a subject and pattern with an object
execRepl :: InterpreterShell m => Lookup -> Pattern -> Expr -> Evaluator m ()
execRepl lookup pattern expr = do
    repl <- evalExpr expr
    case pattern of
        EverythingPattern -> assign lookup repl
        pattern -> do
            val <- execLookup lookup
            case val of
                Nothing -> liftEval $ programError ProgramError
                Just d -> do
                    StringData str <- toString d
                    scanResult <- scanPattern str pattern
                    case scanResult of
                        NoScan -> failEvaluation
                        Scan match assignments startIndex endIndex -> do
                            mapM_ (uncurry assign) assignments
                            StringData replStr <- toString repl
                            let val' = take startIndex str ++ replStr ++ drop endIndex str
                            assign lookup $ StringData val'
                            finishEvaluation $ Nothing

goto :: InterpreterShell m => Expr -> Evaluator m ()
goto expr = do
    result <- evalExpr expr
    StringData label <- toString result
    pc <- liftEval $ labelLookup label
    case pc of
        Nothing -> liftEval $ programError ProgramError
        Just pc -> liftEval $ putProgramCounter pc

-- Execute a goto
execGoto :: InterpreterShell m => EvalStop -> Goto -> Evaluator m ()
execGoto _ (Goto expr) = goto expr
execGoto (EvalSuccess _) (SuccessGoto expr) = goto expr
execGoto EvalFailed (FailGoto expr) = goto expr
execGoto (EvalSuccess _) (BothGoto expr _) = goto expr
execGoto EvalFailed (BothGoto _ expr) = goto expr
execGoto _ _ = liftEval $ modifyProgramCounter (+1)

-- Execute one of the steps above, ignoring if it is missing
execMaybe :: Monad m 
          => (x -> m y) 
          -> Maybe x 
          -> m (Maybe y)
execMaybe f (Just x) = Just <$> f x
execMaybe _ _ = return Nothing

-- | Take an evaluation and return it to the interpreter stack, with a handler 
-- for a failed evaluation
catchEval :: InterpreterShell m 
          => Evaluator m a 
          -> (EvalStop -> Interpreter m a)
          -> Interpreter m a
catchEval m h = do
    result <- unliftEval m
    case result of
        Right val -> return val
        Left stop -> h stop

-- | Execute a statement in the interpreter
exec :: InterpreterShell m => Stmt -> Interpreter m (Maybe Data)
exec (EndStmt _) = programError NormalTermination
exec (Stmt _ sub pat obj go) = flip catchEval handler $ do
    subResult <- execMaybe execSub sub
    lookup <- case subResult of
        Just lookup -> return lookup
        Nothing -> finishEvaluation Nothing
    patResult <- execMaybe execPat pat
    pattern <- case patResult of
        Just pattern -> return pattern
        Nothing -> return EverythingPattern
    replResult <- execMaybe (execRepl lookup pattern) obj
    case replResult of
        Just _ -> finishEvaluation Nothing
        Nothing -> do
            val <- execLookup lookup
            case val of
                Nothing -> liftEval $ programError ProgramError
                Just d -> case pattern of
                    EverythingPattern -> finishEvaluation $ Just d
                    pattern -> do
                        StringData str <- toString d
                        scanResult <- scanPattern str pattern
                        case scanResult of
                            NoScan -> failEvaluation
                            Scan match assignments _ _ -> do
                                mapM_ (uncurry assign) assignments
                                finishEvaluation $ Just match
  where
    handler :: InterpreterShell m => EvalStop -> Interpreter m (Maybe Data)
    handler r = do
        gotoResult <- catchEval (execMaybe (execGoto r) go) 
                   $ \_ -> programError ProgramError
        case gotoResult of
            Just _ -> return ()
            Nothing -> modifyProgramCounter (+1)
        case r of
            EvalFailed -> return Nothing
            EvalSuccess x -> return x
            

                        
                        
-- | Execute the next statement pointed to by the program counter
step :: InterpreterShell m => Interpreter m (Maybe Data)
step = fetch >>= exec

-- | Load a program into the interpreter
load :: InterpreterShell m => Program -> Interpreter m ()
load stmts = putStatements $ V.fromList stmts

-- | Run the interpreter continuously by fetching the next statement 
-- until the program ends
run :: InterpreterShell m => Interpreter m ProgramError
run = do
    result <- Interpreter $ lift $ runExceptT $ runInterpreter $ step
    case result of
        Right _ -> run
        Left err -> return err
    
-- | Execute an interpreter action
interpret :: InterpreterShell m 
          => ProgramState 
          -> Interpreter m a 
          -> m (Either ProgramError a)
interpret st m = flip evalStateT st
        $ runExceptT 
        $ runInterpreter m
