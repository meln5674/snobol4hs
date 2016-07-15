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
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.Types
import Language.Snobol4.Interpreter.Scanner

-- | Evaluate a binary operation on data
evalOp :: InterpreterShell m => Operator -> Data -> Data -> Evaluator m Data
evalOp Plus a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a''+b'')
        (RealData a'', RealData b'') -> return $ RealData (a''+b'')
        _ -> liftEval $ programError ProgramError
evalOp Minus a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a''-b'')
        (RealData a'', RealData b'') -> return $ RealData (a''-b'')
        _ -> liftEval $ programError ProgramError
evalOp Star a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a''*b'')
        (RealData a'', RealData b'') -> return $ RealData (a''*b'')
        _ -> liftEval $ programError ProgramError
evalOp Slash a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a'' `div` b'')
        (RealData a'', RealData b'') -> return $ RealData (a'' / b'')
        _ -> liftEval $ programError ProgramError
evalOp Bang a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a'' ^ b'')
        (RealData a'', RealData b'') -> return $ RealData (a'' ** b'')
        _ -> liftEval $ programError ProgramError
evalOp DoubleStar a b = evalOp Bang a b
evalOp Pipe a b = do
    a' <- toPattern a
    b' <- toPattern b
    return $ PatternData $ AlternativePattern a' b'
evalOp Blank a b = do
    a' <- toPattern a
    b' <- toPattern b
    return $ PatternData $ ConcatPattern a' b'

-- | Evaluate an expression
evalExpr :: InterpreterShell m => Expr -> Evaluator m Data
evalExpr (PrefixExpr Not expr) = do
    result <- liftEval $ unliftEval $ evalExpr expr
    case result of
        Right x -> failEvaluation
        Left x -> return $ StringData ""
evalExpr (PrefixExpr Question expr) = do
    result <- liftEval $ unliftEval $ evalExpr expr
    case result of
        Right x -> return $ StringData ""
        Left x -> failEvaluation
evalExpr (PrefixExpr Minus expr) = do
    data_ <- evalExpr expr
    case data_ of
        s@(StringData _) -> do
            i <- toReal s
            case i of
                RealData i -> return $ RealData $ -i
        (IntegerData i) -> return $ IntegerData $ -i
        (RealData r) -> return $ RealData $ -r
           
-- TODO: UnevaluatedExpr
evalExpr (IdExpr "INPUT") = StringData <$> (lift $ input)
evalExpr (IdExpr "OUTPUT") = StringData <$> (lift $ lastOutput)
evalExpr (IdExpr "PUNCH") = StringData <$> (lift $ lastPunch)
evalExpr (IdExpr name) = do
    d <- liftEval $ varLookup name
    case d of
        Just d -> return d
        Nothing -> failEvaluation
evalExpr (LitExpr (Int i)) = return $ IntegerData i
evalExpr (LitExpr (Real r)) = return $ RealData r
evalExpr (LitExpr (String s)) = return $ StringData s
-- TODO: CallExpr
-- TODO: RefExpr
evalExpr (ParenExpr expr) = evalExpr expr
evalExpr (BinaryExpr a op b) = do
    a' <- evalExpr a
    b' <- evalExpr b
    evalOp op a' b'
evalExpr NullExpr = return $ StringData ""
evalExpr _ = liftEval $ programError ProgramError

execLookup :: InterpreterShell m => Lookup -> Evaluator m (Maybe Data)
execLookup Input = (Just . StringData) <$> lift input
execLookup Output = (Just . StringData) <$> lift lastOutput
execLookup Punch = (Just . StringData) <$> lift lastPunch
execLookup (Lookup i) = liftEval $ varLookup i
    

-- Execute a subject and return the lookup for it
execSub :: InterpreterShell m => Expr -> Evaluator m Lookup
execSub (IdExpr "INPUT") = return $ Input
execSub (IdExpr "OUTPUT") = return $ Output
execSub (IdExpr "PUNCH") = return $ Punch
execSub (IdExpr s) = return $ Lookup s
execSub (PrefixExpr Dollar expr) = do
    expr' <- evalExpr expr
    StringData s <- toString expr'
    return $ Lookup s
execSub _ = liftEval $ programError ProgramError


{-

idea: using parsec for pattern evaluation


user state: list of pairs of Lookups and strings

assignment: create parser which applies pattern and adds a 

-}



-- Execute a pattern, and return the pattern structure for it
execPat :: InterpreterShell m => Expr -> Evaluator m Pattern
execPat = evalExpr >=> toPattern

-- Execute a replacement on a subject and pattern with an object
execRepl :: InterpreterShell m => Lookup -> Pattern -> Expr -> Evaluator m ()
execRepl l EverythingPattern expr = do
    val <- evalExpr expr
    assign l val
-- TODO: Replacements on patterns
execRepl _ _ _ = liftEval $ programError ProgramError

-- Execute a goto
execGoto :: InterpreterShell m => EvalStop -> Goto -> Evaluator m ()
execGoto _ _ = liftEval $ programError ProgramError

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
                Just d -> do
                    case pattern of
                        EverythingPattern -> return $ Just d
                        pattern -> do
                            StringData str <- toString d
                            scanResult <- scanPattern str pattern
                            case scanResult of
                                NoScan -> failEvaluation
                                Scan match assignments -> do
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
