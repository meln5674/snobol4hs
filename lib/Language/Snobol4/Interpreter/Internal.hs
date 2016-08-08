{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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

{-# LANGUAGE LambdaCase #-}
module Language.Snobol4.Interpreter.Internal 
    ( module Language.Snobol4.Interpreter.Scanner
    , module Language.Snobol4.Interpreter.Evaluator
    , Interpreter
    , PausedInterpreter (..)
    , ExecResult (..)
    , liftEval
    , unliftEval
    , interpret
    , emptyState
    , getProgramState
    , eval
    , run
    , step
    , exec
    , call
    , evalExpr
    , load
    ) where

import Prelude hiding ( toInteger, lookup )

import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import Language.Snobol4.Syntax.AST

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Evaluator
import Language.Snobol4.Interpreter.Scanner
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Primitives (addPrimitives)

import Language.Snobol4.Interpreter.Internal.StateMachine
import Language.Snobol4.Interpreter.Internal.StateMachine.Statements
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Functions
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.Labels
import Language.Snobol4.Interpreter.Internal.StateMachine.Run

-- | Evaluate an expression, then choose one of two actions/values to use
checkSuccess :: InterpreterShell m 
             => Expr -- ^ Expression to evaluate
             -> Evaluator m Data -- ^ Action on success
             -> Evaluator m Data -- ^ Action on failure
             -> Evaluator m Data
checkSuccess expr success failure = do
    result <- liftEval $ unliftEval $ evalExpr expr
    case result of
        Right _ -> success
        Left _ -> failure

-- | Evaluate an expression as if it were an L-Value
evalLookup :: InterpreterShell m => Expr -> Evaluator m Lookup
evalLookup expr@(LitExpr _) = LookupLiteral <$> evalExpr expr
evalLookup (IdExpr "INPUT") = return $ Input
evalLookup (IdExpr "OUTPUT") = return $ Output
evalLookup (IdExpr "PUNCH") = return $ Punch
evalLookup (IdExpr s) = return $ LookupId $ mkString s
evalLookup (PrefixExpr Dollar expr) = do
    expr' <- evalExpr expr
    s <- toString expr'
    return $ LookupId s
evalLookup (RefExpr s args) = LookupAggregate (mkString s) <$> mapM evalExpr args
evalLookup (ParenExpr expr) = evalLookup expr
evalLookup expr = LookupLiteral <$> evalExpr expr

-- | Evaluate an expression as if it were an R-value
evalExpr :: InterpreterShell m => Expr -> Evaluator m Data
evalExpr (PrefixExpr Not expr) = checkSuccess 
    expr 
    failEvaluation 
    (return $ StringData nullString)
evalExpr (PrefixExpr Question expr) = checkSuccess 
    expr 
    (return $ StringData nullString)
    failEvaluation
evalExpr (PrefixExpr Minus expr) = do
    data_ <- evalExpr expr
    case data_ of
        s@(StringData _) -> do
            r <- toReal s
            return $ RealData $ -r
        (IntegerData i) -> return $ IntegerData $ -i
        (RealData r) -> return $ RealData $ -r
        _ -> liftEval $ programError IllegalDataType
evalExpr (PrefixExpr Star expr) = return $ TempPatternData $ UnevaluatedExprPattern expr
evalExpr (PrefixExpr Dot (IdExpr name)) = return $ Name $ LookupId $ mkString name
evalExpr (PrefixExpr Dollar expr) = do
    expr' <- evalExpr expr
    s <- toString expr'
    result <- liftEval $ execLookup $ LookupId s
    case result of
        Just d -> return d
        Nothing -> failEvaluation
evalExpr (IdExpr "INPUT") = StringData <$> (lift $ mkString <$> input)
evalExpr (IdExpr "OUTPUT") = StringData <$> (lift $ mkString <$> lastOutput)
evalExpr (IdExpr "PUNCH") = StringData <$> (lift $ mkString <$> lastPunch)
evalExpr (IdExpr name) = do
    lookupResult <- liftEval $ execLookup $ LookupId $ mkString name
    case lookupResult of
        Just val -> return val
        Nothing -> failEvaluation
evalExpr (LitExpr (Int i)) = return $ IntegerData $ mkInteger i
evalExpr (LitExpr (Real r)) = return $ RealData $ mkReal r
evalExpr (LitExpr (String s)) = return $ StringData $ mkString s
evalExpr (CallExpr name args) = do
    args' <- mapM evalExpr args
    callResult <- liftEval $ call (mkString name) args'
    case callResult of
        Just val -> return val
        Nothing -> failEvaluation
evalExpr (RefExpr name args) = do
    args' <- mapM evalExpr args
    lookupResult <- liftEval $ execLookup $ LookupAggregate (mkString name) args'
    case lookupResult of
        Just val -> return val
        Nothing -> liftEval $ programError ErroneousArrayOrTableReference
evalExpr (ParenExpr expr) = evalExpr expr
evalExpr (BinaryExpr a op b) = do
    a' <- evalExpr a
    b' <- evalExpr b
    evalOp op a' b'
evalExpr NullExpr = return $ StringData nullString
evalExpr _ = liftEval $ programError ErrorInSnobol4System

-- | Call a function by name with arguments
call :: InterpreterShell m => Snobol4String -> [Data] -> Interpreter m (Maybe Data)
call n args = callFunction n args loop
  where
    loop = stepStmt >>= \case
        StmtResult _ -> loop
        returnResult -> return returnResult

-- | Execute a subject and return the lookup for it
execSub :: InterpreterShell m => Expr -> Evaluator m Lookup
execSub = evalLookup

-- | Execute a pattern, and return the pattern structure for it
execPat :: InterpreterShell m => Expr -> Evaluator m Pattern
execPat = evalExpr >=> toPattern

-- | Execute a replacement on a subject and pattern with an object
execRepl :: InterpreterShell m => Lookup -> Pattern -> Expr -> Evaluator m ()
execRepl lookup pattern expr = do
    repl <- evalExpr expr
    case pattern of
        EverythingPattern -> assign lookup repl
        _ -> do
            val <- liftEval $ execLookup lookup
            str <- case val of
                Nothing -> return nullString
                Just d -> toString d
            scanResult <- scanPattern str pattern
            case scanResult of
                NoScan -> failEvaluation
                Scan _ assignments startIndex endIndex -> do
                    mapM_ (uncurry assign) assignments
                    replStr <- toString repl
                    let val' = snobol4Take startIndex str 
                               <> replStr 
                               <> snobol4Drop endIndex str
                    assign lookup $ StringData val'
                    finishEvaluation $ Nothing

-- | Evaluate a goto and jump to the appropriate address
goto :: InterpreterShell m => Expr -> Evaluator m GotoResult
goto (IdExpr "RETURN") = return GotoReturn
goto (IdExpr "FRETURN") = return GotoFReturn
goto (IdExpr label) = liftEval $ do
    lookupResult <- labelLookup $ mkString label
    case lookupResult of
        Nothing -> programError UndefinedOrErroneousGoto
        Just (Label pc) -> do
            putProgramCounter pc
            return GotoLabel
        Just (CodeLabel k pc) -> undefined
goto (PrefixExpr Dollar expr) = do
    result <- evalExpr expr
    label <- toString result
    goto (IdExpr $ unmkString label)
goto expr = do
    result <- evalExpr expr
    label <- toString result
    goto (IdExpr $ unmkString label)

data GotoResult
    = GotoReturn
    | GotoFReturn
    | GotoLabel
    | GotoNext
  deriving Show

-- | Evaluate an expression and to a direct jump to object code specified by
-- the result
directGoto :: InterpreterShell m => Expr -> Evaluator m GotoResult
directGoto expr = do
    result <- evalExpr expr
    code <- toCode result
    undefined

-- | Execute either a normal or direct goto
execGotoPart :: InterpreterShell m => GotoPart -> Evaluator m GotoResult
execGotoPart (GotoPart expr) = goto expr
execGotoPart (DirectGotoPart expr) = directGoto expr

-- | Execute a goto
execGoto :: InterpreterShell m => EvalStop -> Goto -> Evaluator m GotoResult
execGoto _ (Goto g) = execGotoPart g
execGoto (EvalSuccess _) (SuccessGoto g) = execGotoPart g
execGoto EvalFailed (FailGoto g) = execGotoPart g
execGoto (EvalSuccess _) (BothGoto g _) = execGotoPart g
execGoto EvalFailed (BothGoto _ g) = execGotoPart g
execGoto _ _ = do
    liftEval $ modifyProgramCounter (+1)
    return GotoNext

-- | Execute one of the steps above, ignoring if it is missing
execMaybe :: Monad m 
          => (x -> m y) 
          -> Maybe x 
          -> m (Maybe y)
execMaybe f (Just x) = Just <$> f x
execMaybe _ _ = return Nothing

-- | Execute a statement in the interpreter
execStmt :: InterpreterShell m => Stmt -> Interpreter m ExecResult
execStmt (EndStmt _) = return EndOfProgram
execStmt (Stmt _ sub pat obj go) = flip catchEval handler $ do
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
            lookupResult <- liftEval $ execLookup lookup
            str <- case lookupResult of
                Nothing -> return ""
                Just val -> case pattern of
                    EverythingPattern -> finishEvaluation $ Just val
                    _ -> toString val
            scanResult <- scanPattern str pattern
            case scanResult of
                NoScan -> failEvaluation
                Scan match assignments _ _ -> do
                    mapM_ (uncurry assign) assignments
                    finishEvaluation $ Just match
  where
    handler :: InterpreterShell m => EvalStop -> Interpreter m ExecResult
    handler r = do
        gotoResult <- catchEval (execMaybe (execGoto r) go) 
                   $ \_ -> programError FailureDuringGotoEvaluation
        case gotoResult of
            Just GotoReturn -> return Return
            Just GotoFReturn -> return FReturn
            _ -> do
                case gotoResult of
                    Nothing -> modifyProgramCounter (+1)
                    _ -> return ()
                case r of
                    EvalFailed -> return $ StmtResult Nothing
                    EvalSuccess x -> return $ StmtResult x

-- | Execute a statement
exec :: InterpreterShell m => Stmt -> Interpreter m (ProgramResult, Maybe Data)
exec stmt = do
    result <- Interpreter $ lift $ runExceptT $ runInterpreter $ execStmt stmt
    presult <- foo result
    return $ case result of
        (Right (StmtResult val)) -> (presult, val)
        _ -> (presult, Nothing)
    
foo :: InterpreterShell m => Either ProgramError ExecResult -> Interpreter m ProgramResult
foo (Right (StmtResult _)) = return ProgramIncomplete
foo (Right Return) = do
    Address pc <- getProgramCounter
    return $ ErrorTermination ReturnFromZeroLevel $ unmkInteger pc
foo (Right FReturn) = do
    Address pc <- getProgramCounter
    return $ ErrorTermination ReturnFromZeroLevel $ unmkInteger pc
foo (Right EndOfProgram) = return $ NormalTermination
foo (Left err) = do
    Address pc <- getProgramCounter
    return $ ErrorTermination err $ unmkInteger pc
                        
-- | Execute the next statement pointed to by the program counter
stepStmt :: InterpreterShell m => Interpreter m ExecResult
stepStmt = fetch >>= execStmt

-- | Execute the next statement in the program
step :: InterpreterShell m => Interpreter m ProgramResult
step = do
    result <- Interpreter $ lift $ runExceptT $ runInterpreter $ stepStmt
    foo result

-- | Evaluate an expression
eval :: InterpreterShell m => Expr -> Interpreter m (ProgramResult, Maybe Data)
eval expr = do
    result <- Interpreter $ lift $ runExceptT $ runInterpreter $ unliftEval $ evalExpr expr
    case result of
        Right (Right v) -> return (ProgramIncomplete, Just v)
        Right (Left (EvalSuccess _)) -> do
            Address pc <- getProgramCounter
            return (ErrorTermination ErrorInSnobol4System $ unmkInteger pc, Nothing)
        Right (Left EvalFailed) -> return (ProgramIncomplete, Nothing)
        Left err -> do
            Address pc <- getProgramCounter
            return (ErrorTermination err $ unmkInteger pc, Nothing)
        

-- | Load a program into the interpreter
load :: InterpreterShell m => Program -> Interpreter m ()
load (Program stmts) = do
    let prog = V.fromList stmts
    addPrimitives
    putStatements prog
    scanForLabels
    unless (V.length prog == 0) $ 
        case V.last prog of
            EndStmt Nothing -> putProgramCounter 0
            EndStmt (Just lbl) -> do
                result <- labelLookup $ mkString lbl
                case result of
                    Just (Label addr) -> putProgramCounter addr
                    Nothing -> programError UndefinedOrErroneousGoto
            

-- | Run the interpreter continuously by fetching the next statement 
-- until the program ends
run :: InterpreterShell m => Interpreter m ProgramResult
run = do
    result <- Interpreter $ lift $ runExceptT $ runInterpreter $ stepStmt
    result' <- foo result
    case result' of
        ProgramIncomplete -> run
        _ -> return result'
    
