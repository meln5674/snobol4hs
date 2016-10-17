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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Language.Snobol4.Interpreter.Internal.Types 
import Language.Snobol4.Interpreter.Internal.CallStack
import Language.Snobol4.Interpreter.Internal.StackMachine
import Language.Snobol4.Interpreter.Internal.StateMachine hiding (call, eval)
import Language.Snobol4.Interpreter.Internal.StateMachine.Functions
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Variables
import qualified Language.Snobol4.Interpreter.Internal.StateMachine as StMch

callUnOp :: ( InterpreterShell m
            {-, Snobol4Machine program-}
            , LocalVariablesClass m
            ) 
         => Operator
         -> ArgType m
         -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
callUnOp op args = do
    lookupResult <- lookupUnOpSyn op
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just (PrimitiveOperator action) -> action args
        Just (OperatorOperatorSynonym op') -> callUnOp op' args
        Just (OperatorFunctionSynonym func) -> call func args



callBinOp :: ( InterpreterShell m
             {-, Snobol4Machine program-}
             , LocalVariablesClass m
             ) 
         => Operator
         -> [Data]
         -> InterpreterGeneric program m (Maybe Data)
callBinOp op args = do
    lookupResult <- lookupBinOpSyn op
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just (PrimitiveOperator action) ->
            catchEval (liftM Just $ action args) $ const $ return Nothing
        Just (OperatorOperatorSynonym op') -> callBinOp op' args
        Just (OperatorFunctionSynonym func) -> 
            catchEval (call (getFuncName func) args) $ const $ return Nothing


callJump :: ( InterpreterShell m, LocalVariablesClass m, CallStackClass m )
         => UserFunction -- ^ Name of the function
         -> [Data] -- ^ Arguments to pass
         -> InterpreterGeneric Statements m ()
callJump func@Function{funcName, formalArgs, localNames} evaldArgs = do
    -- Push current state onto the stack and create blank spots for
    --  args and locals
    pushFuncFrame func

    let allLocalNames = funcName : formalArgs ++ localNames

    -- Capture the references held by the names of the return variable,
    --  locals, and args
    oldReferences <- forM allLocalNames $ \name -> do
        ref <- getReference name
        return (name, ref)
    modifyCallStackHead $ \n -> n{ oldReferences }
    -- Set the function name, arguments, and local variables to
    --  reference the appropriate spot in the new stack frame
    mapM_ (uncurry setReference) $ zip allLocalNames $ map (Just . LocalVar) [0..]
    mapM_ (uncurry varWrite) $ zip formalArgs evaldArgs
    -- Jump to the entry point of the function and execute the passed function
    putProgramCounter $ entryPoint func



returnJump :: ( InterpreterShell m, LocalVariablesClass m, CallStackClass m )
           => ExecResult
           -> InterpreterGeneric Statements m (Maybe Data)
returnJump result = do
    -- Get the return value, and if the the function failed or not
    toReturn <- case result of
        Return -> liftM Just $ localLookup 0
        FReturn -> return Nothing
    Frame{oldReferences} <- popCallStack
    -- Reset the references captured earlier
    forM_ oldReferences $ uncurry setReference
    return toReturn

callFunction' :: ( InterpreterShell m
                 , LocalVariablesClass m
                 , CallStackClass m
                 )
              => Function Statements m
              -> [Data]
              -> InterpreterGeneric Statements m ExecResult
              -> InterpreterGeneric Statements m (Maybe Data)
callFunction' PrimitiveFunction{funcPrim=action} evaldArgs _ = do
    catchEval (action evaldArgs) $ const $ return Nothing
callFunction' (FunctionUnOperatorSynonym _ op) evaldArgs _ = callUnOp op evaldArgs
callFunction' (FunctionBinOperatorSynonym _ op) evaldArgs _ = callBinOp op evaldArgs
callFunction' (UserFunction func) evaldArgs f = do
    callJump func evaldArgs
    result <- f
    returnJump result

-- | Call a function
callFunction :: ( InterpreterShell m
                , LocalVariablesClass m
                , CallStackClass m
                ) 
             => Snobol4String -- ^ Name of the function
             -> [Data] -- ^ Arguments to pass
             -> InterpreterGeneric Statements m ExecResult -- ^ Action to be performed between
                                         -- pushing arguments and popping result
             -> InterpreterGeneric Statements m (Maybe Data)
callFunction name evaldArgs f = do
    lookupResult <- funcLookup name
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just func -> callFunction' func evaldArgs f
        



-- | Mark the current evaluation as failed and prohibit additional evaluation
--failEvaluation :: Monad m => Evaluator m a
failEvaluation :: Monad m => EvaluatorGeneric program EvalStop m a
failEvaluation = Evaluator
               $ lift 
               $ throwE 
                 EvalFailed

-- | Mark the current evaluation as successful and prohibit additional evaluation
--finishEvaluation :: Monad m => Maybe Data -> Evaluator m a
finishEvaluation :: Monad m => Maybe Data -> EvaluatorGeneric program EvalStop m a
finishEvaluation = Evaluator
                 . lift 
                 . throwE
                 . EvalSuccess


-- | Call a function by name with arguments
call :: ( InterpreterShell m
        , LocalVariablesClass m
        , CallStackClass m
        ) 
     => Snobol4String 
     -> [Data] 
     -> InterpreterGeneric Statements m (Maybe Data)
call n args = callFunction n args loop
  where
    loop = stepStmt >>= \case
        StmtResult _ -> loop
        returnResult -> return returnResult

-- | Execute a subject and return the lookup for it
--execSub :: InterpreterShell m => Expr -> Evaluator m Lookup
execSub :: ( InterpreterShell m 
           , CallStackClass m
           , LocalVariablesClass m
           )
        => Expr 
        -> EvaluatorGeneric Statements EvalStop m Lookup
execSub = evalLookup

-- | Execute a pattern, and return the pattern structure for it
--execPat :: InterpreterShell m => Expr -> Evaluator m Pattern
execPat :: ( InterpreterShell m 
           , CallStackClass m
           , LocalVariablesClass m
           )
        => Expr 
        -> EvaluatorGeneric Statements EvalStop m Pattern
execPat = evalExpr >=> toPattern

-- | Execute a replacement on a subject and pattern with an object
--execRepl :: InterpreterShell m => Lookup -> Pattern -> Expr -> Evaluator m ()
execRepl :: ( InterpreterShell m 
            , CallStackClass m
            , LocalVariablesClass m
            )
         => Lookup 
         -> Pattern 
         -> Expr 
         -> EvaluatorGeneric Statements EvalStop m ()
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
--goto :: InterpreterShell m => Expr -> Evaluator m GotoResult
goto :: ( InterpreterShell m 
        , LocalVariablesClass m
        , CallStackClass m
        )
     => Expr 
     -> EvaluatorGeneric Statements EvalStop m GotoResult
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
--directGoto :: InterpreterShell m => Expr -> Evaluator m GotoResult
directGoto :: ( InterpreterShell m 
              , CallStackClass m
              , LocalVariablesClass m
              )
           => Expr 
           -> EvaluatorGeneric Statements EvalStop m GotoResult
directGoto expr = do
    result <- evalExpr expr
    code <- toCode result
    undefined

-- | Execute either a normal or direct goto
--execGotoPart :: InterpreterShell m => GotoPart -> Evaluator m GotoResult
execGotoPart :: ( InterpreterShell m 
                , CallStackClass m
                , LocalVariablesClass m
                )
             => GotoPart 
             -> EvaluatorGeneric Statements EvalStop m GotoResult
execGotoPart (GotoPart expr) = goto expr
execGotoPart (DirectGotoPart expr) = directGoto expr

-- | Execute a goto
--execGoto :: InterpreterShell m => EvalStop -> Goto -> Evaluator m GotoResult
execGoto :: ( InterpreterShell m 
            , CallStackClass m
            , LocalVariablesClass m
            )
         => EvalStop -> Goto -> EvaluatorGeneric Statements EvalStop m GotoResult
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
--execStmt :: InterpreterShell m => Stmt -> Interpreter m ExecResult
execStmt :: ( InterpreterShell m
            , LocalVariablesClass m
            , CallStackClass m
            )
         => Stmt
         -> InterpreterGeneric Statements m ExecResult
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
    --handler :: InterpreterShell m => EvalStop -> Interpreter m ExecResult
    handler :: ( InterpreterShell m 
               , LocalVariablesClass m
               , CallStackClass m
               ) 
            => EvalStop 
            -> InterpreterGeneric Statements m ExecResult
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
--stepStmt :: InterpreterShell m => Interpreter m ExecResult
stepStmt :: ( InterpreterShell m
            , LocalVariablesClass m
            , CallStackClass m
            )
         => InterpreterGeneric Statements m ExecResult
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

instance Snobol4Machine Statements where
    type EvaluationError Statements = EvalStop
    call n args = liftEval $ Language.Snobol4.Interpreter.Internal.call n args
    eval = evalExpr
    failEval = failEvaluation


-- | Load a program into the interpreter
load :: InterpreterShell m => Program -> Interpreter m ()
load (Program stmts) = do
    let prog = V.fromList stmts
    addPrimitives
    putProgram $ Statements prog
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
