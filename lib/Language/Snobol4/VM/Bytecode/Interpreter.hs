{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Snobol4.VM.Bytecode.Interpreter where

import Prelude hiding (lookup)

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map as M

import Data.Tuple

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Language.Snobol4.Interpreter.Data hiding (ConcatPattern, Input, Output, Punch)
import qualified Language.Snobol4.Interpreter.Data as Data
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Shell

import qualified Language.Snobol4.Interpreter.Evaluator as Eval

import Language.Snobol4.Parser

import Language.Snobol4.Interpreter.Primitives.Prototypes

import Language.Snobol4.Syntax.AST

import Language.Snobol4.VM.Bytecode
import Language.Snobol4.VM.Bytecode.Interpreter.StackMachine (StackMachine, runStackMachine)
import qualified Language.Snobol4.VM.Bytecode.Interpreter.StackMachine as S
import Language.Snobol4.Interpreter.Internal.StateMachine hiding (Return, FReturn)
import qualified Language.Snobol4.Interpreter.Internal.StateMachine as StMch
import Language.Snobol4.Interpreter.Internal.StateMachine.Types hiding (Return, FReturn)
import Language.Snobol4.Interpreter.Internal.StateMachine.Labels
import Language.Snobol4.Interpreter.Internal.StateMachine.Functions
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.Variables

type EvalError = ()


type PausedInterpreter = PausedInterpreterGeneric CompiledProgram

type ProgramState = ProgramStateGeneric CompiledProgram

type Interpreter m = InterpreterGeneric CompiledProgram (StackMachine m)

type Evaluator m = EvaluatorGeneric CompiledProgram EvalError (StackMachine m)

instance EmptyProgramClass CompiledProgram where
    emptyProgram = CompiledProgram V.empty

instance ProgramClass CompiledProgram where
    type InstructionType CompiledProgram = Instruction
    getInstruction (Address ix) (CompiledProgram is) = is V.! unmkInteger ix

getFailLabel :: (Monad m) => Interpreter m Address
getFailLabel = lift S.getFailLabel

setFailLabel :: ( Monad m) => Address -> Interpreter m ()
setFailLabel = lift . S.setFailLabel

putSystemLabels :: (Monad m) => Vector Address -> Interpreter m ()
putSystemLabels = lift . S.putSystemLabels

lookupSystemLabel :: (Monad m) => SystemLabel -> Interpreter m Address
lookupSystemLabel = lift . S.lookupSystemLabel

push :: Monad m => Data -> Interpreter m ()
push = lift . S.push

pop :: Monad m => Interpreter m Data
pop = lift S.pop >>= \case
    Nothing -> programError ErrorInSnobol4System
    Just x -> return x

popFailStack :: Monad m => Interpreter m ()
popFailStack = lift S.popFailStack

{-
wrapPop :: InterpreterShell m => MaybeT (StackMachine m) Data -> Interpreter m Data
wrapPop f = do
    result <- lift $ StackMachine $ StateT $ \s -> flip runStateT s $ runStackMachine $ runMaybeT f
    case result of
        Just x -> return x
        Nothing -> programError $ ErrorInSnobol4System
-}

{-
spliceStack :: Monad m => InterpreterGeneric CompiledProgram m a -> Interpreter m a
spliceStack f = 
    Interpreter
        $ ExceptT
            $ StateT $ \si -> lift $ runStateT (runExceptT $ runInterpreter f) si                      
-}

instance Snobol4Machine CompiledProgram where
    type EvaluationError CompiledProgram = EvalError
    eval _ = liftEval $ programError ErrorInSnobol4System
    call _ _ = liftEval $ programError ErrorInSnobol4System
    failEval = Evaluator $ lift $ throwE ()

{-
step :: InterpreterShell m => Interpreter m ProgramResult
step = do
    result <- Interpreter $ lift $ runExceptT $ runInterpreter $ stepInst
-}

{-
arithmetic :: InterpreterShell m
           => (Snobol4Integer -> Snobol4Integer -> Snobol4Integer)
           -> (Snobol4Real -> Snobol4Real -> Snobol4Real)
           -> Interpreter m Bool
arithmetic f g = do
    y' <- wrapPop $ pop
    x' <- wrapPop $ pop
    case (x',y') of
        (IntegerData x,IntegerData y) -> push $ IntegerData $ f x y
        (RealData x,RealData y) -> push $ RealData $ g x y
        (IntegerData x,RealData y) -> push $ RealData $ g (mkReal x) y
        (RealData x, IntegerData y) -> push $ RealData $ g x (mkReal y)
        _ -> programError ErrorInSnobol4System
    incProgramCounter
    return False

pattern :: InterpreterShell m
        => (Pattern -> Pattern -> Pattern)
        -> Interpreter m Bool
pattern f = do
    y' <- wrapPop $ pop
    x' <- wrapPop $ pop
    case (x',y') of
        (PatternData x,PatternData y) -> do
            
            push $ TempPatternData $ f x y
        _ -> programError ErrorInSnobol4System
    incProgramCounter
    return False
-}

arithmetic :: InterpreterShell m
           => (Snobol4Integer -> Snobol4Integer -> Snobol4Integer)
           -> (Snobol4Real -> Snobol4Real -> Snobol4Real)
           -> Interpreter m Bool
arithmetic f g = runEvaluation $ do
    y <- liftEval pop
    x <- liftEval pop
    result <- Eval.arithmetic f g x y
    liftEval $ push result
    liftEval $ incProgramCounter
    return False

pattern :: InterpreterShell m
        => (Pattern -> Pattern -> Pattern)
        -> Interpreter m Bool
pattern f = runEvaluation $ do
    y <- liftEval pop
    x <- liftEval pop
    result <- Eval.pattern f x y
    liftEval $ push result
    liftEval $ incProgramCounter
    return False

runEvaluation :: InterpreterShell m => Evaluator m Bool -> Interpreter m Bool
runEvaluation f = do
    catchEval f $ \_ -> do
        addr <- getFailLabel
        putProgramCounter addr
        popFailStack
        return False

exec :: InterpreterShell m => Instruction -> Interpreter m Bool
exec (PushString s) = do
    push $ StringData s
    incProgramCounter
    return False
exec (PushInteger i) = do
    push $ IntegerData i
    incProgramCounter
    return False
exec (PushReal r) = do
    push $ RealData r
    incProgramCounter
    return False
exec Pop = do
    pop
    incProgramCounter
    return False
exec (Copy n) = do
    x <- pop
    replicateM_ (n+1) $ push x
    incProgramCounter
    return False
exec Add = arithmetic (+) (+)
exec Subtract = arithmetic (-) (-)
exec Multiply = arithmetic (*) (*)
exec Divide = arithmetic div (/)
exec Exponentiate = arithmetic (^) (**)
exec ConcatString = do
    StringData y <- pop
    StringData x <- pop
    push $ StringData $ x <> y
    incProgramCounter
    return False

exec ConcatPattern = pattern Data.ConcatPattern
exec AlternatePattern = pattern Data.AlternativePattern

exec GreaterThan = programError ErrorInSnobol4System
exec GreaterThanOrEqualTo = programError ErrorInSnobol4System
exec LessThan = programError ErrorInSnobol4System
exec LessThanOrEqualTo = programError ErrorInSnobol4System
exec Equal = programError ErrorInSnobol4System
exec NotEqual = programError ErrorInSnobol4System

exec PrimitiveAssignmentPattern = programError ErrorInSnobol4System
exec PrimitiveImmediateAssignmentPattern = programError ErrorInSnobol4System
exec PrimitiveLiteralPattern = programError ErrorInSnobol4System
exec PrimitiveAnyPattern = programError ErrorInSnobol4System

exec LookupDynamic = runEvaluation $ do
    item <- liftEval pop
    sym <- toString item
    value <- lookup $ LookupId $ unmkString sym
    liftEval $ do
        push value
        incProgramCounter
    return False
    
exec (LookupStatic (Symbol sym)) = runEvaluation $ do
    value <- lookup $ LookupId $ unmkString sym
    liftEval $ do
        push value
        incProgramCounter
    return False

            
exec (AssignStatic (Symbol sym)) = runEvaluation $ do
    val <- liftEval pop
    assign (LookupId $ unmkString sym) val
    liftEval $ incProgramCounter
    return False
exec (AssignRefStatic (Symbol sym) argCount) = runEvaluation $ do
    val <- liftEval pop
    args <- liftEval $ replicateM argCount pop
    assign (LookupAggregate (unmkString sym) args) val
    liftEval incProgramCounter
    return False

exec AssignDynamic = runEvaluation $ do
    item <- liftEval pop
    sym <- toString item
    value <- liftEval pop
    assign (LookupId $ unmkString sym) value
    liftEval incProgramCounter
    return False

exec Define = runEvaluation $ do
    labelItem <- liftEval pop
    prototypeItem <- liftEval pop
    label <- toString labelItem
    prototype <- toString prototypeItem
    parseResult <- parseT $ unmkString prototype
    FunctionPrototype funcName argNames localNames <- case parseResult of
        Left _ -> liftEval $ programError ErroneousPrototype
        Right func -> return func
    labelResult <- liftEval $ labelLookup label
    Label addr <- case labelResult of
        Just l -> return l
        Nothing -> liftEval $ programError EntryPointOfFunctionNotLabel
    let func = Function funcName argNames localNames addr
    liftEval $ functionsNew func
    liftEval incProgramCounter
    return False
    
exec CallDynamic = programError ErrorInSnobol4System
exec (CallStatic (Symbol sym) argCount) = do
    args <- replicateM argCount pop
    result <- funcLookup sym
    case result of
        Nothing -> programError UndefinedFunctionOrOperation
        Just (UserFunction func) -> callJump func args
        Just _ -> programError ErrorInSnobol4System
    return False
    
exec GetArgCount = programError ErrorInSnobol4System

exec Return = do
    returnJump StMch.Return
    incProgramCounter
    return False
exec FReturn = do
    returnJump StMch.FReturn
    incProgramCounter
    return False

exec (RefStatic (Symbol sym) argCount) = runEvaluation $ do
    args <- liftEval $ replicateM argCount pop
    result <- lookup (LookupAggregate (unmkString sym) args)
    liftEval $ do
        push result
        incProgramCounter
    return False

exec (SetFailLabel lbl) = do
    addr <- lookupSystemLabel lbl
    setFailLabel addr
    incProgramCounter
    return False
exec JumpToFailureLabel = do
    addr <- getFailLabel
    putProgramCounter addr
    popFailStack
    return False
exec JumpToFailureLabelIf = programError ErrorInSnobol4System
exec JumpToFailureLabelElse = programError ErrorInSnobol4System

exec (JumpStatic lbl) = do
    addr <- lookupSystemLabel lbl
    putProgramCounter addr
    return False
exec (JumpStaticIf _) = programError ErrorInSnobol4System
exec (JumpStaticElse _) = programError ErrorInSnobol4System
exec JumpDynamic = runEvaluation $ do
    labelItem <- liftEval pop
    label <- toString labelItem
    liftEval $ do
        result <- labelLookup label
        case result of
            Nothing -> programError UndefinedOrErroneousGoto
            Just (Label addr) -> putProgramCounter addr
            _ -> programError ErrorInSnobol4System
    return False
exec DirectJump = programError ErrorInSnobol4System
exec (BinOp op) = programError ErrorInSnobol4System
exec (UnOp op) = programError ErrorInSnobol4System

exec InvokeScanner = programError ErrorInSnobol4System
exec InvokeReplacer = programError ErrorInSnobol4System

exec ConvertToString = programError ErrorInSnobol4System
exec ConvertToInteger = programError ErrorInSnobol4System
exec ConvertToReal = programError ErrorInSnobol4System
exec ConvertToPattern = programError ErrorInSnobol4System


exec AllocArray = programError ErrorInSnobol4System
exec AllocTable = programError ErrorInSnobol4System


exec (Panic err) = programError err
exec Finish = return True

exec Input = do
    str <- lift $ lift input 
    push $ StringData $ mkString str
    incProgramCounter
    return False
exec Output = runEvaluation $ do
    value <- liftEval pop
    str <- toString value
    lift $ output $ unmkString str
    liftEval incProgramCounter
    return False
exec Punch = runEvaluation $ do
    value <- liftEval pop
    str <- toString value
    lift $ punch $ unmkString str
    liftEval incProgramCounter
    return False
exec LastOutput = do
    str <- lift $ lastOutput
    push $ StringData $ mkString str
    incProgramCounter
    return False
exec LastPunch = do
    str <- lift $ lastPunch
    push $ StringData $ mkString str
    incProgramCounter
    return False

step :: InterpreterShell m => Interpreter m Bool
step = fetch >>= exec


run :: InterpreterShell m => CompiledProgram -> SymbolTable -> m (Maybe ProgramError)
run prog tbl = do
    result <- runStackMachine $ interpret emptyState $ runProg
    case result of
        Left err -> return $ Just err
        Right _ -> return Nothing
  where
    runProg :: InterpreterShell m => Interpreter m (Maybe ProgramError)
    runProg = do
        putProgram prog
        let statics = V.replicate (M.size $ varSymbols tbl) $ StringData nullString
            dynamics = M.fromList $ zip (M.keys $ varSymbols tbl) $ map GlobalVar [0..]
        putVariables $ Variables statics dynamics
        putLabels $ M.map Label $ userLabels tbl
        lbls <- prepareSystemLabels
        putSystemLabels lbls
        loop
    loop :: InterpreterShell m => Interpreter m (Maybe ProgramError)
    loop = do
        result <- step
        if result
            then return Nothing
            else loop
    prepareSystemLabels :: InterpreterShell m => Interpreter m (Vector Address)
    prepareSystemLabels = do
        case sequence $ systemLabels tbl of
            Just m' -> V.generateM (M.size m') $ \ix -> case M.lookup (SystemLabel $ mkInteger ix) m' of
                Nothing -> programError ErrorInSnobol4System
                Just addr -> return addr
            Nothing -> programError ErrorInSnobol4System
        
