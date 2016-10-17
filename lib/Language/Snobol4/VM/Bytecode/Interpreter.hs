{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Snobol4.VM.Bytecode.Interpreter 
    ( module Language.Snobol4.VM.Bytecode.Interpreter 
    , InterpreterGeneric(Interpreter)
    , getProgram
    , getProgramCounter
    , getCallStackFrameStart
    ) where

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

import Language.Snobol4.Interpreter.Primitives
import Language.Snobol4.Interpreter.Primitives.Prototypes

import Language.Snobol4.Interpreter.Scanner

import Language.Snobol4.Syntax.AST hiding (getProgram)

import Language.Snobol4.VM.Bytecode
import Language.Snobol4.VM.Bytecode.Interpreter.StackMachine (StackMachine, runStackMachine, ExprKey)
import qualified Language.Snobol4.VM.Bytecode.Interpreter.StackMachine as S
import Language.Snobol4.Interpreter.Internal.StateMachine hiding (Return, FReturn)
import qualified Language.Snobol4.Interpreter.Internal.StateMachine as StMch
import Language.Snobol4.Interpreter.Internal.StateMachine.Types hiding (Return, FReturn)
import Language.Snobol4.Interpreter.Internal.StateMachine.Labels
import Language.Snobol4.Interpreter.Internal.StateMachine.Functions
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.Statements
import Language.Snobol4.Interpreter.Internal.StateMachine.Variables
import Language.Snobol4.Interpreter.Internal.StateMachine.Patterns

import Language.Snobol4.VM.Bytecode.Interpreter.Types


type EvalError = ()

type PausedInterpreter = PausedInterpreterGeneric CompiledProgram

type ProgramState = ProgramStateGeneric CompiledProgram

type Interpreter m = InterpreterGeneric CompiledProgram (StackMachine ExprKey m)

--type Evaluator m = EvaluatorGeneric CompiledProgram EvalError (StackMachine m)

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

push :: Monad m => Data ExprKey -> Interpreter m ()
push = lift . S.push

pop :: Monad m => Interpreter m (Data ExprKey)
pop = lift S.pop >>= \case
    Nothing -> programError ErrorInSnobol4System
    Just x -> return x

popFailStack :: Monad m => Interpreter m ()
popFailStack = lift S.popFailStack

popToCallStackFrame = lift S.popToCallStackFrame

popCallStackFrame = lift S.popCallStackFrame

pushCallStackFrame = lift . S.pushCallStackFrame

getCallStackFrameStart = lift S.getCallStackFrameStart

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

{-
instance Snobol4Machine CompiledProgram where
    type EvaluationError CompiledProgram = EvalError
    eval _ = liftEval $ programError ErrorInSnobol4System
    call _ _ = liftEval $ programError ErrorInSnobol4System
    failEval = Evaluator $ lift $ throwE ()
-}

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
arithmetic f g = do
    y <- pop
    x <- pop
    result <- Eval.arithmetic f g x y
    push result
    incProgramCounter
    return False

pattern :: InterpreterShell m
        => (Pattern ExprKey -> Pattern ExprKey -> Pattern ExprKey)
        -> Interpreter m Bool
pattern f = do
    y <- pop
    x <- pop
    result <- Eval.pattern f x y
    push result
    incProgramCounter
    return False

{-
runEvaluation :: InterpreterShell m => Evaluator m Bool -> Interpreter m Bool
runEvaluation f = do
    catchEval f $ \_ -> do
        addr <- getFailLabel
        putProgramCounter addr
        popFailStack
        return False
-}

pushReference name = do
    ref <- getReference name
    case ref of
        Nothing -> push (IntegerData 0) >> push (IntegerData 0)
        Just (LocalVar ix)-> push (IntegerData $ mkInteger ix) >> push (IntegerData 1)
        Just (GlobalVar ix) -> push (IntegerData $ mkInteger ix) >> push (IntegerData 2)
    push $ StringData name
popReference = do
    StringData name <- pop
    IntegerData typeField <- pop
    ref <- case typeField of
        0 -> pop >> return Nothing
        1 -> do
            IntegerData ix <- pop
            return $ Just $ LocalVar $ unmkInteger ix
        2 -> do
            IntegerData ix <- pop
            return $ Just $ GlobalVar $ unmkInteger ix
    setReference name ref


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

exec LookupDynamic = {-runEvaluation $-} do
    item <- {-liftEval-} pop
    sym <- {-liftEval $-} toString item
    value <- lookup $ LookupId $ unmkString sym
    {-liftEval $-} --do
    push value
    incProgramCounter
    return False
    
exec (LookupStatic (Symbol sym)) = {-runEvaluation $-} do
    value <- lookup $ LookupId $ unmkString sym
    {-liftEval $-} --do
    push value
    incProgramCounter
    return False

            
exec (AssignStatic (Symbol sym)) = do
    val <- pop
    assign (LookupId $ unmkString sym) val
    incProgramCounter
    return False
exec (AssignRefStatic (Symbol sym) argCount) = do
    val <- pop
    args <- replicateM argCount pop
    assign (LookupAggregate (unmkString sym) args) val
    incProgramCounter
    return False

exec AssignDynamic = do
    item <- pop >>= \case
        Name lookup -> return lookup
        _ -> programError IllegalDataType
    sym <- toString item
    value <- pop >>= \case
        Name (KeywordName 
    assign (LookupId $ unmkString sym) value
    incProgramCounter
    return False

exec Define = do
    labelItem <- pop
    prototypeItem <- pop
    label <- toString labelItem
    prototype <- toString prototypeItem
    parseResult <- parseT $ unmkString prototype
    FunctionPrototype funcName argNames localNames <- case parseResult of
        Left _ -> programError ErroneousPrototype
        Right func -> return func
    labelResult <- labelLookup label
    Label addr <- case labelResult of
        Just l -> return l
        Nothing -> programError EntryPointOfFunctionNotLabel
    let func = Function funcName argNames localNames addr
    functionsNew func
    incProgramCounter
    return False
    
exec CallDynamic = programError ErrorInSnobol4System
exec (CallStatic (Symbol sym) argCount) = do
    lookupResult <- funcLookup sym
    let execFunc PrimitiveFunction{funcPrim=action} = do
            args <- replicateM argCount pop
            result <- action args
            case result of
                Just value -> do
                    push value
                    incProgramCounter
                    return False
                Nothing -> exec JumpToFailureLabel
        execFunc (UserFunction func) = do
            let firstArgIx = 0
                firstLocalIx = length $ formalArgs func
                returnIx = (length $ formalArgs func) + (length $ localNames func)
        
            -- Push nulls for each missing arguments, pop unused arguments
            let emptyArgCount = argCount - (length $ formalArgs func)
            if emptyArgCount > 0
                then replicateM_ emptyArgCount $ push $ StringData nullString
                else replicateM_ (-emptyArgCount) $ pop
            
            -- Push nulls for locals
            replicateM_ (length $ localNames func) $ push $ StringData nullString
            
            -- Push null for return value
            push $ StringData nullString
            
            -- Push return address
            getProgramCounter >>= push . IntegerData . getAddress
            
            -- Push references
            mapM pushReference $ formalArgs func
            mapM pushReference $ localNames func
            pushReference $ funcName func
            
            -- Set the new references
            forM (zip [firstArgIx..] $ formalArgs func) $ \(ix,name) -> 
                setReference name (Just $ LocalVar ix)
            forM (zip [firstLocalIx..] $ localNames func) $ \(ix,name) -> 
                setReference name (Just $ LocalVar ix)
            setReference (funcName func) $ Just $ LocalVar returnIx
            
            -- Push how many arguments+locals there are
            push $ IntegerData $ mkInteger returnIx
            
            -- Update the stack frame location
            let frameSize = 0
                    + (length $ formalArgs func) -- Arguments
                    + (length $ localNames func) -- Locals
                    + 1 -- Return value
                    + 1 -- Return address
                    + 3 * (length $ formalArgs func) -- References to Arguments
                    + 3 * (length $ localNames func) -- Refernces to Locals
                    + 3 -- Refernce to function name
                    + 1 -- Locals count
            pushCallStackFrame frameSize
            
            putProgramCounter $ entryPoint func
            return False
        execFunc (FunctionUnOperatorSynonym _ op) = do
            replicateM (argCount-1) pop
            exec (UnOp op)
        execFunc (FunctionBinOperatorSynonym _ op) = do
            replicateM (argCount-2) pop
            exec (BinOp op)
        execFunc (FunctionFunctionSynonym _ func) = execFunc func
    
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just func -> execFunc func
    return False
    
exec GetArgCount = programError ErrorInSnobol4System

exec Return = do
    -- Return to the last stack frame
    popCallStackFrame
    
    -- Find how many arguments+locals there are
    IntegerData localsCount <- pop

    -- Reset the references
    replicateM (1 + unmkInteger localsCount) popReference
    
    -- Jump to the return address
    pop >>= \(IntegerData addr) -> putProgramCounter $ Address $ unmkInteger addr
    
    -- Pop return value
    returnValue <- pop
    
    -- Pop off args and locals
    replicateM (unmkInteger localsCount) pop
        
    -- Push the return value back onto the stack
    push returnValue
    
    incProgramCounter
    return False

exec FReturn =  do
    -- Return to the last stack frame
    popCallStackFrame
    
    -- Find how many locals there are
    IntegerData localsCount <- pop

    -- Reset the references
    replicateM (unmkInteger localsCount) popReference

    -- Pop off return address, return value, args and locals
    popToCallStackFrame

    -- Jump to the fail label
    addr <- getFailLabel
    putProgramCounter addr
    popFailStack

    return False

exec (RefStatic (Symbol sym) argCount) = {-runEvaluation $-} do
    args <- {-liftEval $-} replicateM argCount pop
    result <- lookup (LookupAggregate (unmkString sym) args)
    {-liftEval $-} --do
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
exec JumpDynamic = do
    labelItem <- pop
    label <- toString labelItem
    result <- labelLookup label
    case result of
        Nothing -> programError UndefinedOrErroneousGoto
        Just (Label addr) -> putProgramCounter addr
        _ -> programError ErrorInSnobol4System
    return False
exec DirectJump = programError ErrorInSnobol4System
exec (BinOp op) = do
    lookupResult <- lookupBinOpSyn op
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just (PrimitiveOperator action) -> do
            x <- pop
            y <- pop
            result <- action [x,y]
            case result of
                Just x -> do
                    push x
                    incProgramCounter
                    return False
                Nothing -> exec JumpToFailureLabel
        Just (OperatorOperatorSynonym op') -> exec (BinOp op')
        Just (OperatorFunctionSynonym sym) -> exec (CallStatic sym 2)
exec (UnOp op) = do
    lookupResult <- lookupUnOpSyn op
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just (PrimitiveOperator action) -> do
            arg <- pop
            result <- action [arg]
            case result of
                Just x -> do
                    push x
                    incProgramCounter
                    return False
                Nothing -> exec JumpToFailureLabel
        Just (OperatorOperatorSynonym op') -> exec (UnOp op')
        Just (OperatorFunctionSynonym sym) -> exec (CallStatic sym 1)

exec InvokeScanner = do
    patternItem <- pop
    pat <- toPattern patternItem
    toMatch <- pop
    matchStr <- toString toMatch
    result <- scanPattern matchStr pat
    case result of
        NoScan -> do
            addr <- getFailLabel
            putProgramCounter addr
            popFailStack
        Scan match assigns start end -> do
            forM assigns $ uncurry assign
            push $ StringData matchStr
            push match
            push $ IntegerData start
            push $ IntegerData end
            incProgramCounter
    return False
exec InvokeReplacer = do
    IntegerData end <- pop
    IntegerData start <- pop
    replacement <- pop
    StringData originalString <- pop
    replacementStr <- toString replacement
    let preReplacement = snobol4Take start originalString
        postReplacement = snobol4Drop end originalString
        replacedString = preReplacement <> replacementStr <> postReplacement
    push $ StringData replacedString
    incProgramCounter
    return False

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
exec Output = do
    value <- pop
    str <- toString value
    lift $ output $ unmkString str
    incProgramCounter
    return False
exec Punch = do
    value <- pop
    str <- toString value
    lift $ punch $ unmkString str
    incProgramCounter
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


primOp_plus = PrimitiveOperator $ const $ do
    y <- pop
    x <- pop
    liftM Just $ Eval.arithmetic (+) (+) x y

primOpsBin =
    [ (Plus, primOp_plus)
    ]

run :: InterpreterShell m => CompiledProgram -> SymbolTable -> m (Maybe ProgramError)
run prog tbl = do
    result <- runStackMachine $ interpret emptyState $ runProg
    case result of
        Left err -> return $ Just err
        Right _ -> return Nothing
  where
    runProg :: InterpreterShell m => Interpreter m (Maybe ProgramError)
    runProg = do
        initVM prog tbl
        loop
    loop :: InterpreterShell m => Interpreter m (Maybe ProgramError)
    loop = do
        result <- step
        if result
            then return Nothing
            else loop
    

initVM :: InterpreterShell m => CompiledProgram -> SymbolTable -> Interpreter m ()
initVM prog tbl = do
    putProgram prog
    let statics = V.replicate (M.size $ varSymbols tbl) $ StringData nullString
        dynamics = M.fromList $ zip (M.keys $ varSymbols tbl) $ map GlobalVar [0..]
    putVariables $ Variables statics dynamics
    putLabels $ M.map Label $ userLabels tbl
    putBinOpSyns $ M.fromList primOpsBin
    addPrimitives
    lbls <- prepareSystemLabels
    putSystemLabels lbls
    putProgramCounter $ programEntryPoint tbl
  where
    prepareSystemLabels :: InterpreterShell m => Interpreter m (Vector Address)
    prepareSystemLabels = do
        case sequence $ systemLabels tbl of
            Just m' -> V.generateM (M.size m') $ \ix -> case M.lookup (SystemLabel $ mkInteger ix) m' of
                Nothing -> programError ErrorInSnobol4System
                Just addr -> return addr
            Nothing -> programError ErrorInSnobol4System


runVM :: InterpreterShell m => Interpreter m a -> m (Either ProgramError a)
runVM = runStackMachine . interpret emptyState

getStack :: InterpreterShell m => Interpreter m ([Data ExprKey])
getStack = lift S.getStackList
