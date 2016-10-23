{-|
Module          : Language.Snobol4.VM.Bytecode.Interpreter
Description     : The SNOBOL4 Virtual Machine Interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown


Public interface to the SNOBOL4 virtual machine

The virtual machine can be run in one of three ways, continuous, interupted, and
    monad.
    
To run the virtual machine in continuous mode, provide a program and symbol
    table to the run function. The program will run until it terminates.

@
main = do
    program <- loadUserProgram
    table <- loadUserSymbolTable
    shell $ do
        start
        run program table
@


To run the virtual machine in interupted mode, provide a program and symbol
    table to the initAndPauseVM function. This will create a new VM and
    provide a token to access it. The next instruction can be executed
    by passing this token to the stepAndPauseVM function.

@
main = do
    program <- loadUserProgram
    table <- loadUserSymbolTable
    vm <- shell $ do
        start
        initAndPauseVM program table
    vm2 <- shell $ stepAndPauseVM vm
    -- etc
@

To run the virtual machine in monad mode, provide a program and symbol table to
    the initVM method. This will produce a monadic value that, when executed,
    will start the virtual machine. This can then be bound with other actions,
    such as stepVM or execLookup. To run this computation, pass the complete
    monadic value to runVM.

@
main = do
    program <- loadUserProgram
    table <- loadUserSymbolTable
    shell $ do
        start
        runVM $ runUserTransformerT $ do
            lift $ initVM program table
            userOutputFunction
            stepVM
            -- ...
@

-}


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Snobol4.VM.Bytecode.Interpreter 
    ( VM
    , PausedVM
    , mkVM
    , run
    , initAndPauseVM
    , stepAndPauseVM
    , runVM
    , initVM
    , stepVM
    
    , getProgram
    , getProgramCounter
    , getCallStackFrameStart
    , execLookup
    , toString
    , getStack
    ) where

import Prelude hiding (lookup, toInteger)

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
import Language.Snobol4.VM.Bytecode.Interpreter.StackMachine (StackMachine, runStackMachine)
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
import Language.Snobol4.Interpreter.Internal.StateMachine.UserData
import Language.Snobol4.Interpreter.Internal.StateMachine.Keywords

import Language.Snobol4.VM.Bytecode.Interpreter.Types

{-
type EvalError = ()
-}

-- | A paused virtual machine
data PausedVM m = PausedVM (PausedInterpreterGeneric CompiledProgram (StackMachine ExprKey m))
                           (S.StackMachineState ExprKey)

-- | State of the virtual machine
type VMState = ProgramStateGeneric CompiledProgram

-- | Monad transformer for the interpreter
type VM m = InterpreterGeneric CompiledProgram (StackMachine ExprKey m)

--type Evaluator m = EvaluatorGeneric CompiledProgram EvalError (StackMachine m)

instance EmptyProgramClass CompiledProgram where
    emptyProgram = CompiledProgram V.empty

instance ProgramClass CompiledProgram where
    type InstructionType CompiledProgram = Instruction
    getInstruction (Address ix) (CompiledProgram is) = is V.! unmkInteger ix

instance ( InterpreterShell m
         ) => NewSnobol4Machine (StackMachine ExprKey m) where
    type ProgramType (StackMachine ExprKey m) = CompiledProgram
    type ExprType (StackMachine ExprKey m) = ExprKey
    type FuncType (StackMachine ExprKey m) = Symbol
    type ArgType (StackMachine ExprKey m) = ()
    eval lbl = do
        addr <- lookupSystemLabel lbl
        putProgramCounter addr
        result <- runExpr
        if result
            then liftM Just pop
            else return Nothing
        

-- | Retreive the label to jump to on failure
getFailLabel :: (Monad m) => VM m Address
getFailLabel = lift S.getFailLabel

-- | Overwrite the label to jump to on failure
putFailLabel :: ( Monad m) => Address -> VM m ()
putFailLabel = lift . S.putFailLabel

-- | Set the compiler generated labels
putSystemLabels :: (Monad m) => Vector Address -> VM m ()
putSystemLabels = lift . S.putSystemLabels

-- | Reterieve a compiler generated label
lookupSystemLabel :: (Monad m) => SystemLabel -> VM m Address
lookupSystemLabel = lift . S.lookupSystemLabel

-- | Push onto the stack
push :: Monad m => Data ExprKey -> VM m ()
push = lift . S.push

-- | Pop from the stack
pop :: Monad m => VM m (Data ExprKey)
pop = lift S.pop >>= \case
    Nothing -> programError ErrorInSnobol4System
    Just x -> return x

-- | Pop all items since the last time the failure label was set
popFailStack :: Monad m => VM m ()
popFailStack = lift S.popFailStack

-- | Pop all items from the current stack frame
popToCallStackFrame :: Monad m => VM m ()
popToCallStackFrame = lift S.popToCallStackFrame

-- | Pop the current stack frame
popCallStackFrame :: Monad m => VM m ()
popCallStackFrame = do
    x <- lift S.popCallStackFrame
    case x of
        Just _ -> return ()
        Nothing -> programError ErrorInSnobol4System

-- | Push a new stack frame
pushCallStackFrame :: Monad m => Int -> VM m ()
pushCallStackFrame = lift . S.pushCallStackFrame

-- | Get the number of items on the current stack frame
getCallStackFrameStart :: Monad m => VM m Int
getCallStackFrameStart = lift S.getCallStackFrameStart

-- | Push the current fail label onto the stack
pushFailLabel :: Monad m => Address -> VM m ()
pushFailLabel = lift . S.pushFailLabel

-- | Pop the the fail label from the stack
popFailLabel :: Monad m => VM m ()
popFailLabel = do
    x <- lift S.popFailLabel
    case x of
        Just _ -> return ()
        Nothing -> programError ErrorInSnobol4System

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

{-
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
-}

{-
runEvaluation :: InterpreterShell m => Evaluator m Bool -> Interpreter m Bool
runEvaluation f = do
    catchEval f $ \_ -> do
        addr <- getFailLabel
        putProgramCounter addr
        popFailStack
        return False
-}

-- | Push a reference to a variable onto the stack
pushReference :: ( InterpreterShell m ) => Snobol4String -> VM m ()
pushReference name = do
    ref <- getReference name
    case ref of
        Nothing -> push (IntegerData 0) >> push (IntegerData 0)
        Just (LocalVar ix)-> push (IntegerData $ mkInteger ix) >> push (IntegerData 1)
        Just (GlobalVar ix) -> push (IntegerData $ mkInteger ix) >> push (IntegerData 2)
    push $ StringData name

-- | Pop a variable reference off the stack
popReference :: ( InterpreterShell m ) => VM m ()
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
        _ -> programError ErrorInSnobol4System
    setReference name ref

-- | Execute an instruction
exec :: InterpreterShell m => Instruction -> VM m Bool
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
exec (PushReference sym) = do
    push $ ReferenceId sym
    incProgramCounter
    return False
exec (PushReferenceKeyword sym) = do
    push $ ReferenceKeyword sym
    incProgramCounter
    return False
exec (PushReferenceAggregate sym count) = do
    args <- replicateM (unmkInteger count) pop
    push $ ReferenceAggregate sym args
    incProgramCounter
    return False
exec PushReferenceInput = do
    push ReferenceInput
    incProgramCounter
    return False
exec PushReferenceOutput = do
    push ReferenceOutput
    incProgramCounter
    return False
exec PushReferencePunch = do
    push ReferencePunch
    incProgramCounter
    return False
exec (PushExpression lbl) = do
    push $ TempPatternData $ UnevaluatedExprPattern lbl
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
exec Rotate = do
    x <- pop
    y <- pop
    push x
    push y
    incProgramCounter
    return False
{-
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

exec ConcatPattern = pattern $ \p1 p2 -> case (p1,p2) of
    (LiteralPattern l1, LiteralPattern l2) -> LiteralPattern $ l1 <> l2
    _ -> Data.ConcatPattern p1 p2
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
-}
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
exec (LookupStaticKeyword (Symbol sym)) = {-runEvaluation $-} do
    value <- lookup $ LookupKeyword $ unmkString sym
    {-liftEval $-} --do
    push value
    incProgramCounter
    return False

            
exec (AssignStatic (Symbol sym)) = do
    val <- pop
    assign (LookupId $ unmkString sym) val
    incProgramCounter
    return False
exec (AssignStaticKeyword (Symbol sym)) = do
    val <- pop
    assign (LookupKeyword $ unmkString sym) val
    incProgramCounter
    return False
exec (AssignRefStatic (Symbol sym) argCount) = do
    val <- pop
    args <- replicateM argCount pop
    assign (LookupAggregate (unmkString sym) args) val
    incProgramCounter
    return False

exec AssignDynamic = do
    value <- pop
    item <- pop >>= \case
        ReferenceId sym -> return $ LookupId sym
        ReferenceAggregate sym args -> return $ LookupAggregate sym args
        ReferenceKeyword sym -> return $ LookupKeyword sym
        ReferenceUserData key dataName ix -> return $ LookupUserData key dataName ix
        _ -> programError VariableNotPresentWhereRequired
    assign item value
    incProgramCounter
    return False
{-
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
-}
exec (CallStatic (Symbol sym) argCount isLValue) = do
    lookupResult <- funcLookup sym
    let execFunc PrimitiveFunction{funcPrim=action} = do
            args <- replicateM argCount pop
            result <- action args
            case result of
                Just value -> do
                    push value
                    incFunctionLevel
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
        execFunc (DataSelectorFunction name dataName ix) = do
            when (argCount == 0) $ programError IllegalDataType
            replicateM (argCount-1) pop
            UserData key <- pop
            if isLValue
                then push $ ReferenceUserData key dataName $ mkInteger ix
                else do
                    result <- userDataLookup key
                    case result of
                        Nothing -> programError ErrorInSnobol4System
                        Just data_ -> do
                            item <- lookup $ LookupUserData key dataName $ mkInteger ix
                            push item
            incProgramCounter
            return False
        execFunc (DataConstructorFunction name ix) = do
            args <- replicateM ix pop
            result <- userDataConstruct name (mkInteger ix) args
            push $ UserData result
            incProgramCounter
            return False
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just func -> execFunc func
    return False
{-
exec GetArgCount = programError ErrorInSnobol4System
-}
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
    
    decFunctionLevel
    setReturnType "RETURN"
    
    incProgramCounter
    return False

exec NReturn = do
    -- Return to the last stack frame
    popCallStackFrame
    
    -- Find how many arguments+locals there are
    IntegerData localsCount <- pop

    -- Reset the references
    replicateM (1 + unmkInteger localsCount) popReference
    
    -- Jump to the return address
    pop >>= \(IntegerData addr) -> putProgramCounter $ Address $ unmkInteger addr
    
    -- Pop return value
    returnValue <- pop >>= \case
        Name (LookupId sym) -> return $ ReferenceId sym
        Name (LookupAggregate sym args) -> return $ ReferenceAggregate sym args
        _ -> programError IllegalDataType
    
    -- Pop off args and locals
    replicateM (unmkInteger localsCount) pop
        
    -- Push the return value back onto the stack
    push returnValue
    
    decFunctionLevel
    setReturnType "NRETURN"
    
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

    decFunctionLevel
    setReturnType "FRETURN"

    return False

exec ExprReturn = do
    programError ErrorInSnobol4System

exec (LookupStaticRef (Symbol sym) argCount) = {-runEvaluation $-} do
    args <- {-liftEval $-} replicateM argCount pop
    result <- lookup (LookupAggregate (unmkString sym) args)
    {-liftEval $-} --do
    push result
    incProgramCounter
    return False

exec (SetFailLabel lbl) = do
    addr <- lookupSystemLabel lbl
    putFailLabel addr
    incProgramCounter
    return False
exec (PushFailLabel lbl) = do
    addr <- lookupSystemLabel lbl
    pushFailLabel addr
    incProgramCounter
    return False
exec PopFailLabel = do
    popFailLabel
    incProgramCounter
    return False
exec JumpToFailureLabel = do
    addr <- getFailLabel
    putProgramCounter addr
    popFailStack
    incFailCount
    return False
{-
exec JumpToFailureLabelIf = programError ErrorInSnobol4System
exec JumpToFailureLabelElse = programError ErrorInSnobol4System
-}
exec (JumpStatic lbl) = do
    addr <- lookupSystemLabel lbl
    putProgramCounter addr
    return False
{-
exec (JumpStaticIf _) = programError ErrorInSnobol4System
exec (JumpStaticElse _) = programError ErrorInSnobol4System
-}
exec JumpDynamic = do
    labelItem <- pop
    label <- case labelItem of
        ReferenceId sym -> return sym
        _ -> toString labelItem
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
        Just NoOperator -> programError UndefinedFunctionOrOperation
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
        Just (OperatorFunctionSynonym sym) -> exec (CallStatic sym 2 False)
exec (UnOp op) = do
    lookupResult <- lookupUnOpSyn op
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just NoOperator -> programError UndefinedFunctionOrOperation
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
        Just (OperatorFunctionSynonym sym) -> exec (CallStatic sym 1 False)

exec InvokeScanner = do
    patternItem <- pop
    pat <- toPattern patternItem
    toMatch <- pop
    matchStr <- toString toMatch
    anchorMode <- getAnchorMode
    result <- scanPattern matchStr pat anchorMode
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
    replacement <- pop
    IntegerData end <- pop
    IntegerData start <- pop
    match <- pop
    StringData originalString <- pop
    replacementStr <- toString replacement
    let preReplacement = snobol4Take start originalString
        postReplacement = snobol4Drop end originalString
        replacedString = preReplacement <> replacementStr <> postReplacement
    push $ StringData replacedString
    incProgramCounter
    return False
{-
exec ConvertToString = programError ErrorInSnobol4System
exec ConvertToInteger = programError ErrorInSnobol4System
exec ConvertToReal = programError ErrorInSnobol4System
exec ConvertToPattern = programError ErrorInSnobol4System
-}

{-
exec AllocArray = programError ErrorInSnobol4System
exec AllocTable = programError ErrorInSnobol4System
-}

exec (Panic err) = programError err
exec Finish = return True

exec Input = do
    str <- lift $ lift input 
    case str of
        Just str -> do
            push $ StringData $ mkString str
            incProgramCounter
            return False
        Nothing -> exec JumpToFailureLabel
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

-- | result of executing an instruction in unevaluated expression mode
data ExprResult
    = 
    -- | Expression has been evaluated
      ExprFinished
    -- | Expression has not been evaluated
    | ExprUnfinished Bool

-- | Execute an instruction in unevaluated expression mode
execExpr :: ( InterpreterShell m 
           ) 
        => Instruction -> VM m ExprResult
execExpr ExprReturn = return ExprFinished
execExpr x = liftM ExprUnfinished $ exec x

-- | Run the virtual machine in unevaluated expression mode
runExpr :: ( InterpreterShell m
          )
       => VM m Bool
runExpr = loop
  where
    loop = do
        result <- stepExpr
        case result of
            ExprFinished -> return True
            ExprUnfinished True -> loop
            ExprUnfinished False -> return False

-- | Execute the next instruction in unevaluated expression mode
stepExpr :: ( InterpreterShell m
           )
        => VM m ExprResult
stepExpr = fetch >>= execExpr

-- | Execute the next instruction
step :: ( InterpreterShell m 
        )
     => VM m Bool
step = fetch >>= exec

{-
primOp_plus = PrimitiveOperator $ const $ do
    y <- pop
    x <- pop
    liftM Just $ Eval.arithmetic (+) (+) x y

primOpsBin =
    [ (Plus, primOp_plus)
    ]
-}

-- | Run a compiled program to completion
run :: InterpreterShell m => CompiledProgram -> SymbolTable -> m (Maybe ProgramError)
run prog tbl = do
    result <- runStackMachine $ interpret emptyState $ runProg
    case result of
        Left err -> return $ Just err
        Right _ -> return Nothing
  where
    runProg :: InterpreterShell m => VM m (Maybe ProgramError)
    runProg = do
        initVM prog tbl
        loop
    loop :: InterpreterShell m => VM m (Maybe ProgramError)
    loop = do
        result <- step
        if result
            then return Nothing
            else loop
    
-- | Iniialize a virtual machine from a program and symbol table
initVM :: InterpreterShell m => CompiledProgram -> SymbolTable -> VM m ()
initVM prog tbl = do
    putProgram prog
    let statics = V.replicate (M.size $ varSymbols tbl) $ StringData nullString
        dynamics = M.fromList $ zip (M.keys $ varSymbols tbl) $ map GlobalVar [0..]
    putVariables $ Variables statics dynamics
    putLabels $ M.map Label $ userLabels tbl
    --putBinOpSyns $ M.fromList primOpsBin
    addPrimitives
    lbls <- prepareSystemLabels
    putSystemLabels lbls
    putProgramCounter $ programEntryPoint tbl
  where
    prepareSystemLabels :: InterpreterShell m => VM m (Vector Address)
    prepareSystemLabels = do
        case sequence $ systemLabels tbl of
            Just m' -> V.generateM (M.size m') $ \ix -> case M.lookup (SystemLabel $ mkInteger ix) m' of
                Nothing -> programError ErrorInSnobol4System
                Just addr -> return addr
            Nothing -> programError ErrorInSnobol4System

-- | Initialize a pause a virtual machine from a progran and symbol table
initAndPauseVM :: ( InterpreterShell m 
                  , NewSnobol4Machine m
                  )
               => CompiledProgram 
               -> SymbolTable 
               -> m (PausedVM m)
initAndPauseVM prog tbl = do
    stk <- S.initStackMachine
    resumeVM (initVM prog tbl >> return False) $ PausedVM (Paused emptyState) stk

-- | Step a paused virtual machine one instruction
stepAndPauseVM :: InterpreterShell m => PausedVM m -> m (PausedVM m)
stepAndPauseVM = resumeVM step

stepVM :: InterpreterShell m => VM m Bool
stepVM = step

-- | Run an action in a paused virtual machine
resumeVM :: InterpreterShell m => VM m Bool -> PausedVM m -> m (PausedVM m)
resumeVM _ (PausedVM (Terminated err) stk) = return $ PausedVM (Terminated err) stk
resumeVM m (PausedVM (Paused st) stk) = do
    (result, stk') <- flip S.resumeStackMachine stk $ interpret st $ do
        done <- m
        st' <- getProgramState
        return (done,st')
    case result of
        Right (False, st') -> return $ PausedVM (Paused st') stk'
        Right (True, _) -> return $ PausedVM (Terminated NormalTermination) stk'
        Left err -> do
            (Right addr,stk'') <- flip S.resumeStackMachine stk $ interpret st $ liftM (unmkInteger . getAddress) getProgramCounter
            return $ PausedVM (Terminated $ ErrorTermination err addr) stk''



-- | Don't even bother trying to understand this
-- If you need to understand this, I pity you
-- This function exposes a StateT/runStateT interface without exposing
-- constructors/deconstructors
-- The function passed to this function, itself accepts two additional functions,
--      the first is the analog of runStateT
--      the second is the analog of StateT
-- Using this method, class instances can be created for the VM type if they can
--  be created for StateT, without exposing the implementation details of the VM type
mkVM :: forall m a
      . (InterpreterShell m) 
     => ( forall s e
        .  ( forall b . VM m b -> s -> m (Either e b, s) )
        -> ( forall b . (s -> m (Either e b, s)) -> VM m b)
        -> s 
        -> m (Either e a, s)
        )
     -> VM m a
mkVM f = mkInterpreterGeneric 
       $ \runFunc1 stateFunc1 st1 
            -> S.mkStackMachine 
             $ \runFunc2 stateFunc2 st2 -> do
    let runFunc :: forall s s1 e b 
                 . ( forall c . VM m c -> s -> StackMachine ExprKey m (Either e c, s)) 
                -> ( forall c . StackMachine ExprKey m c -> s1 -> m (c, s1) )
                -> VM m b 
                -> (s,s1) 
                -> m (Either e b, (s,s1))
        runFunc runFunc1 runFunc2 g (st1x, st2x) = do
            ((result, st1x'), st2x') <- runFunc2 ((runFunc1 g) st1x) st2x
            return (result, (st1x', st2x'))
        stateFunc :: ( forall c . (s -> StackMachine ExprKey m (Either e c, s)) -> VM m c )
                  -> ( forall c . (s1 -> m (c, s1)) -> StackMachine ExprKey m c )
                  -> ((s,s1) -> m (Either e c, (s,s1)))
                  -> VM m c
        stateFunc stateFunc1 stateFunc2 g = stateFunc1 $ \st1x -> stateFunc2 $ \st2x -> do
            (result, (st1x',st2x')) <- g (st1x,st2x)
            return ((result, st1x'), st2x')
    (result, (st1',st2')) <- f (runFunc runFunc1 runFunc2) (stateFunc stateFunc1 stateFunc2) (st1, st2)
    {-
    (result, (st1',st2')) <- flip f (st1,st2) $ \g (st1x, st2x) -> do
            ((result, st1x'), st2x') <- runFunc2 ((runFunc1 g) st1x) st2x
            return (result, (st1x', st2x')) :: _
    -}
    return ((result, st1'), st2')
        
    

-- | Run the virtual machine
runVM :: InterpreterShell m => VM m a -> m (Either ProgramError a)
runVM = runStackMachine . interpret emptyState

-- | Get the contents of the stack
getStack :: InterpreterShell m => VM m ([Data ExprKey])
getStack = lift S.getStackList

