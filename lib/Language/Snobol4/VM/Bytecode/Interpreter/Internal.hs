{-|
Module          : Language.Snobol4.VM.Bytecode.Interpreter.Internal
Description     : The SNOBOL4 Virtual Machine Interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

Bytecode virtual machine. See Language.Snobol4.VM.Bytecode.Interpreter for
    public interface.

Virtual machine runs a stack machine underneath the state machine. The virtual
    machine runs in either static or dynamic mode. Static mode processes
    instructions as usual. If the scanner encounters an unevaluated expression,
    it will switch the virtual machine in dynamic mode after jumping to the
    entry point for that expression. The EVAL primitive works in the same way.
    A direct jump to a code object will also run in dynamic mode. In this mode,
    the only difference is the treatment of the ExprReturn instruction. In
    static mode, executing this instruction is an error, while in dynamic mode,
    it signals to return to whatever caused the machine to switch modes, and
    return to the mode it was previously in.
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
module Language.Snobol4.VM.Bytecode.Interpreter.Internal where

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
import Language.Snobol4.Interpreter.Internal.StateMachine hiding
    ( Return
    , FReturn
    , programError
    , lookup
    , assign
    , getProgramCounter
    , putProgramCounter
    , getProgram
    , execLookup
    , toString
    )
import qualified Language.Snobol4.Interpreter.Internal.StateMachine as StMch
import Language.Snobol4.Interpreter.Internal.StateMachine.Types hiding (Return, FReturn)


import Language.Snobol4.VM.Bytecode.Interpreter.Types
import Language.Snobol4.VM.Bytecode.Interpreter.Wrappers

-- | Evaluate expressions by jumping to their entry points and running in
-- dynamic mode
instance ( InterpreterShell m
         ) => NewSnobol4Machine (StackMachine m) where
    eval lbl = runVMInternal $ do
        addr <- lookupSystemLabel lbl
        returnAddr <- getProgramCounter
        putProgramCounter addr
        result <- runExpr
        putProgramCounter returnAddr
        if result
            then liftM Just pop
            else return Nothing
        
-- | Push a reference to a variable onto the stack
pushReference :: ( InterpreterShell m ) => Snobol4String -> VM m ()
pushReference name = do
    ref <- VM $ getReference name
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
    VM $ setReference name ref

-- | Execute an instruction in static mode
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
exec LookupDynamic = do
    item <- pop
    sym <- toString item
    value <- lookup $ LookupId $ unmkString sym
    
    push value
    incProgramCounter
    return False
    
exec (LookupStatic (Symbol sym)) = do
    value <- lookup $ LookupId $ unmkString sym
    
    push value
    incProgramCounter
    return False
exec (LookupStaticKeyword (Symbol sym)) = do
    value <- lookup $ LookupKeyword $ unmkString sym
    
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
exec (CallStatic (Symbol sym) argCount isLValue) = do
    lookupResult <- VM $ funcLookup sym
    let execFunc PrimitiveFunction{funcPrim=action} = do
            args <- replicateM argCount pop
            result <- VM $ action args
            case result of
                Just value -> do
                    push value
                    VM incFunctionLevel
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
            VM $ forM_ (zip [firstArgIx..] $ formalArgs func) $ \(ix,name) -> 
                setReference name (Just $ LocalVar ix)
            VM $ forM_ (zip [firstLocalIx..] $ localNames func) $ \(ix,name) -> 
                setReference name (Just $ LocalVar ix)
            VM $ setReference (funcName func) $ Just $ LocalVar returnIx
            
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
                    result <- VM $ userDataLookup key
                    case result of
                        Nothing -> programError ErrorInSnobol4System
                        Just data_ -> do
                            item <- lookup $ LookupUserData key dataName $ mkInteger ix
                            push item
            incProgramCounter
            return False
        execFunc (DataConstructorFunction name ix) = do
            args <- replicateM ix pop
            result <- VM $ userDataConstruct name (mkInteger ix) args
            push $ UserData result
            incProgramCounter
            return False
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just func -> execFunc func
    return False

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
    
    VM decFunctionLevel
    VM $ setReturnType "RETURN"
    
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
    
    VM decFunctionLevel
    VM $ setReturnType "NRETURN"
    
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

    VM decFunctionLevel
    VM $ setReturnType "FRETURN"

    return False

exec ExprReturn = do
    programError ErrorInSnobol4System

exec (LookupStaticRef (Symbol sym) argCount) = do
    args <- replicateM argCount pop
    result <- lookup (LookupAggregate (unmkString sym) args)
    
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
    VM incFailCount
    return False

exec (JumpStatic lbl) = do
    addr <- lookupSystemLabel lbl
    putProgramCounter addr
    return False

exec JumpDynamic = do
    labelItem <- pop
    label <- case labelItem of
        ReferenceId sym -> return sym
        _ -> toString labelItem
    result <- VM $ labelLookup label
    case result of
        Nothing -> programError UndefinedOrErroneousGoto
        Just (Label addr) -> putProgramCounter addr
        _ -> programError ErrorInSnobol4System
    return False
exec DirectJump = programError ErrorInSnobol4System
exec (BinOp op) = do
    lookupResult <- VM $ lookupBinOpSyn op
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just NoOperator -> programError UndefinedFunctionOrOperation
        Just (PrimitiveOperator action) -> do
            x <- pop
            y <- pop
            result <- VM $ action [x,y]
            case result of
                Just x -> do
                    push x
                    incProgramCounter
                    return False
                Nothing -> exec JumpToFailureLabel
        Just (OperatorOperatorSynonym op') -> exec (BinOp op')
        Just (OperatorFunctionSynonym sym) -> exec (CallStatic sym 2 False)
exec (UnOp op) = do
    lookupResult <- VM $ lookupUnOpSyn op
    case lookupResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just NoOperator -> programError UndefinedFunctionOrOperation
        Just (PrimitiveOperator action) -> do
            arg <- pop
            result <- VM $ action [arg]
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
    pat <- VM $ toPattern patternItem
    toMatch <- pop
    matchStr <- toString toMatch
    anchorMode <- VM $ getAnchorMode
    result <- VM $ scanPattern matchStr pat anchorMode
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

exec (Panic err) = programError err
exec Finish = return True

exec Input = do
    str <- VM $ lift $ lift input 
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
    VM $ lift $ punch $ unmkString str
    incProgramCounter
    return False
exec LastOutput = do
    str <- VM $ lift $ lastOutput
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

-- | Execute the next instruction in static mode
step :: ( InterpreterShell m 
        )
     => VM m Bool
step = VM fetch >>= exec


-- | Execute an instruction in dynamic mode
execExpr :: ( InterpreterShell m 
           ) 
        => Instruction -> VM m ExprResult
execExpr ExprReturn = return ExprFinished
execExpr x = liftM ExprUnfinished $ exec x

-- | Run the virtual machine in dynamic mode
runExpr :: ( InterpreterShell m
          )
       => VM m Bool
runExpr = loop
  where
    loop = do
        result <- stepExpr
        case result of
            ExprFinished -> return True
            ExprUnfinished False -> loop
            ExprUnfinished True -> return False

-- | Execute the next instruction in dynamic mode
stepExpr :: ( InterpreterShell m
           )
        => VM m ExprResult
stepExpr = VM fetch >>= execExpr

-- | Run a compiled program to completion
run :: InterpreterShell m => CompiledProgram -> SymbolTable -> m (Maybe ProgramError)
run prog tbl = do
    result <- runStackMachine $ interpret emptyState $ runVMInternal $ runProg
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
    
-- | Initialize a virtual machine from a program and symbol table
initVM :: InterpreterShell m => CompiledProgram -> SymbolTable -> VM m ()
initVM prog tbl = do
    VM $ putProgram prog
    let statics = V.replicate (M.size $ varSymbols tbl) $ StringData nullString
        dynamics = M.fromList $ zip (M.keys $ varSymbols tbl) $ map GlobalVar [0..]
    VM $ putVariables $ Variables statics dynamics
    VM $ putLabels $ M.map Label $ userLabels tbl
    --putBinOpSyns $ M.fromList primOpsBin
    VM addPrimitives
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

-- | Initialize a pause a virtual machine from a program and symbol table
initAndPauseVM :: ( InterpreterShell m 
                  )
               => CompiledProgram 
               -> SymbolTable 
               -> m (PausedVM m)
initAndPauseVM prog tbl = do
    stk <- S.initStackMachine
    let initialState = PausedVM (Paused emptyState) stk
        initialAction = do
            initVM prog tbl
            return $ Just ()
    resumeVM' initialAction initialState

-- | Step a paused virtual machine one instruction
stepAndPauseVM :: InterpreterShell m => PausedVM m -> m (PausedVM m)
stepAndPauseVM = resumeVM' $ do
    done <- step
    if done
        then return Nothing
        else return $ Just ()

-- | Run an action in a paused virtual machine, then pause it when complete
runAndPauseVM :: InterpreterShell m => VM m a -> PausedVM m -> m (Maybe a, PausedVM m)
runAndPauseVM = resumeVM . liftM Just

-- | Execute the next instruction in the virtual machine
stepVM :: InterpreterShell m => VM m Bool
stepVM = step

-- | Run an action in a paused virtual machine
resumeVM :: InterpreterShell m => VM m (Maybe a) -> PausedVM m -> m (Maybe a, PausedVM m)
resumeVM _ (PausedVM (Terminated err) stk) = return $ (Nothing, PausedVM (Terminated err) stk)
resumeVM m (PausedVM (Paused st) stk) = do
    (result, stk') <- flip S.resumeStackMachine stk $ interpret st $ runVMInternal $ do
        done <- m
        st' <- VM getProgramState
        return (done,st')
    case result of
        Right (Nothing, st') -> return $ (Nothing, PausedVM (Paused st') stk')
        Right (Just x, _) -> return $ (Just x, PausedVM (Terminated NormalTermination) stk')
        Left err -> do
            (Right addr,stk'') <- flip S.resumeStackMachine stk 
                                $ interpret st 
                                $ liftM (unmkInteger . getAddress) 
                                $ runVMInternal getProgramCounter
            return $ (Nothing, PausedVM (Terminated $ ErrorTermination err addr) stk'')

-- | Run an action in a paused virtual machine and discard the result
resumeVM' :: InterpreterShell m => VM m (Maybe a) -> PausedVM m -> m (PausedVM m)
resumeVM' f = liftM snd . resumeVM f

-- | Stateful computations that may fail
type StateExcept e s m b = s -> m (Either e b, s)

-- | Functions which decompose a transformer value into a stateful computation
-- which may fail
type RunStateExcept t e s m = forall b . t m b -> StateExcept e s m b

-- | Functions which build a tranformer value from a stateful computation which
-- may fail
type MkStateExcept t e s m = forall b . StateExcept e s m b -> t m b

-- | Functions which build a stateful computation which may fail, provided
-- how to build and decompose said computations from a transformed monadic value
type StateExceptBuilder t m a
    = forall s e
    .  ( RunStateExcept t e s m ) -- ^ Computation deconstructor
    -> ( MkStateExcept t e s m ) -- ^ Computation constructor
    -> StateExcept e s m a -- ^ Computation
        
        

-- | Don't even bother trying to understand this, If you need to understand this,
-- I pity you.
-- 
-- This function exposes a StateT/runStateT-like interface without exposing
-- constructors/deconstructors
-- The function passed to this function, itself accepts two additional functions
--
--      * the first is the analog of runStateT
--      * the second is the analog of StateT
-- 
-- Using this method, class instances can be created for the VM type if they can
--  be created for StateT, without exposing the implementation details of the VM type
mkVM :: forall m a
      . (InterpreterShell m) 
     => StateExceptBuilder VM m a
     -> VM m a
mkVM f = VM 
       $ mkInterpreterGeneric 
       $ \runFunc1 stateFunc1 st1 
            -> S.mkStackMachine 
             $ \runFunc2 stateFunc2 st2 -> do
    let runFunc :: forall s s1 e b 
                 . ( forall c . VM m c -> s -> StackMachine m (Either e c, s)) 
                -> ( forall c . StackMachine m c -> s1 -> m (c, s1) )
                -> VM m b 
                -> (s,s1) 
                -> m (Either e b, (s,s1))
        runFunc runFunc1 runFunc2 g (st1x, st2x) = do
            ((result, st1x'), st2x') <- runFunc2 ((runFunc1 g) st1x) st2x
            return (result, (st1x', st2x'))
        stateFunc :: ( forall c . (s -> StackMachine m (Either e c, s)) -> VM m c )
                  -> ( forall c . (s1 -> m (c, s1)) -> StackMachine m c )
                  -> ((s,s1) -> m (Either e c, (s,s1)))
                  -> VM m c
        stateFunc stateFunc1 stateFunc2 g = stateFunc1 $ \st1x -> stateFunc2 $ \st2x -> do
            (result, (st1x',st2x')) <- g (st1x,st2x)
            return ((result, st1x'), st2x')
    (result, (st1',st2')) <- f (runFunc (runFunc1 . runVMInternal) runFunc2) (stateFunc (VM . stateFunc1) stateFunc2) (st1, st2)
    return ((result, st1'), st2')
        
    

-- | Run the virtual machine
runVM :: InterpreterShell m => VM m a -> m (Either ProgramError a)
runVM = runStackMachine . interpret emptyState . runVMInternal

-- | Get the contents of the stack
getStack :: InterpreterShell m => VM m ([Data ExprKey])
getStack = VM $ lift S.getStackList


