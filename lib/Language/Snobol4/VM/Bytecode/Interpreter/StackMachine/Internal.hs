{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Snobol4.VM.Bytecode.Interpreter.StackMachine.Internal where

import qualified Data.Stack as S
import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Language.Snobol4.Interpreter.Data


import Language.Snobol4.VM.Bytecode (SystemLabel(..), CompiledProgram, Symbol, ExprKey)

import Language.Snobol4.Interpreter.Internal.StateMachine.Types hiding (Return, FReturn)


import Language.Snobol4.Interpreter.Shell


type Stack expr = S.Stack (Data expr)

-- | State of the stack machine
data StackMachineState expr
    = StackMachineState
    { 
    -- | The stack
      stack :: Stack expr
    -- | The label to jump to in case of evaluation failure
    , failLabel :: Address
    -- | The number of items that have been pushed onto the stack since the fail
    -- label was last set
    , failStackCounter :: Int
    -- | How many items down in the stack the first item of the current call stack frame is
    , callStackFrameStart :: Int
    -- | Addresses of jumps created by the compiler
    , systemLabels :: Vector Address
    }
    
-- | Initial state of the stack machine
emptyStackMachineState = StackMachineState emptyStack (-1) 0 0 V.empty

-- | Monad for running the stack machine
newtype StackMachine expr m a = StackMachine
    { runStackMachineInternal :: StateT (StackMachineState expr) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | Run the stack machine
runStackMachine :: Monad m => StackMachine expr m a -> m a
runStackMachine f = flip evalStateT emptyStackMachineState $ runStackMachineInternal $ f

instance InterpreterShell m => InterpreterShell (StackMachine expr m) where
    input = lift input
    output = lift . output
    punch = lift . punch
    lastOutput = lift lastOutput
    lastPunch = lift lastPunch
    date = lift date
    time = lift time

-- | An empty stack
emptyStack :: Stack expr
emptyStack = S.empty

-- | Modify the state of the stack machine
modifyStackMachineState :: Monad m => (StackMachineState expr -> StackMachineState expr) -> StackMachine expr m ()
modifyStackMachineState f = StackMachine $ modify f

getCallStackFrameStart :: Monad m => StackMachine expr m Int
getCallStackFrameStart = StackMachine $ gets callStackFrameStart

putCallStackFrameStart :: Monad m => Int -> StackMachine expr m ()
putCallStackFrameStart depth = modifyStackMachineState $ \st -> st{callStackFrameStart=depth}

modifyCallStackFrameStart :: Monad m => (Int -> Int) -> StackMachine expr m ()
modifyCallStackFrameStart f = getCallStackFrameStart >>= putCallStackFrameStart . f

pushCallStackFrame :: Monad m => Int -> StackMachine expr m ()
pushCallStackFrame depth = do
    oldDepth <- getCallStackFrameStart
    push $ IntegerData $ mkInteger oldDepth
    putCallStackFrameStart depth

popCallStackFrame :: Monad m => StackMachine expr m (Maybe ())
popCallStackFrame = do
    oldDepth <- pop
    case oldDepth of
        Just (IntegerData oldDepth) -> do
            putCallStackFrameStart $ unmkInteger oldDepth
            return $ Just ()
        _ -> return Nothing

popToCallStackFrame :: Monad m => StackMachine expr m ()
popToCallStackFrame = do
    depth <- getCallStackFrameStart
    replicateM_ depth pop

-- | Get the address to jump to in case of failure
getFailLabel :: Monad m => StackMachine expr m Address
getFailLabel = StackMachine $ gets failLabel

-- | Set the label to jump to in case of failure
setFailLabel :: Monad m => Address -> StackMachine expr m ()
setFailLabel addr = do
    modifyStackMachineState $ \st -> st { failLabel = addr }
    modifyFailStackCounter (const 0)

-- | Set the labels for compiler-created jumps
putSystemLabels :: Monad m => Vector Address -> StackMachine expr m ()
putSystemLabels lbls = modifyStackMachineState $ \st -> st { systemLabels = lbls }

-- | Look up a compiled-created jump address
lookupSystemLabel :: Monad m => SystemLabel -> StackMachine expr m Address
lookupSystemLabel (SystemLabel ix) = StackMachine $ gets $ (V.! unmkInteger ix) . systemLabels

-- | Modify the counter that tracks the number of items on the stack since the
-- fail label was last set
modifyFailStackCounter :: Monad m => (Int -> Int) -> StackMachine expr m ()
modifyFailStackCounter f = modifyStackMachineState $ \st -> st{ failStackCounter = f $ failStackCounter st }

-- | Modify the stack
modifyStack :: Monad m => (Stack expr -> Stack expr) -> StackMachine expr m ()
modifyStack f = modifyStackMachineState $ \st -> st{ stack = f $ stack st }

-- | Retreive the stack
getStack :: Monad m => StackMachine expr m (Stack expr)
getStack = StackMachine $ gets stack

-- | Set the stack
putStack :: Monad m => Stack expr -> StackMachine expr m ()
putStack = modifyStack . const

-- | Push an item onto the stack
push :: Monad m => Data expr -> StackMachine expr m ()
push x = do
    modifyStack $ S.push x
    modifyFailStackCounter (+1)
    modifyCallStackFrameStart (+1)
    
-- | Pop an item off the stack
pop :: Monad m => StackMachine expr m (Maybe (Data expr))
pop = do
    stk <- getStack
    case S.pop stk of
        Just (x,stk') -> do
            putStack stk'
            modifyFailStackCounter (subtract 1)
            modifyCallStackFrameStart (subtract 1)
            return $ Just x
        _ -> return Nothing

-- | Pop each item that was pushed since the fail label was last set
popFailStack :: Monad m => StackMachine expr m ()
popFailStack = do
    n <- StackMachine $ gets failStackCounter
    replicateM n pop
    modifyFailStackCounter (const 0)
    return ()

{-
instance (Monad m) => NewSnobol4Machine (StackMachine expr m) where
    type ProgramType (StackMachine expr m) = CompiledProgram
    type ExprType (StackMachine expr m) = expr
    type FuncType (StackMachine expr m) = ()
    type ArgType (StackMachine expr m) = ()
    eval = undefined
-}

instance (Monad m) => NewSnobol4Machine (StackMachine ExprKey m) where
    type ProgramType (StackMachine ExprKey m) = CompiledProgram
    type ExprType (StackMachine ExprKey m) = Address
    type FuncType (StackMachine ExprKey m) = Symbol
    type ArgType (StackMachine ExprKey m) = ()
    eval = undefined

instance (Monad m) => LocalVariablesClass (StackMachine ExprKey m) where
    lookupLocal ix = do
        frameStackStart <- getCallStackFrameStart
        let stackIx = frameStackStart - ix
        liftM (S.get stackIx) getStack
    writeLocal ix x = do
        stk <- getStack
        frameStackStart <- getCallStackFrameStart
        let stackIx = frameStackStart - ix
        case S.set stackIx x stk of 
            Just stk' -> do
                putStack stk'
                return $ Just ()
            Nothing -> return Nothing

getStackList :: Monad m => StackMachine expr m [Data expr]
getStackList = liftM S.toList $ getStack
