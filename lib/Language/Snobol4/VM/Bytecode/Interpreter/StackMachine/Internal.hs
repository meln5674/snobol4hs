{-|
Module          : Language.Snobol4.VM.Bytecode.Interpreter.StackMachine.Internal
Description     : Stack machine for the VM (Internals)
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

Internals for the stack machine.

See Language.Snobol4.VM.Bytecode.Interpreter.StackMachine for overview

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Language.Snobol4.VM.Bytecode.Interpreter.StackMachine.Internal where

import qualified Data.Stack as S
import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Language.Snobol4.Interpreter.Data hiding (Data)
import qualified Language.Snobol4.Interpreter.Data as I



import Language.Snobol4.VM.Bytecode (SystemLabel(..), CompiledProgram, Symbol, ExprKey)

import Language.Snobol4.Interpreter.Internal.StateMachine.Types hiding (Return, FReturn)


import Language.Snobol4.Interpreter.Shell

-- | Synonym for data used by the stack machine
type Data = I.Data ExprKey

-- | Synonym for the stack used by the stack machine
type Stack = S.Stack Data


-- | State of the stack machine
data StackMachineState
    = StackMachineState
    { 
    -- | The stack
      stack :: Stack
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
newtype StackMachine m a = StackMachine
    { runStackMachineInternal :: StateT StackMachineState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | 
type instance ProgramType (StackMachine m) = CompiledProgram

-- | 
type instance ExprType (StackMachine m) = ExprKey

-- | 
type instance FuncType (StackMachine m) = Symbol
--type ArgType (StackMachine ExprKey m) = ()

-- | Run the stack machine
runStackMachine :: Monad m => StackMachine m a -> m a
runStackMachine f = flip evalStateT emptyStackMachineState $ runStackMachineInternal $ f

-- | Initialize a stack machine
initStackMachine :: Monad m => m StackMachineState
initStackMachine = liftM snd $ resumeStackMachine (return ()) emptyStackMachineState

-- | Run a paused stack machine
resumeStackMachine :: Monad m => StackMachine m a -> StackMachineState -> m (a, StackMachineState)
resumeStackMachine = runStateT . runStackMachineInternal

-- | Don't ask
--
-- See Language.Snobol4.VM.Bytecode.Interpreter.Internal.mkVM
mkStackMachine :: Monad m 
               => ( forall s
                  .  ( forall b . StackMachine m b -> s -> m (b, s) )
                  -> ( forall b . (s -> m (b, s)) -> StackMachine m b )
                  -> ( s -> m (a, s) )
                  ) 
               -> StackMachine m a
mkStackMachine f = StackMachine $ StateT $ f runFunc stateFunc
  where
    runFunc g st = runStateT (runStackMachineInternal g) st
    stateFunc = StackMachine . StateT

-- | Lift another InterpreterShell's methods
instance InterpreterShell m => InterpreterShell (StackMachine m) where
    input = lift input
    output = lift . output
    punch = lift . punch
    lastOutput = lift lastOutput
    lastPunch = lift lastPunch
    date = lift date
    time = lift time

-- | An empty stack
emptyStack :: Stack
emptyStack = S.empty

-- | Modify the state of the stack machine
modifyStackMachineState :: Monad m => (StackMachineState -> StackMachineState) -> StackMachine m ()
modifyStackMachineState f = StackMachine $ modify f

-- | Get the depth of the first item in the current frame
getCallStackFrameStart :: Monad m => StackMachine m Int
getCallStackFrameStart = StackMachine $ gets callStackFrameStart

-- | Set the depth of the first item in the current frame
putCallStackFrameStart :: Monad m => Int -> StackMachine m ()
putCallStackFrameStart depth = modifyStackMachineState $ \st -> st{callStackFrameStart=depth}

-- | Apply a function to the deppth of the first item in the current frame
modifyCallStackFrameStart :: Monad m => (Int -> Int) -> StackMachine m ()
modifyCallStackFrameStart f = getCallStackFrameStart >>= putCallStackFrameStart . f

-- | Get the number of items on the stack since the last fail label was set
getFailStackCounter :: Monad m => StackMachine m Int
getFailStackCounter = StackMachine $ gets failStackCounter

-- | Set the number of items on the stack since the last fail label was set
putFailStackCounter :: Monad m => Int -> StackMachine m ()
putFailStackCounter c = modifyStackMachineState $ \st -> st{ failStackCounter = c }

-- | Push the current depth of the stack frame onto the stack and create a new
-- frame starting at a certain depth
pushCallStackFrame :: Monad m => Int -> StackMachine m ()
pushCallStackFrame depth = do
    oldDepth <- getCallStackFrameStart
    push $ IntegerData $ mkInteger oldDepth
    putCallStackFrameStart depth

-- | Pop a depth off the stack, and set that to be the depth of the current
-- stack frame
popCallStackFrame :: Monad m => StackMachine m (Maybe ())
popCallStackFrame = do
    oldDepth <- pop
    case oldDepth of
        Just (IntegerData oldDepth) -> do
            putCallStackFrameStart $ unmkInteger oldDepth
            return $ Just ()
        _ -> return Nothing

-- | Pop items off the stack until the current frame is empty
popToCallStackFrame :: Monad m => StackMachine m ()
popToCallStackFrame = do
    depth <- getCallStackFrameStart
    replicateM_ depth pop

-- | Get the address to jump to in case of failure
getFailLabel :: Monad m => StackMachine m Address
getFailLabel = StackMachine $ gets failLabel

-- | Set the label to jump to in case of failure
putFailLabel :: Monad m => Address -> StackMachine m ()
putFailLabel addr = do
    modifyStackMachineState $ \st -> st { failLabel = addr }
    modifyFailStackCounter (const 0)

-- | Push the current fail address and size and then set a new label with no
-- items
pushFailLabel :: Monad m => Address -> StackMachine m ()
pushFailLabel lbl = do
    getFailLabel >>= push . IntegerData . getAddress
    getFailStackCounter >>= push . IntegerData . mkInteger
    putFailLabel lbl
    putFailStackCounter 0

-- | Pop the fail address and size off the stack
popFailLabel :: Monad m => StackMachine m (Maybe ())
popFailLabel = do
    counter <- pop
    addr <- pop
    case (counter, addr) of
        (Just (IntegerData counter), Just (IntegerData addr)) -> do
            putFailLabel $ Address addr
            putFailStackCounter $ unmkInteger counter
            return $ Just ()
        _ -> return Nothing


-- | Set the labels for compiler-created jumps
putSystemLabels :: Monad m => Vector Address -> StackMachine m ()
putSystemLabels lbls = modifyStackMachineState $ \st -> st { systemLabels = lbls }

-- | Look up a compiled-created jump address
lookupSystemLabel :: Monad m => SystemLabel -> StackMachine m Address
lookupSystemLabel (SystemLabel ix) = StackMachine $ gets $ (V.! unmkInteger ix) . systemLabels

-- | Modify the counter that tracks the number of items on the stack since the
-- fail label was last set
modifyFailStackCounter :: Monad m => (Int -> Int) -> StackMachine m ()
modifyFailStackCounter f = modifyStackMachineState $ \st -> st{ failStackCounter = f $ failStackCounter st }

-- | Modify the stack
modifyStack :: Monad m => (Stack -> Stack ) -> StackMachine m ()
modifyStack f = modifyStackMachineState $ \st -> st{ stack = f $ stack st }

-- | Retreive the stack
getStack :: Monad m => StackMachine m Stack
getStack = StackMachine $ gets stack

-- | Set the stack
putStack :: Monad m => Stack -> StackMachine m ()
putStack = modifyStack . const

-- | Push an item onto the stack
push :: Monad m => Data -> StackMachine m ()
push x = do
    modifyStack $ S.push x
    modifyFailStackCounter (+1)
    modifyCallStackFrameStart (+1)
    
-- | Pop an item off the stack
pop :: Monad m => StackMachine m (Maybe Data)
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
popFailStack :: Monad m => StackMachine m ()
popFailStack = do
    getFailStackCounter >>= flip replicateM pop
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

-- | Use the current stack frame to store local variables
instance ( Monad m
         ) => LocalVariablesClass (StackMachine m) where
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

-- | Get a list containing the items on the stack
getStackList :: Monad m => StackMachine m [Data]
getStackList = liftM S.toList $ getStack
