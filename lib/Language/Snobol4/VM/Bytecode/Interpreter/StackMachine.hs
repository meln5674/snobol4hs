{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.VM.Bytecode.Interpreter.StackMachine 
    ( StackMachine
    , runStackMachine
    , push
    , pop
    , getFailLabel
    , setFailLabel
    , putSystemLabels
    , lookupSystemLabel
    , popFailStack
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Language.Snobol4.Interpreter.Data

import Language.Snobol4.VM.Bytecode (SystemLabel(..))

import Language.Snobol4.Interpreter.Internal.StateMachine.Types hiding (Return, FReturn)


import Language.Snobol4.Interpreter.Shell

-- | State of the stack machine
data StackMachineState
    = StackMachineState
    { 
    -- | The stack
      stack :: Stack Data
    -- | The label to jump to in case of evaluation failure
    , failLabel :: Address
    -- | The number of items that have been pushed onto the stack since the fail
    -- label was last set
    , failStackCounter :: Int
    -- | Addresses of jumps created by the compiler
    , systemLabels :: Vector Address
    }
    
-- | Initial state of the stack machine
emptyStackMachineState = StackMachineState [] (-1) 0 V.empty

-- | Monad for running the stack machine
newtype StackMachine m a = StackMachine
    { runStackMachineInternal :: StateT StackMachineState m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | Run the stack machine
runStackMachine :: Monad m => StackMachine m a -> m a
runStackMachine f = flip evalStateT emptyStackMachineState $ runStackMachineInternal $ f

instance InterpreterShell m => InterpreterShell (StackMachine m) where
    input = lift input
    output = lift . output
    punch = lift . punch
    lastOutput = lift lastOutput
    lastPunch = lift lastPunch
    date = lift date
    time = lift time

-- | A stack
type Stack = []

-- | An empty stack
emptyStack :: Stack a
emptyStack = []

-- | Modify the state of the stack machine
modifyStackMachineState :: Monad m => (StackMachineState -> StackMachineState) -> StackMachine m ()
modifyStackMachineState f = StackMachine $ modify f

-- | Get the address to jump to in case of failure
getFailLabel :: Monad m => StackMachine m Address
getFailLabel = StackMachine $ gets failLabel

-- | Set the label to jump to in case of failure
setFailLabel :: Monad m => Address -> StackMachine m ()
setFailLabel addr = do
    modifyStackMachineState $ \st -> st { failLabel = addr }
    modifyFailStackCounter (const 0)

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
modifyStack :: Monad m => (Stack Data -> Stack Data) -> StackMachine m ()
modifyStack f = modifyStackMachineState $ \st -> st{ stack = f $ stack st }

-- | Retreive the stack
getStack :: Monad m => StackMachine m (Stack Data)
getStack = StackMachine $ gets stack

-- | Set the stack
putStack :: Monad m => Stack Data -> StackMachine m ()
putStack = modifyStack . const

-- | Push an item onto the stack
push :: Monad m => Data -> StackMachine m ()
push x = do
    modifyStack (x:)
    modifyFailStackCounter (+1)
    
-- | Pop an item off the stack
pop :: Monad m => StackMachine m (Maybe Data)
pop = do
    stk <- getStack
    case stk of
        x:xs -> do
            putStack xs
            modifyFailStackCounter (subtract 1)
            return $ Just x
        _ -> return Nothing

-- | Pop each item that was pushed since the fail label was last set
popFailStack :: Monad m => StackMachine m ()
popFailStack = do
    n <- StackMachine $ gets failStackCounter
    replicateM n pop
    modifyFailStackCounter (const 0)
    return ()
