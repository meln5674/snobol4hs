{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Language.Snobol4.Interpreter.Internal.StackMachine where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import qualified Data.Vector as V

import Language.Snobol4.Interpreter.Internal.Types
import Language.Snobol4.Interpreter.Internal.CallStack (CallStackClass)
import qualified Language.Snobol4.Interpreter.Internal.CallStack as CS
import Language.Snobol4.Interpreter.Internal.StateMachine
import Language.Snobol4.Interpreter.Internal.StateMachine.Types


-- | An empty stack
emptyStack :: Stack a
emptyStack = []

-- | A node of the call stack

emptyStackMachineState = StackMachineState emptyStack

-- | Run the stack machine
runStackMachine :: Monad m => StackMachine m a -> m a
runStackMachine f = flip evalStateT emptyStackMachineState $ runStackMachineInternal $ f

getStack :: Monad m => StackMachine m (Stack CallStackFrame)
getStack = StackMachine $ gets stack

modifyStack :: Monad m => (Stack CallStackFrame -> Stack CallStackFrame) -> StackMachine m ()
modifyStack f = StackMachine $ modify $ \st -> st{ stack = f $ stack st }

putStack :: Monad m => Stack CallStackFrame -> StackMachine m ()
putStack = modifyStack . const

instance (Monad m) => LocalVariablesClass (StackMachine m) where
    lookupLocal ix = do
        stk <- getStack
        case stk of
            [] -> return Nothing
            (frame:_) -> return $ Just $ locals frame V.! ix
    writeLocal ix x = do
        stk <- getStack
        case stk of
            [] -> return Nothing
            (frame:frames) -> do
                putStack $ frame{locals = (locals frame) V.// [(ix,x)]} : frames
                return $ Just ()

instance (Monad m) => CallStackClass (StackMachine m) where
    getStack = getStack
    putStack = putStack
    modifyStack = modifyStack
