{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.CallStack
Description     : Interpreter call stack
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.CallStack where

import qualified Data.Map as M
import qualified Data.Vector as V

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell

import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState

-- | Call stack with no Frames
emptyCallStack :: [CallStackFrame]
emptyCallStack = []

-- | Get the call stack
getCallStack :: InterpreterShell m => InterpreterGeneric program m [CallStackFrame]
getCallStack = getsProgramState callStack

-- | Set the call stack
putCallStack :: InterpreterShell m => [CallStackFrame] -> InterpreterGeneric program m ()
putCallStack stk = modifyProgramState $ \st -> st { callStack = stk }

-- | Apply a function to the call stack
modifyCallStack :: InterpreterShell m => ([CallStackFrame] -> [CallStackFrame]) -> InterpreterGeneric program m ()
modifyCallStack f = modifyProgramState $
    \st -> st { callStack = f $ callStack st }




-- | Apply a function to the head of the call stack
modifyCallStackHead :: InterpreterShell m 
                    => (CallStackFrame 
                    -> CallStackFrame) 
                    -> InterpreterGeneric program m ()
modifyCallStackHead f = modifyCallStack $ \(n:ns) -> f n:ns

-- | Write a local variable to the top of the call stack
writeCallStackLocal :: InterpreterShell m 
                    => Int
                    -> Data 
                    -> InterpreterGeneric program m ()
writeCallStackLocal ix val = modifyCallStackHead $ 
    \n -> n { locals = locals n V.// [(ix, val)] }

-- | Push a Frame onto the call stack
pushCallStack :: InterpreterShell m => CallStackFrame -> InterpreterGeneric program m ()
pushCallStack n = modifyCallStack (n:)

-- | Pop a Frame off of the call stack and set the program counter accordingly
popCallStack :: InterpreterShell m => InterpreterGeneric program m CallStackFrame
popCallStack = do
    n <- head <$> getCallStack 
    modifyCallStack $ \(_:ns) -> ns
    putProgramCounter $ returnAddr n
    return n

-- | Push a Frame onto the call stack for calling a function
pushFuncFrame :: InterpreterShell m 
             => UserFunction
             -> InterpreterGeneric program m ()
pushFuncFrame f = do
    pc <- getProgramCounter
    let allLocalNames = funcName f : localNames f ++ formalArgs f
        localsList = map (\(name,ix) -> (name,LocalVar ix)) $ zip allLocalNames [0..]
    pushCallStack
        Frame 
        { callName = funcName f
        , locals = V.replicate (length allLocalNames) $ StringData nullString
        , oldReferences = []
        , returnAddr = pc
        }
