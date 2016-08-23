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

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell

import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState

-- | Call stack with no nodes
emptyCallStack :: [CallStackNode]
emptyCallStack = []

-- | Get the call stack
getCallStack :: InterpreterShell m => InterpreterGeneric program instruction m [CallStackNode]
getCallStack = getsProgramState callStack

-- | Set the call stack
putCallStack :: InterpreterShell m => [CallStackNode] -> InterpreterGeneric program instruction m ()
putCallStack stk = modifyProgramState $ \st -> st { callStack = stk }

-- | Apply a function to the call stack
modifyCallStack :: InterpreterShell m => ([CallStackNode] -> [CallStackNode]) -> InterpreterGeneric program instruction m ()
modifyCallStack f = modifyProgramState $
    \st -> st { callStack = f $ callStack st }




-- | Apply a function to the head of the call stack
modifyCallStackHead :: InterpreterShell m => (CallStackNode -> CallStackNode) -> InterpreterGeneric program instruction m ()
modifyCallStackHead f = modifyCallStack $ \(n:ns) -> f n:ns

-- | Write a local variable to the top of the call stack
writeCallStackLocal :: InterpreterShell m => Snobol4String -> Data -> InterpreterGeneric program instruction m ()
writeCallStackLocal name val = modifyCallStackHead $ 
    \n -> n { locals = M.insert name val $ locals n }

-- | Push a node onto the call stack
pushCallStack :: InterpreterShell m => CallStackNode -> InterpreterGeneric program instruction m ()
pushCallStack n = modifyCallStack (n:)

-- | Pop a node off of the call stack and set the program counter accordingly
popCallStack :: InterpreterShell m => InterpreterGeneric program instruction m CallStackNode
popCallStack = do
    n <- head <$> getCallStack 
    modifyCallStack $ \(_:ns) -> ns
    putProgramCounter $ returnAddr n
    return n

-- | Push a node onto the call stack for calling a function
pushFuncNode :: InterpreterShell m => Function program instruction m -> InterpreterGeneric program instruction m ()
pushFuncNode f = do
    pc <- getProgramCounter
    pushCallStack
        Node 
        { callName = funcName f
        , locals = M.fromList $ map (\x -> (x,StringData  nullString))
                              $ funcName f : localNames f ++ formalArgs f
        , returnAddr = pc
        }
