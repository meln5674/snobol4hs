module Language.Snobol4.Interpreter.Internal.StateMachine.CallStack where

import qualified Data.Map as M

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell

import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState

emptyCallStack :: [CallStackNode]
emptyCallStack = []

-- | Get the call stack
getCallStack :: InterpreterShell m => Interpreter m [CallStackNode]
getCallStack = getsProgramState callStack

-- | Set the call stack
putCallStack :: InterpreterShell m => [CallStackNode] -> Interpreter m ()
putCallStack stk = modifyProgramState $ \st -> st { callStack = stk }

-- | Apply a function to the call stack
modifyCallStack :: InterpreterShell m => ([CallStackNode] -> [CallStackNode]) -> Interpreter m ()
modifyCallStack f = modifyProgramState $
    \st -> st { callStack = f $ callStack st }




-- | Apply a function to the head of the call stack
modifyCallStackHead :: InterpreterShell m => (CallStackNode -> CallStackNode) -> Interpreter m ()
modifyCallStackHead f = modifyCallStack $ \(n:ns) -> f n:ns

writeCallStackLocal :: InterpreterShell m => Snobol4String -> Data -> Interpreter m ()
writeCallStackLocal name val = modifyCallStackHead $ 
    \n -> n { locals = M.insert name val $ locals n }

-- | Push a node onto the call stack
pushCallStack :: InterpreterShell m => CallStackNode -> Interpreter m ()
pushCallStack n = modifyCallStack (n:)

-- | Pop a node off of the call stack and set the program counter accordingly
popCallStack :: InterpreterShell m => Interpreter m CallStackNode
popCallStack = do
    n <- head <$> getCallStack 
    modifyCallStack $ \(_:ns) -> ns
    putProgramCounter $ returnAddr n
    return n

-- | Push a node onto the call stack for calling a function
pushFuncNode :: InterpreterShell m => Function m -> Interpreter m ()
pushFuncNode f = do
    pc <- getProgramCounter
    pushCallStack
        Node 
        { callName = funcName f
        , locals = M.fromList $ map (\x -> (x,StringData  nullString))
                              $ funcName f : localNames f ++ formalArgs f
        , returnAddr = pc
        }
