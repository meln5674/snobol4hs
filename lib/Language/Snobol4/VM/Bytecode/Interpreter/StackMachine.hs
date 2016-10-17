module Language.Snobol4.VM.Bytecode.Interpreter.StackMachine
    ( StackMachine 
    , ExprKey 
    , runStackMachine 
    , push 
    , pop 
    , getFailLabel 
    , setFailLabel 
    , putSystemLabels 
    , lookupSystemLabel 
    , popFailStack 
    , pushCallStackFrame 
    , popCallStackFrame 
    , popToCallStackFrame 
    , getStackList 
    , getCallStackFrameStart 
    ) where

import Language.Snobol4.VM.Bytecode.Interpreter.StackMachine.Internal