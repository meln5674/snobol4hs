module Language.Snobol4.VM.Bytecode.Interpreter.StackMachine
    ( StackMachine 
    , runStackMachine 
    
    , push 
    , pop 
    
    , getFailLabel 
    , putFailLabel 
    
    , putSystemLabels 
    , lookupSystemLabel 
    
    , popFailStack 
    , pushFailLabel
    , popFailLabel
    
    , pushCallStackFrame 
    , popCallStackFrame 
    , popToCallStackFrame 
    
    , getStackList 
    , getCallStackFrameStart 
    ) where

import Language.Snobol4.VM.Bytecode.Interpreter.StackMachine.Internal
