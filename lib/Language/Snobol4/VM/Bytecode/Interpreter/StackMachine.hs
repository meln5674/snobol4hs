{-|
Module          : Language.Snobol4.VM.Bytecode.Interpreter.StackMachine
Description     : Stack machine for the VM
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

The stack machine contains a stack of values, as well as a number of registers.

The stack contains "Frames" which are all of the values relevant to the current
    user-defined function. A frame contains the return value, return address,
    arguments, local variables, and stored references to variables with the same
    names as the above. The stack machine contains a register which tracks the
    number of items in the current frame. This register is pushed onto the stack
    when a new frame is created, and popped when a frame is removed.

The stack machine also contains the "Fail stack", along with a "Fail address"
    register. This register contains the address to jump to when an instruction
    fails. Another register tracks how many items were pushed since the last
    time the Fail Address was changed. This allows the stack to create
    checkpoints that can be restored before passing control to an error handler.
    These two registers can be pushed and popped, allowing for nested levels of
    failure handling.

The stack machine is also in charge of maintaining the list of compiler
    generated and dynamically created labels.
-}



module Language.Snobol4.VM.Bytecode.Interpreter.StackMachine
    ( StackMachine 
    , StackMachineState

    -- * Running the stack machine    
    , runStackMachine
    , initStackMachine
    , resumeStackMachine
    
    -- * Basic stack manipulation
    , push 
    , pop 
    
    , getFailLabel 
    , putFailLabel 
    
    -- * Managing compiler generated and dynaimcally generated labels
    , putSystemLabels 
    , lookupSystemLabel 
    
    -- * Failure handling
    , popFailStack 
    , pushFailLabel
    , popFailLabel
    
    -- * Call stack
    , pushCallStackFrame 
    , popCallStackFrame 
    , popToCallStackFrame 
    
    -- * Debugging and utilities
    , getStackList 
    , getCallStackFrameStart 
    
    -- * Handlers for defining class instances
    , mkStackMachine
    ) where

import Language.Snobol4.VM.Bytecode.Interpreter.StackMachine.Internal
