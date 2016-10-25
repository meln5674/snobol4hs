{-|
Module          : Language.Snobol4.VM.Bytecode.Interpreter
Description     : The SNOBOL4 Virtual Machine Interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown


Public interface to the SNOBOL4 virtual machine

The virtual machine can be run in one of three ways, continuous, interupted, and
    monad.
    
To run the virtual machine in continuous mode, provide a program and symbol
    table to the run function. The program will run until it terminates.

@
main = do
    program <- loadUserProgram
    table <- loadUserSymbolTable
    shell $ do
        start
        run program table
@


To run the virtual machine in interupted mode, provide a program and symbol
    table to the initAndPauseVM function. This will create a new VM and
    provide a token to access it. The next instruction can be executed
    by passing this token to the stepAndPauseVM function.

@
main = do
    program <- loadUserProgram
    table <- loadUserSymbolTable
    vm <- shell $ do
        start
        initAndPauseVM program table
    vm2 <- shell $ stepAndPauseVM vm
    -- etc
@

To run the virtual machine in monad mode, provide a program and symbol table to
    the initVM method. This will produce a monadic value that, when executed,
    will start the virtual machine. This can then be bound with other actions,
    such as stepVM or execLookup. To run this computation, pass the complete
    monadic value to runVM.

@
main = do
    program <- loadUserProgram
    table <- loadUserSymbolTable
    shell $ do
        start
        runVM $ runUserTransformerT $ do
            lift $ initVM program table
            userOutputTransformerFunction
            lift $ stepVM
            -- ...
@

-}


module Language.Snobol4.VM.Bytecode.Interpreter 
    ( VM
    
    -- * Continuous mode
    , run
    
    -- * Interupted mode
    , PausedVM
    , initAndPauseVM
    , stepAndPauseVM
    , runAndPauseVM
    
    -- * Monad mode
    , runVM
    , initVM
    , stepVM
    
    -- * Utilities
    , getLoadedProgram
    , getProgramCounter
    , getCallStackFrameStart
    , execLookup
    , toString
    , getStack
    
    -- * Helpers for defining instances
    , mkVM
    , StateExcept
    , MkStateExcept
    , RunStateExcept
    , StateExceptBuilder
    ) where

import Language.Snobol4.VM.Bytecode.Interpreter.Types
import Language.Snobol4.VM.Bytecode.Interpreter.Wrappers
import Language.Snobol4.VM.Bytecode.Interpreter.Internal
