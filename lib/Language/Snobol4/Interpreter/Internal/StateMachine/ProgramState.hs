{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
Description     : Maintaining the state of the interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Shell

-- | The program counter to start at
initialProgramCounter :: Address
initialProgramCounter = 0

-- | Get the state of the interpreter
getProgramState :: InterpreterShell m => Interpreter m (ProgramState m)
getProgramState = getsProgramState id

-- | Set the state of the interpreter
putProgramState :: InterpreterShell m => ProgramState m -> Interpreter m ()
putProgramState = modifyProgramState . const

-- | Apply an accessor function to the state of the interpreter
getsProgramState :: InterpreterShell m => (ProgramState m -> a) -> Interpreter m a
getsProgramState = Interpreter . lift . gets

-- | Apply an update function to the state of the interpreter
modifyProgramState :: InterpreterShell m 
                   => (ProgramState m
                   -> ProgramState m) 
                   -> Interpreter m ()
modifyProgramState = Interpreter . lift . modify

-- | Get the program counter from the interpreter
getProgramCounter :: InterpreterShell m => Interpreter m Address
getProgramCounter = getsProgramState programCounter


-- | Set the program counter
putProgramCounter :: InterpreterShell m => Address -> Interpreter m ()
putProgramCounter pc = modifyProgramState $ \st -> st { programCounter = pc }


-- | Apply a function to the program counter
modifyProgramCounter :: InterpreterShell m => (Address -> Address) -> Interpreter m ()
modifyProgramCounter f = modifyProgramState $
    \st -> st { programCounter = f $ programCounter st }
