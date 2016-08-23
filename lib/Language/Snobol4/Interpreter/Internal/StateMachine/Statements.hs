{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Statements
Description     : Maintaining the loaded program
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.Statements where

import qualified Data.Map as M
import qualified Data.Vector as V

import Language.Snobol4.Syntax.AST hiding (getProgram)

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC

-- | Get the loaded program
getProgram :: InterpreterShell m => InterpreterGeneric program instruction m program
getProgram = getsProgramState program

-- | Set the loaded program
putProgram :: InterpreterShell m => program -> InterpreterGeneric program instruction m ()
putProgram prog = modifyProgramState $ \st -> st { program = prog }

-- | Apply a function to the loaded program
modifyStatements :: InterpreterShell m 
                 => (program -> program) 
                 -> InterpreterGeneric program instruction m ()
modifyStatements f = modifyProgramState $
    \st -> st { program = f $ program st }

-- | Fetch the next statement to execute
fetch :: ( InterpreterShell m, ProgramClass program instruction ) 
      => InterpreterGeneric program instruction m instruction
fetch = getInstruction <$> getProgramCounter <*> getProgram


