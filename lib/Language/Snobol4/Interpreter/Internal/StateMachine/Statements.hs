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

import Language.Snobol4.Syntax.AST

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC

-- | Empty program
noStatements :: Statements
noStatements = V.empty

-- | Get the loaded program
getStatements :: InterpreterShell m => Interpreter m Statements
getStatements = getsProgramState statements

-- | Set the loaded program
putStatements :: InterpreterShell m => Statements -> Interpreter m ()
putStatements stmts = modifyProgramState $ \st -> st { statements = stmts }

-- | Apply a function to the loaded program
modifyStatements :: InterpreterShell m => (Statements -> Statements) -> Interpreter m ()
modifyStatements f = modifyProgramState $
    \st -> st { statements = f $ statements st }

-- | Fetch the next statement to execute
fetch :: InterpreterShell m => Interpreter m Stmt
fetch = (V.!) <$> getStatements <*> (unmkInteger . getAddress <$> getProgramCounter)


