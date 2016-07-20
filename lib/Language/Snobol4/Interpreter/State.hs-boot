module Language.Snobol4.Interpreter.State where

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.Types

emptyState :: InterpreterShell m => ProgramState m

addPrimitives :: InterpreterShell m => Interpreter m ()

