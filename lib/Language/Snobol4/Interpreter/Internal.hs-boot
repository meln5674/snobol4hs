module Language.Snobol4.Interpreter.Internal where

import Prelude hiding ( toInteger, lookup )

import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import Language.Snobol4.Syntax.AST

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error

import Language.Snobol4.Interpreter.Internal.Types

call :: InterpreterShell m => Snobol4String -> [Data] -> Interpreter m (Maybe Data)

evalExpr :: InterpreterShell m => Expr -> Evaluator m Data
