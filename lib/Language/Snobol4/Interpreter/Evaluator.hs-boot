module Language.Snobol4.Interpreter.Evaluator where

import Prelude hiding (toInteger)

import Text.Read hiding (lift, String, step, get)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.Types

arithmetic :: InterpreterShell m 
           => (Int -> Int -> Int) 
           -> (Float -> Float -> Float)
           -> Data 
           -> Data 
           -> Evaluator m Data

pattern :: InterpreterShell m => (Pattern -> Pattern -> Pattern) -> Data -> Data -> Evaluator m Data

-- | Evaluate a binary operation on data
evalOp :: InterpreterShell m => Operator -> Data -> Data -> Evaluator m Data

-- | Evaluate an expression
evalExpr :: InterpreterShell m => Expr -> Evaluator m Data

execLookup :: InterpreterShell m => Lookup -> Evaluator m (Maybe Data) 

-- | Take an evaluation and return it to the interpreter stack, with a handler 
-- for a failed evaluation
catchEval :: InterpreterShell m 
          => Evaluator m a 
          -> (EvalStop -> Interpreter m a)
          -> Interpreter m a
