module Language.Snobol4.Interpreter.Evaluator
    ( evalExpr
    , catchEval
    , evalLookup
    ) where

import Prelude hiding (toInteger)

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.Types

-- | Evaluate an expression
evalExpr :: InterpreterShell m => Expr -> Evaluator m Data

-- | Take an evaluation and return it to the interpreter stack, with a handler 
-- for a failed evaluation
catchEval :: InterpreterShell m 
          => Evaluator m a 
          -> (EvalStop -> Interpreter m a)
          -> Interpreter m a

-- | Evaluate an expression as if it were an L-Value
evalLookup :: InterpreterShell m => Expr -> Evaluator m Lookup
