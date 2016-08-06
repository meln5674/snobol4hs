module Language.Snobol4.Interpreter.Evaluator where

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Internal.StateMachine

-- | Evaluate an expression
evalExpr :: InterpreterShell m => Expr -> Evaluator m Data
