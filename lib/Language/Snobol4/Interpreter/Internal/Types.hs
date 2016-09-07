{-# LANGUAGE TypeFamilies #-}
module Language.Snobol4.Interpreter.Internal.Types where

import qualified Data.Vector as V

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Internal.StateMachine

type PausedInterpreter = PausedInterpreterGeneric Statements

type ProgramState = ProgramStateGeneric Statements

type Interpreter = InterpreterGeneric Statements

type Evaluator = EvaluatorGeneric Statements EvalStop

instance EmptyProgramClass Statements where
    emptyProgram = Statements V.empty

instance ProgramClass Statements where
    type InstructionType Statements = Stmt
    getInstruction (Address ix) (Statements v) = v V.! unmkInteger ix
