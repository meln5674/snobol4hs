{-# LANGUAGE ScopedTypeVariables #-}
module Language.Snobol4.Interpreter.State where

import Language.Snobol4.Interpreter.Internal.Types
import Language.Snobol4.Interpreter.Primitives
import Language.Snobol4.Interpreter.Shell

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V


-- | A ProgramState with no variable, statements, or labels, pointed at the 
-- first statement
emptyState :: forall m . InterpreterShell m => ProgramState m
emptyState = ProgramState
    (M.fromList primitiveVars) 
    V.empty 
    M.empty 
    0 
    (M.fromList $ zip (map funcName (primitiveFunctions :: [Function m])) primitiveFunctions)
    []
