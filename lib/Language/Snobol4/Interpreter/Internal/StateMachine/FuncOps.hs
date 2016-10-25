{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.FuncOps
Description     : Maintaining functions and operators
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Language.Snobol4.Interpreter.Internal.StateMachine.FuncOps where


import qualified Data.Map as M

import Control.Monad
import Control.Monad.Trans.State.Strict

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Error
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC
import Language.Snobol4.Interpreter.Internal.StateMachine.Variables

import qualified Data.Map as M

import Language.Snobol4.Interpreter.Internal.StateMachine.Types


-- | Empty collection of functions
noFunctions :: ( InterpreterShell m
               ) 
            => Functions program m
noFunctions = M.empty

-- | Get the functions known to the interpreter
getFunctions :: ( InterpreterShell m{-, Snobol4Machine program-} ) => InterpreterGeneric program m (Functions program m)
getFunctions = getsProgramState functions

-- | Set the functions known to the interpreter
putFunctions :: ( InterpreterShell m{-, Snobol4Machine program-} ) => Functions program m -> InterpreterGeneric program m ()
putFunctions funcs = modifyProgramState $ \st -> st { functions = funcs }

-- | Apply a function to the functions known to the interpreter
modifyFunctions :: ( InterpreterShell m{-, Snobol4Machine program-} ) => (Functions program m -> Functions program m) -> InterpreterGeneric program m ()
modifyFunctions f = modifyProgramState $
    \st -> st { functions = f $ functions st }

-- | Erase all functions known to the interpreter
clearFunc :: ( InterpreterShell m{-, Snobol4Machine program-} ) => Snobol4String-> InterpreterGeneric program m ()
clearFunc = modifyFunctions . M.delete

-- | Look up a function by name
funcLookup :: ( InterpreterShell m{-, Snobol4Machine program-} ) => Snobol4String -> InterpreterGeneric program m (Maybe (Function program m))
funcLookup name = M.lookup name <$> getFunctions

-- | Add a new function
functionsNew :: ( InterpreterShell m{-, Snobol4Machine program-} ) 
             => UserFunction 
             -> InterpreterGeneric program m ()
functionsNew func = modifyFunctions $ M.insert (funcName func) $ UserFunction func

-- | Create a new datatype selector function
selectorFunctionsNew :: ( InterpreterShell m
                        )
                     => Snobol4String
                     -> Snobol4String
                     -> Int
                     -> InterpreterGeneric program m ()
selectorFunctionsNew name dataname ix = modifyFunctions $ M.insert name $ DataSelectorFunction name dataname ix

-- | Create a new datatype constructor function
constructorFunctionsNew :: ( InterpreterShell m
                           )
                        => Snobol4String
                        -> Int
                        -> InterpreterGeneric program m ()
constructorFunctionsNew name count = modifyFunctions $ M.insert name $ DataConstructorFunction name count

-- | Empty collection of unary operators
noUnOpSyns :: ( InterpreterShell m
              {-, Snobol4Machine program-}
              )
           => OpSyns program m
noUnOpSyns = M.empty

-- | Empty collectino of binary operators
noBinOpSyns :: ( InterpreterShell m
               {-, Snobol4Machine program-}
               ) 
            => OpSyns program m
noBinOpSyns = M.empty

-- | Get unary operators
getUnOpSyns :: ( InterpreterShell m
               {-, Snobol4Machine program-}
               ) 
            => InterpreterGeneric program m (OpSyns program m)
getUnOpSyns = getsProgramState unOpSyns

-- | Get unary operators and apply a transformation
getsUnOpSyns :: ( InterpreterShell m
                {-, Snobol4Machine program-}
                ) 
             => (OpSyns program m -> a)
             -> InterpreterGeneric program m a
getsUnOpSyns f = liftM f getUnOpSyns

-- | Get binary operators
getBinOpSyns :: ( InterpreterShell m
               {-, Snobol4Machine program-}
               ) 
            => InterpreterGeneric program m (OpSyns program m)
getBinOpSyns = getsProgramState binOpSyns

-- | Get binary operators and apply a transformation
getsBinOpSyns :: ( InterpreterShell m
                {-, Snobol4Machine program-}
                ) 
             => (OpSyns program m -> a)
             -> InterpreterGeneric program m a
getsBinOpSyns f = liftM f getBinOpSyns

-- | Set unary operators
putUnOpSyns :: ( InterpreterShell m
               {-, Snobol4Machine program-}
               ) 
            => OpSyns program m
            -> InterpreterGeneric program m ()
putUnOpSyns ops = modifyProgramState $
    \st -> st { unOpSyns = ops }

-- | Set binary operators
putBinOpSyns :: ( InterpreterShell m
               {-, Snobol4Machine program-}
               ) 
            => OpSyns program m
            -> InterpreterGeneric program m ()
putBinOpSyns ops = modifyProgramState $
    \st -> st { binOpSyns = ops }


-- | Apply a transformation to unary operators
modifyUnOpSyns :: ( InterpreterShell m
                  {-, Snobol4Machine program-}
                  ) 
               => (OpSyns program m -> OpSyns program m)
               -> InterpreterGeneric program m ()
modifyUnOpSyns f = modifyProgramState $
    \st -> st { unOpSyns = f $ unOpSyns st }

-- | Apply a transformation to binary operators
modifyBinOpSyns :: ( InterpreterShell m
                  {-, Snobol4Machine program-}
                  ) 
               => (OpSyns program m -> OpSyns program m)
               -> InterpreterGeneric program m ()
modifyBinOpSyns f = modifyProgramState $
    \st -> st { binOpSyns = f $ binOpSyns st }

-- | Look up a unary operator
lookupUnOpSyn :: ( InterpreterShell m
                 {-, Snobol4Machine program-}
                 ) 
              => Operator
              -> InterpreterGeneric program m (Maybe (OpSyn program m))
lookupUnOpSyn = getsUnOpSyns . M.lookup

-- | Look up a binary operator
lookupBinOpSyn :: ( InterpreterShell m
                 {-, Snobol4Machine program-}
                 ) 
              => Operator
              -> InterpreterGeneric program m (Maybe (OpSyn program m))
lookupBinOpSyn = getsBinOpSyns . M.lookup

-- | Set a unary operator
setUnOpSyn :: ( InterpreterShell m
              {-, Snobol4Machine program-}
              ) 
           => Operator
           -> OpSyn program m
           -> InterpreterGeneric program m ()
setUnOpSyn op syn = modifyUnOpSyns $ M.insert op syn

-- | Set a binary operator
setBinOpSyn :: ( InterpreterShell m
              {-, Snobol4Machine program-}
              ) 
           => Operator
           -> OpSyn program m
           -> InterpreterGeneric program m ()
setBinOpSyn op syn = modifyBinOpSyns $ M.insert op syn
