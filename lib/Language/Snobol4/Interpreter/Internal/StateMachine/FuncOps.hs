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
--import Language.Snobol4.Interpreter.Internal.StateMachine.Run

import qualified Data.Map as M

import Language.Snobol4.Interpreter.Internal.StateMachine.Types


-- | Empty collection of functions
noFunctions :: ( InterpreterShell m
--               {-, Snobol4Machine program-} 
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

selectorFunctionsNew :: ( InterpreterShell m
                        )
                     => Snobol4String
                     -> Snobol4String
                     -> Int
                     -> InterpreterGeneric program m ()
selectorFunctionsNew name dataname ix = modifyFunctions $ M.insert name $ DataSelectorFunction name dataname ix

constructorFunctionsNew :: ( InterpreterShell m
                           )
                        => Snobol4String
                        -> Int
                        -> InterpreterGeneric program m ()
constructorFunctionsNew name count = modifyFunctions $ M.insert name $ DataConstructorFunction name count


noUnOpSyns :: ( InterpreterShell m
              {-, Snobol4Machine program-}
              )
           => OpSyns program m
noUnOpSyns = M.empty


noBinOpSyns :: ( InterpreterShell m
               {-, Snobol4Machine program-}
               ) 
            => OpSyns program m
noBinOpSyns = M.empty

getUnOpSyns :: ( InterpreterShell m
               {-, Snobol4Machine program-}
               ) 
            => InterpreterGeneric program m (OpSyns program m)
getUnOpSyns = getsProgramState unOpSyns

getsUnOpSyns :: ( InterpreterShell m
                {-, Snobol4Machine program-}
                ) 
             => (OpSyns program m -> a)
             -> InterpreterGeneric program m a
getsUnOpSyns f = liftM f getUnOpSyns

getBinOpSyns :: ( InterpreterShell m
               {-, Snobol4Machine program-}
               ) 
            => InterpreterGeneric program m (OpSyns program m)
getBinOpSyns = getsProgramState binOpSyns

getsBinOpSyns :: ( InterpreterShell m
                {-, Snobol4Machine program-}
                ) 
             => (OpSyns program m -> a)
             -> InterpreterGeneric program m a
getsBinOpSyns f = liftM f getBinOpSyns

putUnOpSyns :: ( InterpreterShell m
               {-, Snobol4Machine program-}
               ) 
            => OpSyns program m
            -> InterpreterGeneric program m ()
putUnOpSyns ops = modifyProgramState $
    \st -> st { unOpSyns = ops }

putBinOpSyns :: ( InterpreterShell m
               {-, Snobol4Machine program-}
               ) 
            => OpSyns program m
            -> InterpreterGeneric program m ()
putBinOpSyns ops = modifyProgramState $
    \st -> st { binOpSyns = ops }



modifyUnOpSyns :: ( InterpreterShell m
                  {-, Snobol4Machine program-}
                  ) 
               => (OpSyns program m -> OpSyns program m)
               -> InterpreterGeneric program m ()
modifyUnOpSyns f = modifyProgramState $
    \st -> st { unOpSyns = f $ unOpSyns st }

modifyBinOpSyns :: ( InterpreterShell m
                  {-, Snobol4Machine program-}
                  ) 
               => (OpSyns program m -> OpSyns program m)
               -> InterpreterGeneric program m ()
modifyBinOpSyns f = modifyProgramState $
    \st -> st { binOpSyns = f $ binOpSyns st }

lookupUnOpSyn :: ( InterpreterShell m
                 {-, Snobol4Machine program-}
                 ) 
              => Operator
              -> InterpreterGeneric program m (Maybe (OpSyn program m))
lookupUnOpSyn = getsUnOpSyns . M.lookup

lookupBinOpSyn :: ( InterpreterShell m
                 {-, Snobol4Machine program-}
                 ) 
              => Operator
              -> InterpreterGeneric program m (Maybe (OpSyn program m))
lookupBinOpSyn = getsBinOpSyns . M.lookup

setUnOpSyn :: ( InterpreterShell m
              {-, Snobol4Machine program-}
              ) 
           => Operator
           -> OpSyn program m
           -> InterpreterGeneric program m ()
setUnOpSyn op syn = modifyUnOpSyns $ M.insert op syn

setBinOpSyn :: ( InterpreterShell m
              {-, Snobol4Machine program-}
              ) 
           => Operator
           -> OpSyn program m
           -> InterpreterGeneric program m ()
setBinOpSyn op syn = modifyBinOpSyns $ M.insert op syn
