{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Arrays
Description     : Maintaining arrays
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE LambdaCase #-}
module Language.Snobol4.Interpreter.Internal.StateMachine.Arrays where

import qualified Data.Map as M

import Control.Monad

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC

-- | Empty collection of arrays
noArrays :: (Arrays expr)
noArrays = M.empty

-- | Get the arrays known to the interpreter
getArrays :: InterpreterShell m => InterpreterGeneric program m (Arrays (ExprType m))
getArrays = getsProgramState arrays

-- | Set the arrays known to the interpreter
putArrays :: InterpreterShell m => (Arrays (ExprType m)) -> InterpreterGeneric program m ()
putArrays arrs = modifyProgramState $ \st -> st { arrays = arrs }

-- | Apply a function to the arrays known to the interpreter
modifyArrays :: InterpreterShell m 
             => ((Arrays (ExprType m)) -> (Arrays (ExprType m)))
             -> InterpreterGeneric program m ()
modifyArrays f = modifyProgramState $
    \st -> st { arrays = f $ arrays st }

arraysNextKey :: InterpreterShell m => InterpreterGeneric program m ArrayKey
arraysNextKey = liftM (maybe (toEnum 0) (succ . fst . fst) . M.maxViewWithKey) getArrays 

-- | Allocate a new array with an upper and lower bound each set to an intital value
arraysNew :: InterpreterShell m => Snobol4Integer -> Snobol4Integer -> (Data (ExprType m)) -> InterpreterGeneric program m ArrayKey
arraysNew minIx maxIx v = arraysNew' $ newArray minIx maxIx v

-- | Add a new array
arraysNew' :: InterpreterShell m => (Snobol4Array (ExprType m)) -> InterpreterGeneric program m ArrayKey
arraysNew' arr = do
    newKey <- arraysNextKey
    modifyArrays $ M.insert newKey $ newRef $ arr
    return newKey

-- | Allocate a new array with the provided dimensions and initial value
arraysNew'' :: InterpreterShell m 
           => [(Snobol4Integer,Snobol4Integer)]
           -> (Data (ExprType m))
           -> InterpreterGeneric program m (Data (ExprType m))
arraysNew'' [] val = return val
arraysNew'' ((minIx,maxIx):ds) val = do
    newKey <- arraysNextKey
    xs <- forM [minIx..maxIx] $ \ix -> do
        v <- arraysNew'' ds val
        return (ix,v)
    modifyArrays $ M.insert newKey $ newRef $ newArray' xs
    return $ ArrayData newKey

-- | Create a new array that is a copy of an existing one
arraysCopy :: InterpreterShell m => ArrayKey -> InterpreterGeneric program m (Maybe ArrayKey)
arraysCopy k = do
    result <- arraysLookup k
    case result of
        Nothing -> return Nothing
        Just arr -> do
            newKey <- arraysNextKey
            modifyArrays $ M.insert newKey $ newRef arr
            return $ Just newKey

-- | Lookup an array
arraysLookup :: InterpreterShell m => ArrayKey -> InterpreterGeneric program m (Maybe (Snobol4Array (ExprType m)))
arraysLookup k = fmap getRefItem <$> M.lookup k <$> getArrays

-- | Apply a function to an array
arraysUpdate :: InterpreterShell m => ((Snobol4Array (ExprType m)) -> (Snobol4Array (ExprType m))) -> ArrayKey -> InterpreterGeneric program m ()
arraysUpdate f k = modifyArrays $ M.adjust (fmap f) k

-- | Get the value of an array with the given index
arraysRead :: InterpreterShell m => Snobol4Integer -> ArrayKey -> InterpreterGeneric program m (Maybe (Data (ExprType m)))
arraysRead ix k = arraysLookup k >>= \x -> return $ x >>= readArray ix

-- | Set the value of an array with the given index
arraysWrite :: InterpreterShell m => Snobol4Integer -> (Data (ExprType m)) -> ArrayKey -> InterpreterGeneric program m ()
arraysWrite ix v = arraysUpdate $ writeArray ix v

-- | Increment the reference counter for an array
arraysIncRef :: InterpreterShell m => ArrayKey -> InterpreterGeneric program m ()
arraysIncRef k = modifyArrays $ M.adjust incRefCount k

-- | Decrement the reference counter for an array
arraysDecRef :: InterpreterShell m => ArrayKey -> InterpreterGeneric program m ()
arraysDecRef k = modifyArrays $ M.update decRefCount k
