{-|
Module          : Language.Snobol4.Interpreter
Description     : Functions for dealing with laziness
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Snobol4.Interpreter.Internal.StateMachine.Lazy where

import Prelude hiding (toInteger)

import Control.Monad
import Control.Monad.Trans.Maybe

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Error
import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Error

-- | Class of types which can be extracted from an evaluated thunk
class ( InterpreterShell m
      , NewSnobol4Machine m
      )
   => Forceable a m where
    -- | Extract a value
    fromForce :: Data (ExprType m) -> InterpreterGeneric (ProgramType m) m (Maybe a)

-- | Extract a value, and then wrap it in an evaluated thunk
fromForce' :: ( InterpreterShell m
              , NewSnobol4Machine m
              , Forceable a m
              ) 
           => Data (ExprType m)
           -> InterpreterGeneric (ProgramType m) m (Maybe (Lazy (ExprType m) a))
fromForce' = runMaybeT . liftM EvaluatedThunk . MaybeT . fromForce

-- | Force evaluation of a thunk
force :: ( InterpreterShell m
         , NewSnobol4Machine m
         , Forceable a m
         ) 
      => Lazy (ExprType m) a
      -> InterpreterGeneric (ProgramType m) m (Maybe a)
force (EvaluatedThunk x) = return $ Just x
force (Thunk expr) = runMaybeT $ (MaybeT $ eval expr) >>= MaybeT . fromForce

-- | Force evaluation of a thunk and then wrap in in an evaluated thunk
force' :: ( InterpreterShell m
          , NewSnobol4Machine m
          , Forceable a m
          ) 
       => Lazy (ExprType m) a
       -> MaybeT (InterpreterGeneric (ProgramType m) m) (Lazy (ExprType m) a)
force' = liftM EvaluatedThunk . MaybeT . force
