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

class ( InterpreterShell m
      , NewSnobol4Machine m
      )
   => Forceable a m where
    fromForce :: Data (ExprType m) -> InterpreterGeneric (ProgramType m) m (Maybe a)

fromForce' :: ( InterpreterShell m
              , NewSnobol4Machine m
              , Forceable a m
              ) 
           => Data (ExprType m)
           -> InterpreterGeneric (ProgramType m) m (Maybe (Lazy (ExprType m) a))
fromForce' = runMaybeT . liftM EvaluatedThunk . MaybeT . fromForce

-- | Force once level of evaluation of a thunk
force :: ( InterpreterShell m
         , NewSnobol4Machine m
         , Forceable a m
         ) 
      => Lazy (ExprType m) a
      -> InterpreterGeneric (ProgramType m) m (Maybe a)
force (EvaluatedThunk x) = return $ Just x
force (Thunk expr) = runMaybeT $ (MaybeT $ eval expr) >>= MaybeT . fromForce

force' :: ( InterpreterShell m
          , NewSnobol4Machine m
          , Forceable a m
          ) 
       => Lazy (ExprType m) a
       -> MaybeT (InterpreterGeneric (ProgramType m) m) (Lazy (ExprType m) a)
force' = liftM EvaluatedThunk . MaybeT . force

{-

forceInteger' :: ( InterpreterShell m
                 , NewSnobol4Machine m
                 )
              => LazyInteger (ExprType m)
              -> MaybeT (InterpreterGeneric (ProgramType m) m) (LazyInteger (ExprType m))
forceInteger' = liftM EvaluatedThunk . MaybeT . forceInteger

forceString' :: ( InterpreterShell m
                , NewSnobol4Machine m
                )
             => LazyString (ExprType m)
             -> MaybeT (InterpreterGeneric (ProgramType m) m) (LazyString (ExprType m))
forceString' = liftM EvaluatedThunk . MaybeT . forceString

forcePattern' :: ( InterpreterShell m
                 , NewSnobol4Machine m
                 )
              => LazyPattern (ExprType m)
              -> MaybeT (InterpreterGeneric (ProgramType m) m) (LazyPattern (ExprType m))
forcePattern' = liftM EvaluatedThunk . MaybeT . forcePattern
-}

{-
-- | Force a thunk to be entirely evaluated
deepForce :: ( InterpreterShell m
             , NewSnobol4Machine m
             ) 
          => Pattern (ExprType m)
          -> InterpreterGeneric (ProgramType m) m (Maybe (Pattern (ExprType m)))
deepForce (EvaluatedThunk x) = case x of
    (TempPatternData y) -> (liftM (liftM TempPatternData) $ forcePattern y) >>= 
    _ -> return $ Just x
deepForce x = runMaybeT $ (MaybeT $ tryForce x) >>= (MaybeT . deepForce . EvaluatedThunk)
-}

{-
-- | Force the evaluation of a pattern
forcePattern :: ( InterpreterShell m
                , NewSnobol4Machine m
                ) 
             => Pattern (ExprType m)
             -> InterpreterGeneric (ProgramType m) m (Maybe (Pattern (ExprType m)))
forcePattern (AssignmentPattern thunk l) =
    liftM (liftM $ \d -> AssignmentPattern (EvaluatedThunk d) l) $ deepForce thunk
forcePattern (ImmediateAssignmentPattern thunk l) =
    liftM (liftM $ \d -> ImmediateAssignmentPattern (EvaluatedThunk d) l) $ deepForce thunk
-}

{-
-- | Perform an operation after forcing a thunk
withDeepForce :: ( InterpreterShell m
                 , NewSnobol4Machine m
                 )
              => ((Data (ExprType m)) -> InterpreterGeneric (ProgramType m) m a)
              -> LazyData (ExprType m)
              -> InterpreterGeneric (ProgramType m) m (Maybe a)
withDeepForce f x = deepForce x >>= \case
    Just y -> liftM Just $ f y
    Nothing -> return Nothing
-}

{-
-- | Perform an operation after forcing a thunk, and raise an error if
-- evaluation fails
withDeepForceFail :: ( InterpreterShell m
                     , NewSnobol4Machine m
                     )
                  => ProgramError
                  -> ((Data (ExprType m)) -> InterpreterGeneric (ProgramType m) m a)
                  -> LazyData (ExprType m)
                  -> InterpreterGeneric (ProgramType m) m a
withDeepForceFail e f x = withDeepForce f x >>= \case
    Just x -> return x
    Nothing -> programError e
-}
