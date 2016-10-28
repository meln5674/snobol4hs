{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Convert
Description     : Converting between datatypes
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Snobol4.Interpreter.Internal.StateMachine.Convert where

import Prelude hiding (toInteger)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.StateMachine.Error
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Patterns
import Language.Snobol4.Interpreter.Internal.StateMachine.ObjectCode
import Language.Snobol4.Interpreter.Internal.StateMachine.Lazy
--import Language.Snobol4.Interpreter.Internal.StateMachine.Run

instance ( InterpreterShell m
         , NewSnobol4Machine m
         ) 
      => Forceable Snobol4Integer m where
    fromForce = toInteger

instance ( InterpreterShell m
         , NewSnobol4Machine m
         ) 
      => Forceable Snobol4String m where
    fromForce = liftM Just . toString

instance ( InterpreterShell m
         , NewSnobol4Machine m
         ) 
      => Forceable Snobol4Real m where
    fromForce = toReal

instance ( InterpreterShell m
         , NewSnobol4Machine m
         , expr ~ ExprType m
         ) 
      => Forceable (Pattern expr) m where
    fromForce d = runMaybeT $ (lift $ toPattern d) >>= \case
        AssignmentPattern pat l -> 
            AssignmentPattern <$> force' pat <*> pure l
        ImmediateAssignmentPattern pat l -> do
            ImmediateAssignmentPattern <$> force' pat <*> pure l
        -- LiteralPattern -> LiteralPattern
        AlternativePattern patA patB -> do
            AlternativePattern <$> force' patA <*> force' patB
        ConcatPattern patA patB -> ConcatPattern <$> force' patA <*> force' patB
        LengthPattern len -> LengthPattern <$> force' len
        -- EverythingPattern -> EverythingPattern
        --HeadPattern -> HeadPattern
        SpanPattern cs -> SpanPattern <$> force' cs
        BreakPattern cs -> BreakPattern <$> force' cs
        AnyPattern cs -> AnyPattern <$> force' cs
        NotAnyPattern cs -> NotAnyPattern <$> force' cs
        TabPattern ix -> TabPattern <$> force' ix
        RTabPattern ix -> RTabPattern <$> force' ix
        PosPattern ix -> PosPattern <$> force' ix
        RPosPattern ix -> RPosPattern <$> force' ix
        -- FailPattern -> FailPattern
        -- FencePattern -> FencePattern
        -- AbortPattern -> AbortPattern
        -- ArbPattern -> ArbPattern
        -- ArbNoPattern -> ArbNoPattern
        -- BalPattern -> BalPattern
        -- SuccessPattern -> SuccessPattern
        otherPattern -> return otherPattern
    
-- | Attempt to convert an array to a table
arrayToTable :: InterpreterShell m => (Snobol4Array (ExprType m)) -> InterpreterGeneric program m (Maybe (Snobol4Table (ExprType m)))
arrayToTable arr = undefined

-- | Attempt to convert a table to an array
tableToArray :: InterpreterShell m => (Snobol4Table (ExprType m)) -> InterpreterGeneric program m (Maybe (Snobol4Array (ExprType m)))
tableToArray tab = undefined

{-
-- | Check if a value can be turned into a string
isStringable :: InterpreterShell m => (Data (ExprType m)) -> InterpreterGeneric program m Bool
isStringable (StringData _) = return True
isStringable (IntegerData _) = return True
isStringable (RealData _) = return True
isStringable (PatternData k) = do
    result <- patternsLookup k
    case result of
        Nothing -> programError ErrorInSnobol4System
        Just pat -> do
            let pred (LiteralPattern _) = True
                pred (ConcatPattern a b) = pred a && pred b
                pred _ = False
            return $ pred pat
isStringable _ = return False
-}
  
-- | Take two arguments and cast the "lower" one on the scale of
-- String -> Int -> Real to match the "higher" one
raiseArgs :: ( InterpreterShell m
--             , NewSnobol4Machine m
--             , LocalVariablesClass m
             ) 
          => (Data (ExprType m)) 
          -> (Data (ExprType m)) 
          -> InterpreterGeneric (ProgramType m)
                              m (Maybe ((Data (ExprType m)), (Data (ExprType m))))
raiseArgs a b
    | isString a && isString b = return $ Just (a,b)
    | isInteger a && isInteger b = return $ Just (a,b)
    | isReal a && isReal b = return $ Just (a,b)
    
    | isString a && isInteger b = do
        a' <- toInteger a
        return $ pairLeft IntegerData a' b
    | isInteger a && isString b = do
        b' <- toInteger b
        return $ pairRight a IntegerData b'
    
    | isString a && isReal b = do
        a' <- toReal a
        return $ pairLeft RealData a' b
    | isReal a && isString b = do
        b' <- toReal b
        return $ pairRight a RealData b'
    
    | isInteger a && isReal b = do
        a' <- toReal a
        return $ pairLeft RealData a' b
    | isReal a && isInteger b = do
        b' <- toReal b
        return $ pairRight a RealData b'
            
    | otherwise = programError IllegalDataType

-- | Combine items into a tuple inside the maybe functor
pairLeft :: (a -> a') -> Maybe a -> b -> Maybe (a',b)
pairLeft _ Nothing _ = Nothing
pairLeft f (Just a) b = Just (f a,b)

-- | Combine items into a tuple inside the maybe functor
pairRight :: a -> (b -> b') -> Maybe b -> Maybe (a,b')
pairRight _ _ Nothing = Nothing
pairRight a f (Just b) = Just (a,f b)


-- | Take two arguments and cast the "higher" one on the scale of
-- String -> Int -> Real to match the "lower" one
lowerArgs :: ( InterpreterShell m 
             , NewSnobol4Machine m
             , LocalVariablesClass m
             )
          => (Data (ExprType m)) 
          -> (Data (ExprType m)) 
          -> InterpreterGeneric (ProgramType m)
                              m (Maybe ((Data (ExprType m)), (Data (ExprType m))))
lowerArgs a b
    | isString a && isString b = return $ Just (a,b)
    | isInteger a && isInteger b = return $ Just (a,b)
    | isReal a && isReal b = return $ Just (a,b)
    
    | isString a && isInteger b = do
        b' <- toString b
        return $ pairRight a StringData (Just b')
    | isInteger a && isString b = do
        a' <- toString a
        return $ pairLeft StringData (Just a') b
    
    | isString a && isReal b = do
        b' <- toString b
        return $ pairRight a StringData (Just b')
    | isReal a && isString b = do
        a' <- toString a
        return $ pairLeft StringData (Just a') b
    
    | isInteger a && isReal b = do
        b' <- toInteger b
        return $ pairRight a IntegerData b'
    | isReal a && isInteger b = do
        a' <- toInteger a
        return $ pairLeft IntegerData a' b
    
    | otherwise = programError IllegalDataType



-- | Convert data to a string
-- Throws IllegalDataType if not a valid conversion
-- Returns nothing if a thunk evaluation failed
toString :: ( InterpreterShell m 
            )
          => (Data (ExprType m)) 
          -> InterpreterGeneric (ProgramType m)
                              m Snobol4String
toString (StringData s) = return s
toString (IntegerData i) = return $ mkString i
toString (RealData r) = return $ mkString r
toString (PatternData k) = return $ datatypeNamePattern
toString (TempPatternData p) = return $ datatypeNamePattern
toString _ = programError IllegalDataType

toLazyString :: ( InterpreterShell m
                ) 
             => Data (ExprType m)
             -> InterpreterGeneric (ProgramType m) m (LazyString (ExprType m))
toLazyString (ExprData expr) = return $ Thunk expr
toLazyString x = liftM EvaluatedThunk $ toString x

-- | Convert data to a pattern
-- Throws a ProgramError if this is not valid
toPattern :: ( InterpreterShell m 
             )
          => Data (ExprType m)
          -> InterpreterGeneric (ProgramType m)
                              m (Pattern (ExprType m))
toPattern (PatternData k) = do
    result <- patternsLookup k
    case result of
        Nothing -> programError ErrorInSnobol4System
        Just pat -> return pat
toPattern (TempPatternData p) = return p
toPattern x = liftM LiteralPattern $ toString x

toLazyPattern :: ( InterpreterShell m 
                 )
              => Data (ExprType m)
              -> InterpreterGeneric (ProgramType m)
                                  m (LazyPattern (ExprType m))
toLazyPattern (ExprData expr) = return $ Thunk expr
toLazyPattern x = liftM EvaluatedThunk $ toPattern x

-- | Convert data to object code
-- Throws a ProgramError if this is not valid
toCode :: ( InterpreterShell m 
          {-, Snobol4Machine program-}
          )
       => (Data (ExprType m)) 
       -> InterpreterGeneric program 
                           m Snobol4Code
toCode (CodeData k) = do
    result <- codesLookup k
    case result of
        Nothing -> programError ErrorInSnobol4System
        Just code -> return code
toCode _ = programError IllegalDataType

-- | Convert data to an integer
-- Fails the evaluation if this can be turned into a string, but not into an 
-- integer
-- Throws a ProgramError if this is not valid
toInteger :: ( InterpreterShell m 
             )
          => (Data (ExprType m)) 
          -> InterpreterGeneric (ProgramType m)
                                      m (Maybe Snobol4Integer)
toInteger (IntegerData i) = return $ Just i
toInteger x = runMaybeT $ do
    s <- lift $ toString x
    if s == nullString
        then return 0
        else MaybeT $ return $ snobol4Read s

toLazyInteger :: ( InterpreterShell m 
             )
          => (Data (ExprType m)) 
          -> InterpreterGeneric (ProgramType m)
                                      m (Maybe (LazyInteger (ExprType m)))
toLazyInteger (ExprData expr) = return $ Just $ Thunk expr
toLazyInteger x = runMaybeT $ liftM EvaluatedThunk $ MaybeT $ toInteger x

-- | Convert data to a real
-- Fails the evaluation if this can be turned into a string, but not into an 
-- real
-- Throws a ProgramError if this is not valid
toReal :: ( InterpreterShell m 
          ) 
       => Data (ExprType m) 
       -> InterpreterGeneric (ProgramType m)
                           m (Maybe Snobol4Real)
toReal (RealData r) = return $ Just r
toReal x = runMaybeT $ do
    s <- lift $ toString x
    if s == nullString
        then return 0
        else MaybeT $ return $ snobol4Read s 
