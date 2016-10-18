{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Convert
Description     : Converting between datatypes
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.Convert where

import Prelude hiding (toInteger)

import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.StateMachine.Error
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Patterns
import Language.Snobol4.Interpreter.Internal.StateMachine.ObjectCode
--import Language.Snobol4.Interpreter.Internal.StateMachine.Run

-- | Attempt to convert an array to a table
arrayToTable :: InterpreterShell m => (Snobol4Array (ExprType m)) -> InterpreterGeneric program m (Maybe (Snobol4Table (ExprType m)))
arrayToTable arr = undefined

-- | Attempt to convert a table to an array
tableToArray :: InterpreterShell m => (Snobol4Table (ExprType m)) -> InterpreterGeneric program m (Maybe (Snobol4Array (ExprType m)))
tableToArray tab = undefined

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
    
-- | Take two arguments and cast the "lower" one on the scale of
-- String -> Int -> Real to match the "higher" one
raiseArgs :: ( InterpreterShell m
--             {-, Snobol4Machine program-}
             , LocalVariablesClass m
             ) 
          => (Data (ExprType m)) 
          -> (Data (ExprType m)) 
          -> InterpreterGeneric program 
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


pairLeft :: (a -> a') -> Maybe a -> b -> Maybe (a',b)
pairLeft _ Nothing _ = Nothing
pairLeft f (Just a) b = Just (f a,b)
pairRight :: a -> (b -> b') -> Maybe b -> Maybe (a,b')
pairRight _ _ Nothing = Nothing
pairRight a f (Just b) = Just (a,f b)


-- | Take two arguments and cast the "higher" one on the scale of
-- String -> Int -> Real to match the "lower" one
lowerArgs :: ( InterpreterShell m 
             {-, Snobol4Machine program-}
             , LocalVariablesClass m
             )
          => (Data (ExprType m)) 
          -> (Data (ExprType m)) 
          -> InterpreterGeneric program 
                              m (Maybe ((Data (ExprType m)), (Data (ExprType m))))
lowerArgs a b
    | isString a && isString b = return $ Just (a,b)
    | isInteger a && isInteger b = return $ Just (a,b)
    | isReal a && isReal b = return $ Just (a,b)
    
    | isString a && isInteger b = do
        b' <- toString b
        return $ Just (a, StringData b')
    | isInteger a && isString b = do
        a' <- toString a
        return $ Just (StringData a', b)
    
    | isString a && isReal b = do
        b' <- toString b
        return $ Just (a, StringData b')
    | isReal a && isString b = do
        a' <- toString a
        return $ Just (StringData a', b)
    
    | isInteger a && isReal b = do
        b' <- toInteger b
        return $ pairRight a IntegerData b'
    | isReal a && isInteger b = do
        a' <- toInteger a
        return $ pairLeft IntegerData a' b
    
    | otherwise = programError IllegalDataType



-- | Convert data to a string
-- Throws a ProgramError if this is not valid
toString :: ( InterpreterShell m 
--            {-, Snobol4Machine program-}
            )
          => (Data (ExprType m)) 
          -> InterpreterGeneric program 
                              m Snobol4String
toString (StringData s) = return $ s
toString (IntegerData i) = return $ mkString i
toString (RealData r) = return $ mkString r
toString (PatternData k) = do
    result <- patternsLookup k
    case result of
        Nothing -> programError ErrorInSnobol4System
        Just pat -> toString (TempPatternData pat)
toString (TempPatternData pat) = do
    let conv (LiteralPattern s) = return $ s
        conv (ConcatPattern a b) = (<>) <$> conv a <*> conv b
        conv _ = programError IllegalDataType
    conv pat
toString _ = programError IllegalDataType

-- | Convert data to a pattern
-- Throws a ProgramError if this is not valid
toPattern :: ( InterpreterShell m 
--             {-, Snobol4Machine program-}
             )
          => (Data (ExprType m)) 
          -> InterpreterGeneric program 
                              m (Pattern (ExprType m))
toPattern (PatternData k) = do
    result <- patternsLookup k
    case result of
        Nothing -> programError ErrorInSnobol4System
        Just pat -> return pat
toPattern (TempPatternData p) = return p
toPattern x = LiteralPattern <$> toString x

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
--             {-, Snobol4Machine program-}
             , LocalVariablesClass m
             )
          => (Data (ExprType m)) -> InterpreterGeneric program 
                                      m (Maybe Snobol4Integer)
toInteger (IntegerData i) = return $ Just i
toInteger x = do
    s <- toString x
    if s == nullString
        then return $ Just 0
        else return $ snobol4Read s

-- | Convert data to a real
-- Fails the evaluation if this can be turned into a string, but not into an 
-- real
-- Throws a ProgramError if this is not valid
toReal :: ( InterpreterShell m 
--          {-, Snobol4Machine program-}
          , LocalVariablesClass m
          )
       => Data (ExprType m)
       -> InterpreterGeneric program 
                           m (Maybe Snobol4Real)
toReal (RealData r) = return $ Just r
toReal x = do
    s <- toString x
    if s == nullString
        then return $ Just 0
        else return $ snobol4Read s 
