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
import Language.Snobol4.Interpreter.Internal.StateMachine.Run

-- | Attempt to convert an array to a table
arrayToTable :: InterpreterShell m => Snobol4Array -> InterpreterGeneric program m (Maybe Snobol4Table)
arrayToTable arr = undefined

-- | Attempt to convert a table to an array
tableToArray :: InterpreterShell m => Snobol4Table -> InterpreterGeneric program m (Maybe Snobol4Array)
tableToArray tab = undefined

-- | Check if a value can be turned into a string
isStringable :: InterpreterShell m => Data -> InterpreterGeneric program m Bool
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
raiseArgs :: (InterpreterShell m, Snobol4Machine program) 
          => Data 
          -> Data 
          -> EvaluatorGeneric program 
                              (EvaluationError program) 
                              m (Data, Data)
raiseArgs a b
    | isString a && isString b = return (a,b)
    | isInteger a && isInteger b = return (a,b)
    | isReal a && isReal b = return (a,b)
    
    | isString a && isInteger b = do
        a' <- toInteger a
        return (IntegerData a',b)
    | isInteger a && isString b = do
        b' <- toInteger b
        return (a,IntegerData b')
    
    | isString a && isReal b = do
        a' <- toReal a
        return (RealData a',b)
    | isReal a && isString b = do
        b' <- toReal b
        return (a,RealData b')
    
    | isInteger a && isReal b = do
        a' <- toReal a
        return (RealData a',b)
    | isReal a && isInteger b = do
        b' <- toReal b
        return (a,RealData b')
    
    | otherwise = liftEval $ programError IllegalDataType

-- | Take two arguments and cast the "higher" one on the scale of
-- String -> Int -> Real to match the "lower" one
lowerArgs :: ( InterpreterShell m 
             , Snobol4Machine program
             )
          => Data 
          -> Data 
          -> EvaluatorGeneric program 
                              (EvaluationError program) 
                              m (Data, Data)
lowerArgs a b
    | isString a && isString b = return (a,b)
    | isInteger a && isInteger b = return (a,b)
    | isReal a && isReal b = return (a,b)
    
    | isString a && isInteger b = do
        b' <- toString b
        return (a,StringData b')
    | isInteger a && isString b = do
        a' <- toString a
        return (StringData a',b)
    
    | isString a && isReal b = do
        b' <- toString b
        return (a,StringData b')
    | isReal a && isString b = do
        a' <- toString a
        return (StringData a',b)
    
    | isInteger a && isReal b = do
        b' <- toInteger b
        return (a,IntegerData b')
    | isReal a && isInteger b = do
        a' <- toInteger a
        return (IntegerData a',b)
    
    | otherwise = liftEval $ programError IllegalDataType



-- | Convert data to a string
-- Throws a ProgramError if this is not valid
toString :: ( InterpreterShell m 
            , Snobol4Machine program
            )
          => Data 
          -> EvaluatorGeneric program 
                              (EvaluationError program)
                              m Snobol4String
toString (StringData s) = return s
toString (IntegerData i) = return $ mkString i
toString (RealData r) = return $ mkString r
toString (PatternData k) = liftEval $ do
    result <- patternsLookup k
    case result of
        Nothing -> programError ErrorInSnobol4System
        Just pat -> do
            let conv (LiteralPattern s) = return $ s
                conv (ConcatPattern a b) = (<>) <$> conv a <*> conv b
                conv _ = programError IllegalDataType
            conv pat
toString _ = liftEval $ programError IllegalDataType

-- | Convert data to a pattern
-- Throws a ProgramError if this is not valid
toPattern :: ( InterpreterShell m 
             , Snobol4Machine program
             )
          => Data 
          -> EvaluatorGeneric program 
                              (EvaluationError program)
                              m Pattern
toPattern (PatternData k) = liftEval $ do
    result <- patternsLookup k
    case result of
        Nothing -> programError ErrorInSnobol4System
        Just pat -> return pat
toPattern (TempPatternData p) = return p
toPattern x = LiteralPattern <$> toString x

-- | Convert data to object code
-- Throws a ProgramError if this is not valid
toCode :: ( InterpreterShell m 
          , Snobol4Machine program
          )
       => Data 
       -> EvaluatorGeneric program 
                           (EvaluationError program) 
                           m Snobol4Code
toCode (CodeData k) = liftEval $ do
    result <- codesLookup k
    case result of
        Nothing -> programError ErrorInSnobol4System
        Just code -> return code
toCode _ = liftEval $ programError IllegalDataType

-- | Convert data to an integer
-- Fails the evaluation if this can be turned into a string, but not into an 
-- integer
-- Throws a ProgramError if this is not valid
toInteger :: ( InterpreterShell m 
             , Snobol4Machine program
             )
          => Data -> EvaluatorGeneric program 
                                      (EvaluationError program)
                                      m Snobol4Integer
toInteger (IntegerData i) = return i
toInteger x = do
    s <- toString x
    if s == nullString
        then return 0
        else case snobol4Read s of
            Just i -> return i
            Nothing -> failEval

-- | Convert data to a real
-- Fails the evaluation if this can be turned into a string, but not into an 
-- real
-- Throws a ProgramError if this is not valid
toReal :: ( InterpreterShell m 
          , Snobol4Machine program
          )
       => Data 
       -> EvaluatorGeneric program 
                           (EvaluationError program)
                           m Snobol4Real
toReal (RealData r) = return r
toReal x = do
    s <- toString x
    case snobol4Read s of
        Just r -> return r
        Nothing -> failEval
