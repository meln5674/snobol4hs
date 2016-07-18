module Language.Snobol4.Interpreter.Primitives where

import Prelude hiding (len, span, break, any, notany, toInteger)

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Internal.Types

primitiveVars :: [(String, Data)]
primitiveVars =
    [ ("NULL",  StringData "")
    , ("REM",   PatternData $ RTabPattern 0)
    , ("FAIL",  PatternData FailPattern)
    , ("FENCE", PatternData FencePattern)
    , ("ABORT", PatternData AbortPattern)
    , ("ARB", PatternData ArbPattern)
    ]
    
primitiveFunctions :: InterpreterShell m => [Function m]
primitiveFunctions =
    [ PrimitiveFunction "LEN"       len
    , PrimitiveFunction "SPAN"      span
    , PrimitiveFunction "BREAK"     break
    , PrimitiveFunction "ANY"       any
    , PrimitiveFunction "NOTANY"    notany
    , PrimitiveFunction "TAB"       rtab
    , PrimitiveFunction "RTAB"      rtab
    , PrimitiveFunction "ARBNO"     arbno
    ]

len :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
len (a:_) = do
    IntegerData i <- toInteger a
    if i >= 0
        then return $ Just $ PatternData $ LengthPattern i
        else liftEval $ programError ProgramError
len [] = len [StringData ""]

span :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
span (a:_) = do
    StringData s <- toString a
    case s of
        "" -> liftEval $ programError ProgramError
        _ -> return $ Just $ PatternData $ SpanPattern s
span [] = span [StringData ""]

break :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
break (a:_) = do
    StringData s <- toString a
    case s of
        "" -> liftEval $ programError ProgramError 
        _ -> return $ Just $ PatternData $ BreakPattern s
break [] = break [StringData ""]

any :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
any (a:_) = do
    StringData cs <- toString a
    case cs of
        "" -> liftEval $ programError ProgramError 
        _ -> return $ Just $ PatternData $ AnyPattern cs
any [] = any [StringData ""]

notany :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
notany (a:_) = do
    StringData cs <- toString a
    case cs of
        "" -> liftEval $ programError ProgramError 
        _ -> return $ Just $ PatternData $ AnyPattern cs
notany [] = notany [StringData ""]

tab :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
tab (a:_) = do
    IntegerData i <- toInteger a
    if i >= 0
        then return $ Just $ PatternData $ TabPattern i
        else liftEval $ programError ProgramError
tab [] = tab [StringData ""]

rtab :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
rtab (a:_) = do
    IntegerData i <- toInteger a
    if i >= 0
        then return $ Just $ PatternData $ RTabPattern i
        else liftEval $ programError ProgramError
rtab [] = rtab [StringData ""]


arbno :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
arbno (a:_) = do
    p <- toPattern a
    return $ Just $ PatternData $ ArbNoPattern p
arbno [] = arbno [StringData ""]
