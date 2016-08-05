{-|
Module          : Language.Snobol4.Interpreter.Primitives
Description     : Primitive functions and variables
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

Primitive functions and variables are provided by the interpreter without the
user needing to define them, and offer values and operations that cannot be
expressed in the source language.
-}

{-# LANGUAGE OverloadedStrings #-}
module Language.Snobol4.Interpreter.Primitives where

import Prelude hiding ( span, break, any, toInteger )

import Data.List (genericReplicate)

import Control.Monad

import Language.Snobol4.Interpreter.Shell (InterpreterShell)
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Internal
import Language.Snobol4.Interpreter.Internal.Types
import Language.Snobol4.Interpreter.Evaluator
import Language.Snobol4.Interpreter.Primitives.Prototypes

import Language.Snobol4.Parser

-- | The names and initial values of the primitive variables
primitiveVars :: [(Snobol4String, Data)]
primitiveVars =
    [ ("NULL",  StringData nullString)
    , ("REM",   TempPatternData $ RTabPattern 0)
    , ("FAIL",  TempPatternData FailPattern)
    , ("FENCE", TempPatternData FencePattern)
    , ("ABORT", TempPatternData AbortPattern)
    , ("ARB", TempPatternData ArbPattern)
    , ("BAL", TempPatternData BalPattern)
    , ("SUCCEED", TempPatternData SucceedPattern)
    ]


-- | The names and actions of the primitive functions
primitiveFunctions :: InterpreterShell m => [Function m]
primitiveFunctions =
    [ PrimitiveFunction "ANY"       any
    , PrimitiveFunction "APPLY"     apply
    , PrimitiveFunction "ARBNO"     arbno
    , PrimitiveFunction "ARG"       arg
    , PrimitiveFunction "ARRAY"     array
    , PrimitiveFunction "BACKSPACE" backspace
    , PrimitiveFunction "BREAK"     break
    , PrimitiveFunction "CLEAR"     clear
    , PrimitiveFunction "CODE"      code
    , PrimitiveFunction "COLLECT"   collect
    , PrimitiveFunction "CONVERT"   convert
    , PrimitiveFunction "COPY"      copy
    , PrimitiveFunction "DATA"      data_
    , PrimitiveFunction "DATATYPE"  datatype
    , PrimitiveFunction "DEFINE"    define
    , PrimitiveFunction "DETACH"    detach
    , PrimitiveFunction "DIFFER"    differ
    , PrimitiveFunction "DUMP"      dump
    , PrimitiveFunction "DUPL"      dupl
    , PrimitiveFunction "ENDFILE"   endfile
    , PrimitiveFunction "EQ"        eq
    , PrimitiveFunction "EVAL"      eval
    , PrimitiveFunction "FIELD"     field
    , PrimitiveFunction "GE"        ge
    , PrimitiveFunction "GT"        gt
    , PrimitiveFunction "IDENT"     ident
    , PrimitiveFunction "INPUT"     input
    , PrimitiveFunction "INTEGER"   integer
    , PrimitiveFunction "ITEM"      item
    , PrimitiveFunction "LE"        le
    , PrimitiveFunction "LEN"       len
    , PrimitiveFunction "LGT"       lgt
    , PrimitiveFunction "LOCAL"     local
    , PrimitiveFunction "LT"        lt
    , PrimitiveFunction "NE"        ne
    , PrimitiveFunction "NOTANY"    notany
    , PrimitiveFunction "UNLOAD"    unload
    , PrimitiveFunction "OPSYN"     opsyn
    , PrimitiveFunction "OUTPUT"    output
    , PrimitiveFunction "POS"       pos
    , PrimitiveFunction "PROTOTYPE" prototype
    , PrimitiveFunction "REMDR"     remdr
    , PrimitiveFunction "REPLACE"   replace
    , PrimitiveFunction "REWIND"    rewind
    , PrimitiveFunction "RPOS"      rpos
    , PrimitiveFunction "RTAB"      rtab
    , PrimitiveFunction "SIZE"      size
    , PrimitiveFunction "SPAN"      span
    , PrimitiveFunction "STOPTR"    stoptr
    , PrimitiveFunction "TAB"       rtab
    , PrimitiveFunction "TABLE"     table
    , PrimitiveFunction "TIME"      time
    , PrimitiveFunction "TRACE"     trace
    , PrimitiveFunction "TRIM"      trim
    , PrimitiveFunction "VALUE"     value
    ]

-- | Generalization for lt, le, eq, ne, ge, and gt
numericalPredicate :: (Num a, InterpreterShell m) 
                  => (a -> a -> Bool)
                  -> [Data] 
                  -> Evaluator m (Maybe Data)
numericalPreciate pred [a,b] = do
    (a',b') <- raiseArgs a b
    (a'',b'') <- case a' of
        StringData a -> (,) <$> (IntegerData <$> toInteger a') <*> (IntegerData <$> toInteger b')
        _ -> return (a',b')
    return $ if a'' < b''
        then Just $ StringData ""
        else Nothing
numericalPredicate pred [a] = numericalPredicate pred [a,IntegerData 0]
numericalPredicate _ _ = liftEval $ programError IncorrectNumberOfArguments

-- | The any function, returns a pattern which matches any one of the provided
-- characters
any :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
any (a:_) = do
    cs <- toString a
    liftEval $ case cs of
        "" -> programError NullStringInIllegalContext
        _ -> return $ Just $ TempPatternData $ AnyPattern cs
any [] = any [StringData ""]


-- | The apply function, calls a function by name with arguments
apply :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
apply (nameArg:args) = do
    name <- toString nameArg
    liftEval $ call name args
apply _ = liftEval $ programError IncorrectNumberOfArguments

-- | The array function, creates an array with the provided dimensions and initial value
array :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
array [dimStr,val] = do
    str <- toString dimStr
    parseResult <- parseT $ unmkString str
    case parseResult of
        Left _ -> return Nothing
        Right (ArrayPrototype dims) -> Just <$> liftEval (arraysNew'' dims val)
array [dimStr] = array [dimStr, StringData ""]
array [] = liftEval $ programError NullStringInIllegalContext
array _ = liftEval $ programError IncorrectNumberOfArguments

-- | The arbno function, returns a pattern which matches an arbitrary number of
-- repetitions of the provided pattern
arbno :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
arbno (a:_) = do
    p <- toPattern a
    return $ Just $ TempPatternData $ ArbNoPattern p
arbno [] = arbno [StringData ""]

-- | The arg function, returns the name of the nth argument of a function
arg :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
arg [fname,n] = do
    fname' <- toString fname
    n' <- toInteger n
    funcResult <- liftEval $ funcLookup fname'
    case funcResult of
        Nothing -> liftEval $ programError UndefinedFunctionOrOperation
        Just func -> case drop (unmkInteger n') $ formalArgs func of
            [] -> liftEval $ programError IncorrectNumberOfArguments
            (arg:_) -> return $ Just $ StringData arg
arg _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO
backspace :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
backspace = const $ liftEval $ programError ErrorInSnobol4System

-- | The break function, returns a pattern which matches the longest string
-- containing none of the provided characters
break :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
break (a:_) = do
    s <- toString a
    liftEval $ case s of
        "" -> programError NullStringInIllegalContext
        _ ->  return $ Just $ TempPatternData $ BreakPattern s
break [] = break [StringData ""]

-- | TODO
clear :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
clear [] = liftEval $ do
    ns <- naturalVarNames
    forM ns clearVar
    return $ Just $ StringData nullString
clear _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO
code :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
code [src] = do
    src' <- toString src
    result <- parseT $ unmkString src'
    case result of
        Left _ -> return Nothing
        Right stmts -> do
            newKey <- liftEval $ codesNew $ Snobol4Code stmts
            return $ Just $ CodeData newKey
code _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO
collect :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
collect = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO
convert :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
convert = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO
copy :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
copy = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO
data_ :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
data_ = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO
datatype :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
datatype = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO
date :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
date = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO
define :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
define = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO
detach :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
detach = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO
differ :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
differ [a,b] = do
    if a /= b
        then return $ Just $ StringData ""
        else return Nothing
differ _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO
dump :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
dump = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO
dupl :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
dupl [a,b] = do
    a' <- toString a
    b' <- toInteger b
    return $ Just $ StringData $ foldl (<>) "" $ genericReplicate b' a'
dupl _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO
endfile :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
endfile = const $ liftEval $ programError ErrorInSnobol4System

-- | Equality predicate
eq :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
eq = numericalPredicate (==)

-- | TODO
eval :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
eval [a] = do
    expr <- case a of
        (PatternData _) -> do
            pat <- toPattern a
            case pat of
                UnevaluatedExprPattern expr -> return expr
                _ -> liftEval $ programError IllegalArgumentToPrimitiveFunction
        _ -> do
            a' <- toString a
            result <- parseT (unmkString a')
            case result of
                Left err -> liftEval $ programError IllegalArgumentToPrimitiveFunction
                Right expr -> return expr
    Just <$> evalExpr expr
eval _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO
field :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
field = const $ liftEval $ programError ErrorInSnobol4System

-- | Greater than or equal predicate
ge :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
ge = numericalPredicate (>=)

-- | Greater than predicate
gt :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
gt = numericalPredicate (>)

-- | TODO
ident :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
ident [a,b] = do
    if a == b
        then return $ Just $ StringData ""
        else return Nothing
ident _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO
input :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
input = const $ liftEval $ programError ErrorInSnobol4System

-- | Integer predicate
integer :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
integer [a] = do
    result <- toInteger a
    return $ Just $ StringData ""
integer [] = return $ Just $ StringData ""

-- | TODO
item :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
item (name:keys) = do
    name' <- toString name
    liftEval $ execLookup $ LookupAggregate name' keys
item _ = liftEval $ programError IncorrectNumberOfArguments

-- | Less than or equal predicate
le :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
le = numericalPredicate (<=)

-- | The length function, returns a pattern with matches the provided number
-- of characters
len :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
len (a:_) = do
    i <- toInteger a
    liftEval $ if i >= 0
        then return $ Just $ TempPatternData $ LengthPattern i
        else programError NegativeNumberInIllegalContext
len [] = len [StringData ""]

-- | TODO
lgt :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
lgt [a,b] = do
    a' <- toString a
    b' <- toString b
    if a' > b'
        then return $ Just $ StringData ""
        else return Nothing
lgt _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO
local :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
local = const $ liftEval $ programError ErrorInSnobol4System

-- | Less than predicate
lt :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
lt = numericalPredicate (<)

-- | Inequality predicate
ne :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
ne = numericalPredicate (/=)

-- | The notany function, returns a pattern which matches one character not
-- provided
notany :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
notany (a:_) = do
    cs <- toString a
    liftEval $ case cs of
        "" -> programError NullStringInIllegalContext
        _ -> return $ Just $ TempPatternData $ NotAnyPattern cs
notany [] = notany [StringData ""]

-- | TODO
opsyn :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
opsyn = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO 
output :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
output = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO 
pos :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
pos [a] = do
    result <- toInteger a
    return $ Just $ TempPatternData $ PosPattern result
pos _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO 
prototype :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
prototype = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO 
remdr :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
remdr [a,b] = do
    a' <- toInteger a
    b' <- toInteger b
    return $ Just $ IntegerData $ b' `rem` a'
remdr _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO 
replace :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
replace [x,y,z] = do
    x' <- toString x
    y' <- toString y
    z' <- toString z
    if snobol4Length y' /= snobol4Length z'
        then liftEval $ programError IllegalArgumentToPrimitiveFunction
        else return $ Just $ StringData $ snobol4Replace x' y' z'
replace _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO 
rewind :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
rewind = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO 
rpos :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
rpos [a] = do
    result <- toInteger a
    return $ Just $ TempPatternData $ RPosPattern result
rpos _ = liftEval $ programError IncorrectNumberOfArguments

-- | The rtab function, returns a pattern which matches the null string if the
-- cursor is after the provided column, measured from the right
rtab :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
rtab (a:_) = do
    i <- toInteger a
    liftEval $ if i >= 0
        then return $ Just $ TempPatternData $ RTabPattern i
        else programError NegativeNumberInIllegalContext
rtab [] = rtab [StringData ""]

-- | TODO 
size :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
size [arg] = Just . IntegerData . snobol4Length <$> toString arg
size _ = liftEval $ programError IncorrectNumberOfArguments

-- | The span function, returns a pattern which matches the longest string
-- containing only the provided characters
span :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
span (a:_) = do
    s <- toString a
    liftEval $ case s of
        "" -> programError NullStringInIllegalContext
        _ -> return $ Just $ TempPatternData $ SpanPattern s
span [] = span [StringData ""]

-- | TODO 
stoptr :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
stoptr = const $ liftEval $ programError ErrorInSnobol4System

-- | The tab function, returns a pattern which matches the null string if the
-- cursor is before the provided column, measured from the left
tab :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
tab (a:_) = do
    i <- toInteger a
    liftEval $ if i >= 0
        then return $ Just $ TempPatternData $ TabPattern i
        else programError NegativeNumberInIllegalContext
tab [] = tab [StringData ""]

-- | The table function, returns an empty table
table :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
table (a:_) = do
    i <- toInteger a
    if i >= 0
        then Just . TableData <$> liftEval tablesNew
        else liftEval $ programError NegativeNumberInIllegalContext
table _ = Just . TableData <$> liftEval tablesNew

-- | TODO 
time :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
time = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO 
trace :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
trace = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO 
trim :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
trim [a] = do
    a' <- toString a
    return $ Just $ StringData $ snobol4Trim a' 
trim _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO
unload :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
unload = const $ liftEval $ programError ErrorInSnobol4System

-- | TODO 
value :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
value = const $ liftEval $ programError ErrorInSnobol4System

