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

module Language.Snobol4.Interpreter.Primitives where

import Prelude hiding ( span, break, any, toInteger )

import Text.Read (readMaybe)

import qualified Data.Map as M

import qualified Data.Array as A

import Language.Snobol4.Interpreter.Shell (InterpreterShell)
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Internal.Types
import Language.Snobol4.Interpreter.Primitives.Prototypes

import Language.Snobol4.Parser

-- | The names and initial values of the primitive variables
primitiveVars :: [(String, Data)]
primitiveVars =
    [ ("NULL",  StringData "")
    , ("REM",   PatternData $ RTabPattern 0)
    , ("FAIL",  PatternData FailPattern)
    , ("FENCE", PatternData FencePattern)
    , ("ABORT", PatternData AbortPattern)
    , ("ARB", PatternData ArbPattern)
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



-- | The any function, returns a pattern which matches any one of the provided
-- characters
any :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
any (a:_) = do
    cs <- toString a
    case cs of
        "" -> liftEval $ programError NullStringInIllegalContext
        _ -> return $ Just $ PatternData $ AnyPattern cs
any [] = any [StringData ""]


-- | The array function, creates an array with the provided dimensions and initial value
array :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
array [dimStr,item] = do
    str <- toString dimStr
    parseResult <- parseT str
    case parseResult of
        Left _ -> return Nothing
        Right (ArrayPrototype dims) -> Just <$> (liftEval $ arraysNew'' dims item)
array [dimStr] = array [dimStr, StringData ""]
array [] = liftEval $ programError NullStringInIllegalContext
array _ = liftEval $ programError IncorrectNumberOfArguments

apply :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
apply = const $ liftEval $ programError ErrorInSnobol4System

-- | The arbno function, returns a pattern which matches an arbitrary number of
-- repetitions of the provided pattern
arbno :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
arbno (a:_) = do
    p <- toPattern a
    return $ Just $ PatternData $ ArbNoPattern p
arbno [] = arbno [StringData ""]

arg :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
arg = const $ liftEval $ programError ErrorInSnobol4System

backspace :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
backspace = const $ liftEval $ programError ErrorInSnobol4System

-- | The break function, returns a pattern which matches the longest string
-- containing none of the provided characters
break :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
break (a:_) = do
    s <- toString a
    case s of
        "" -> liftEval $ programError NullStringInIllegalContext
        _ -> return $ Just $ PatternData $ BreakPattern s
break [] = break [StringData ""]

clear :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
clear = const $ liftEval $ programError ErrorInSnobol4System

code :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
code = const $ liftEval $ programError ErrorInSnobol4System

collect :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
collect = const $ liftEval $ programError ErrorInSnobol4System

convert :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
convert = const $ liftEval $ programError ErrorInSnobol4System

copy :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
copy = const $ liftEval $ programError ErrorInSnobol4System

data_ :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
data_ = const $ liftEval $ programError ErrorInSnobol4System

datatype :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
datatype = const $ liftEval $ programError ErrorInSnobol4System

date :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
date = const $ liftEval $ programError ErrorInSnobol4System

define :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
define = const $ liftEval $ programError ErrorInSnobol4System

detach :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
detach = const $ liftEval $ programError ErrorInSnobol4System

differ :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
differ = const $ liftEval $ programError ErrorInSnobol4System

dump :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
dump = const $ liftEval $ programError ErrorInSnobol4System

dupl :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
dupl = const $ liftEval $ programError ErrorInSnobol4System

endfile :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
endfile = const $ liftEval $ programError ErrorInSnobol4System

eq :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
eq = const $ liftEval $ programError ErrorInSnobol4System

eval :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
eval = const $ liftEval $ programError ErrorInSnobol4System

field :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
field = const $ liftEval $ programError ErrorInSnobol4System

ge :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
ge = const $ liftEval $ programError ErrorInSnobol4System

gt :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
gt = const $ liftEval $ programError ErrorInSnobol4System

ident :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
ident = const $ liftEval $ programError ErrorInSnobol4System

input :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
input = const $ liftEval $ programError ErrorInSnobol4System

integer :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
integer = const $ liftEval $ programError ErrorInSnobol4System

item :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
item = const $ liftEval $ programError ErrorInSnobol4System

le :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
le = const $ liftEval $ programError ErrorInSnobol4System

-- | The length function, returns a pattern with matches the provided number
-- of characters
len :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
len (a:_) = do
    i <- toInteger a
    if i >= 0
        then return $ Just $ PatternData $ LengthPattern i
        else liftEval $ programError NegativeNumberInIllegalContext
len [] = len [StringData ""]

lgt :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
lgt = const $ liftEval $ programError ErrorInSnobol4System

local :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
local = const $ liftEval $ programError ErrorInSnobol4System

lt :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
lt = const $ liftEval $ programError ErrorInSnobol4System

ne :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
ne = const $ liftEval $ programError ErrorInSnobol4System

-- | The notany function, returns a pattern which matches one character not
-- provided
notany :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
notany (a:_) = do
    cs <- toString a
    case cs of
        "" -> liftEval $ programError NullStringInIllegalContext
        _ -> return $ Just $ PatternData $ AnyPattern cs
notany [] = notany [StringData ""]

opsyn :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
opsyn = const $ liftEval $ programError ErrorInSnobol4System

output :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
output = const $ liftEval $ programError ErrorInSnobol4System

pos :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
pos = const $ liftEval $ programError ErrorInSnobol4System

prototype :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
prototype = const $ liftEval $ programError ErrorInSnobol4System

remdr :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
remdr = const $ liftEval $ programError ErrorInSnobol4System

replace :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
replace = const $ liftEval $ programError ErrorInSnobol4System

rewind :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
rewind = const $ liftEval $ programError ErrorInSnobol4System

rpos :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
rpos = const $ liftEval $ programError ErrorInSnobol4System

-- | The rtab function, returns a pattern which matches the null string if the
-- cursor is after the provided column, measured from the right
rtab :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
rtab (a:_) = do
    i <- toInteger a
    if i >= 0
        then return $ Just $ PatternData $ RTabPattern i
        else liftEval $ programError NegativeNumberInIllegalContext
rtab [] = rtab [StringData ""]

size :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
size = const $ liftEval $ programError ErrorInSnobol4System

-- | The span function, returns a pattern which matches the longest string
-- containing only the provided characters
span :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
span (a:_) = do
    s <- toString a
    case s of
        "" -> liftEval $ programError NullStringInIllegalContext
        _ -> return $ Just $ PatternData $ SpanPattern s
span [] = span [StringData ""]

stoptr :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
stoptr = const $ liftEval $ programError ErrorInSnobol4System

-- | The tab function, returns a pattern which matches the null string if the
-- cursor is before the provided column, measured from the left
tab :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
tab (a:_) = do
    i <- toInteger a
    if i >= 0
        then return $ Just $ PatternData $ TabPattern i
        else liftEval $ programError NegativeNumberInIllegalContext
tab [] = tab [StringData ""]

-- | The table function, returns an empty table
table :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
table (a:_) = do
    i <- toInteger a
    if i >= 0
        then Just . TableData <$> liftEval tablesNew
        else liftEval $ programError NegativeNumberInIllegalContext
table _ = Just . TableData <$> liftEval tablesNew

time :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
time = const $ liftEval $ programError ErrorInSnobol4System

trace :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
trace = const $ liftEval $ programError ErrorInSnobol4System

trim :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
trim = const $ liftEval $ programError ErrorInSnobol4System

value :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
value = const $ liftEval $ programError ErrorInSnobol4System

