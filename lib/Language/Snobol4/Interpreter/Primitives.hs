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
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Snobol4.Interpreter.Primitives where

import Prelude hiding ( span, break, any, toInteger )

import qualified Data.Map as M
import Data.List (genericReplicate)

import Control.Monad
import Control.Monad.Trans

import Language.Snobol4.Parser

import Language.Snobol4.Interpreter.Shell (InterpreterShell)
import qualified Language.Snobol4.Interpreter.Shell as Shell
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Data

import {-# SOURCE #-} Language.Snobol4.Interpreter.Internal

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.Arrays
import Language.Snobol4.Interpreter.Internal.StateMachine.Tables
import Language.Snobol4.Interpreter.Internal.StateMachine.Functions
import Language.Snobol4.Interpreter.Internal.StateMachine.UserData
import Language.Snobol4.Interpreter.Internal.StateMachine.Variables
import Language.Snobol4.Interpreter.Internal.StateMachine.ObjectCode
import Language.Snobol4.Interpreter.Internal.StateMachine.Convert
import Language.Snobol4.Interpreter.Internal.StateMachine.Labels
import Language.Snobol4.Interpreter.Internal.StateMachine.Error
import Language.Snobol4.Interpreter.Primitives.Prototypes


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

addPrimitives :: forall m . InterpreterShell m => Interpreter m ()
addPrimitives = do
    let funcs :: [Function m]
        funcs = primitiveFunctions
        funcMap :: M.Map Snobol4String (Function m)
        funcMap = M.fromList $ zip (map funcName funcs) funcs 
    mapM_ (uncurry varWrite) primitiveVars
    putFunctions funcMap


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

-- | The clear function, resets the values of all natural variables
clear :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
clear [] = liftEval $ do
    ns <- naturalVarNames
    forM ns clearVar
    return $ Just $ StringData nullString
clear _ = liftEval $ programError IncorrectNumberOfArguments

-- | The code function, parses a string containing source code and creates a
-- code object
code :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
code [src] = do
    src' <- toString src
    result <- parseT $ unmkString src'
    case result of
        Left _ -> return Nothing
        Right stmts -> do
            newKey <- liftEval $ codesNew $ newCode stmts
            return $ Just $ CodeData newKey
code _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO
collect :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
collect = const $ liftEval $ programError ErrorInSnobol4System

-- | The convert function, attempts to convert the first argument to the type
-- specified by the second as a string
convert :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
convert [toConvert,resultType] = do
    resultType' <- toString resultType
    case toConvert of
        StringData _ -> case () of
            ()
                | resultType' == datatypeNameString -> return $ Just toConvert
                | resultType' == datatypeNameInteger -> Just . IntegerData <$> toInteger toConvert
                | resultType' == datatypeNameReal -> Just . RealData <$> toReal toConvert
                | resultType' == datatypeNameExpression -> undefined
                | resultType' == datatypeNameCode -> undefined
                | otherwise -> return Nothing
        IntegerData _ -> case () of
            ()
                | resultType' == datatypeNameString -> Just . StringData <$> toString toConvert
                | resultType' == datatypeNameInteger -> return $ Just toConvert
                | resultType' == datatypeNameReal -> Just . RealData <$> toReal toConvert
                | otherwise -> return Nothing
        RealData _ -> case () of
            ()
                | resultType' == datatypeNameString -> Just . StringData <$> toString toConvert
                | resultType' == datatypeNameInteger -> Just . IntegerData <$> toInteger toConvert
                | resultType' == datatypeNameReal -> return $ Just toConvert
                | otherwise -> return Nothing
        TempPatternData (UnevaluatedExprPattern _) -> undefined
        PatternData _ -> case () of
            ()
                | resultType' == datatypeNameString -> return $ Just $ StringData datatypeNamePattern
                | resultType' == datatypeNamePattern -> return $ Just toConvert
                | otherwise -> return Nothing
        ArrayData k -> case () of
            ()
                | resultType' == datatypeNameString -> do
                    result <- liftEval $ arraysLookup k
                    case result of
                        Nothing -> liftEval $ programError ErrorInSnobol4System
                        Just arr -> return $ Just $ StringData $ arrayFormalIdent arr
                | resultType' == datatypeNameArray -> return $ Just toConvert
                | resultType' == datatypeNameTable -> do
                    result <- liftEval $ arraysLookup k
                    case result of
                        Nothing -> liftEval $ programError ErrorInSnobol4System
                        Just arr -> do
                            tabResult <- liftEval $ arrayToTable arr
                            case tabResult of
                                Nothing -> return Nothing
                                Just tab -> do
                                    k' <- liftEval $ tablesNew' tab
                                    return $ Just $ TableData k'
                | otherwise -> return Nothing
        TableData k -> case () of
            ()
                | resultType' == datatypeNameString -> do
                    result <- liftEval $ tablesLookup k
                    case result of
                        Nothing -> liftEval $ programError ErrorInSnobol4System
                        Just tab -> return $ Just $ StringData $ tableFormalIdent tab
                | resultType' == datatypeNameArray -> do
                    result <- liftEval $ tablesLookup k
                    case result of
                        Nothing -> liftEval $ programError ErrorInSnobol4System
                        Just tab -> do
                            arrResult <- liftEval $ tableToArray tab
                            case arrResult of
                                Nothing -> return Nothing
                                Just arr -> do
                                    k' <- liftEval $ arraysNew' arr
                                    return $ Just $ ArrayData k'
                | resultType' == datatypeNameTable -> return $ Just toConvert
                | otherwise -> return Nothing
        Name _ -> case () of
            ()
                | resultType' == datatypeNameString -> return $ Just $ StringData datatypeNameName
                | resultType' == datatypeNameName -> return $ Just toConvert
                | otherwise -> return Nothing
        CodeData _ -> case () of
            ()
                | resultType' == datatypeNameString -> return $ Just $ StringData datatypeNameCode
                | otherwise -> return Nothing
        UserData k -> do
            result <- liftEval $ userDataLookup k
            case result of
                Nothing -> liftEval $ programError ErrorInSnobol4System
                Just userData -> case () of
                    ()
                        | resultType' == datatypeNameString -> do
                            return $ Just $ StringData $ datatypeNameUser userData
                        | resultType' == datatypeNameUser userData -> return $ Just toConvert
                        | otherwise -> return Nothing
convert _ = liftEval $ programError IncorrectNumberOfArguments

-- | The copy function, creates a new instance of the given array
copy :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
copy [arg] = do
    case arg of
        ArrayData k -> do
            result <- liftEval $ arraysCopy k
            case result of
                Nothing -> liftEval $ programError ErrorInSnobol4System
                Just k' -> return $ Just $ ArrayData k'
        _ -> liftEval $ programError IllegalDataType
copy _ = liftEval $ programError ErrorInSnobol4System

-- | The data function, parses a data prototype to create a user-defined
-- datatype
data_ :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
data_ [arg] = do
    protoStr <- toString arg
    parseResult <- parseT $ unmkString protoStr
    DataPrototype dataName fieldNames <- case parseResult of
        Left _ -> liftEval $ programError ErroneousPrototype
        Right datatype -> return datatype
    let datatype = Snobol4Datatype dataName fieldNames
    liftEval $ datatypesNew datatype
    return $ Just $ StringData $ nullString
data_ _ = liftEval $ programError IncorrectNumberOfArguments

-- | The datatype function, returns the string representation of the type of
-- the argument
datatype :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
datatype [arg] = do
    case arg of
        UserData k -> do
            result <- liftEval $ userDataLookup k
            case result of
                Nothing -> liftEval $ programError ErrorInSnobol4System
                Just userData -> do
                    datatypeResult <- liftEval $ datatypesLookup $ datatypeNameUser userData
                    case datatypeResult of
                        Nothing -> liftEval $ programError ErrorInSnobol4System
                        Just datatype -> return $ Just $ StringData $ datatypeName datatype
        _ -> return $ Just $ StringData $ mkString $ case arg of
            StringData _ -> datatypeNameString
            PatternData _ -> datatypeNamePattern
            TempPatternData (UnevaluatedExprPattern _) -> datatypeNameExpression
            TempPatternData _ -> datatypeNamePattern
            IntegerData _ -> datatypeNameInteger
            RealData _ -> datatypeNameReal
            ArrayData _ -> datatypeNameArray
            TableData _ -> datatypeNameTable
            Name _ -> datatypeNameName
            CodeData _ -> datatypeNameCode
datatype _ = liftEval $ programError IncorrectNumberOfArguments

-- | The date function, returns the current date in mm/dd/yyyy format
date :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
date [] = Just . StringData . mkString <$> (liftEval $ lift Shell.date)
date _ = liftEval $ programError IncorrectNumberOfArguments

-- | The define function, creates a user-defined function
define :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
define [arg,labelArg] = do
    protoStr <- toString arg
    labelStr <- toString labelArg
    parseResult <- parseT $ unmkString protoStr
    FunctionPrototype funcName argNames localNames <- case parseResult of
        Left _ -> liftEval $ programError ErroneousPrototype
        Right func -> return func
    labelResult <- liftEval $ labelLookup labelStr
    Label addr <- case labelResult of
        Just l -> return l
        Nothing -> liftEval $ programError EntryPointOfFunctionNotLabel
    let func = UserFunction funcName argNames localNames addr
    liftEval $ functionsNew func
    return Nothing
define _ = liftEval $ programError ErrorInSnobol4System

-- | TODO
detach :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
detach = const $ liftEval $ programError ErrorInSnobol4System

-- | The differ function, returns the null string if the two arguments are not
-- the same
differ :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
differ [a,b] = do
    if a /= b
        then return $ Just $ StringData ""
        else return Nothing
differ _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO
dump :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
dump = const $ liftEval $ programError ErrorInSnobol4System

-- | The dupl function, creates a string by repeating a sub-string
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

-- | The eval function, evaluates an unevaluated expression
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

-- | The field function, returns the name of the nth field of a user-defined
-- data type
field :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
field [dname,n] = do
    dname' <- toString dname
    n' <- toInteger n
    datatypeResult <- liftEval $ datatypeLookup dname'
    case datatypeResult of
        Nothing -> liftEval $ programError IllegalArgumentToPrimitiveFunction
        Just datatype -> case drop (unmkInteger n') $ datatypeFieldNames datatype of
            [] -> liftEval $ programError IncorrectNumberOfArguments
            (arg:_) -> return $ Just $ StringData arg
field _ = liftEval $ programError ErrorInSnobol4System

-- | Greater than or equal predicate
ge :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
ge = numericalPredicate (>=)

-- | Greater than predicate
gt :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
gt = numericalPredicate (>)

-- | The ident function, returns the null string if the two arguments are the
-- same
ident :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
ident [a,b] = do
    if a == b
        then return $ Just $ StringData ""
        else return Nothing
ident _ = liftEval $ programError IncorrectNumberOfArguments

-- | Integer predicate
integer :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
integer [a] = do
    result <- toInteger a
    return $ Just $ StringData ""
integer [] = return $ Just $ StringData ""

-- | The item function, indexes an array or table
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

-- | The lgt function, returns the null string if the first argument comes after
-- the second, lexically
lgt :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
lgt [a,b] = do
    a' <- toString a
    b' <- toString b
    if a' > b'
        then return $ Just $ StringData ""
        else return Nothing
lgt _ = liftEval $ programError IncorrectNumberOfArguments

-- | The local function, returns the name of the nth local variable of a user-defined function
local :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
local [fname,n] = do
    fname' <- toString fname
    n' <- toInteger n
    funcResult <- liftEval $ funcLookup fname'
    case funcResult of
        Nothing -> liftEval $ programError UndefinedFunctionOrOperation
        Just func -> case drop (unmkInteger n') $ localNames func of
            [] -> liftEval $ programError IncorrectNumberOfArguments
            (arg:_) -> return $ Just $ StringData arg
local _ = liftEval $ programError ErrorInSnobol4System

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

-- | The pos function, returns a pattern that succeeds if the cursor is at the
-- given column
pos :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
pos [a] = do
    result <- toInteger a
    return $ Just $ TempPatternData $ PosPattern result
pos _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO 
prototype :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
prototype = const $ liftEval $ programError ErrorInSnobol4System

-- | The remainder primitive, returns the the remainder of the second argument
-- divided by the first
remdr :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
remdr [a,b] = do
    a' <- toInteger a
    b' <- toInteger b
    return $ Just $ IntegerData $ b' `rem` a'
remdr _ = liftEval $ programError IncorrectNumberOfArguments

-- | The replace function, returns a string formed by replacing instances of a
-- substring with another
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

-- | The rpos function, creates a pattern which succeeds if the cursor is the
-- current position from the right
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

-- | The size function, returns the length of the argument
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

-- | The time primitive, returns the number of seconds ellapsed since the
-- beginning of the program
time :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
time [] = Just . IntegerData . mkInteger <$> (liftEval $ lift Shell.time)
time _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO 
trace :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
trace = const $ liftEval $ programError ErrorInSnobol4System

-- | The trim function, removes whitespace from a string
trim :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
trim [a] = do
    a' <- toString a
    return $ Just $ StringData $ snobol4Trim a' 
trim _ = liftEval $ programError IncorrectNumberOfArguments

-- | TODO
unload :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
unload = const $ liftEval $ programError ErrorInSnobol4System

-- | The value function, looks up a variable by name
value :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
value [arg] = do
    name <- toString arg
    return $ Just $ Name $ LookupId name
value _ = liftEval $ programError IncorrectNumberOfArguments


