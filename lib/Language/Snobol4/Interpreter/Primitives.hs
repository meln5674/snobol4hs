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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Language.Snobol4.Interpreter.Primitives 
    ( addPrimitives
    ) where

import Prelude hiding ( span, break, any, toInteger )

import qualified Data.Map as M
import Data.List (genericReplicate)

import Control.Monad
import Control.Monad.Trans

import Language.Snobol4.Parser

import Language.Snobol4.Syntax.AST (Operator(..))

import Language.Snobol4.Interpreter.Shell (InterpreterShell)
import qualified Language.Snobol4.Interpreter.Shell as Shell
import Language.Snobol4.Interpreter.Error
--import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Evaluator

import Language.Snobol4.Interpreter.Internal.StateMachine.Types hiding ( call, eval, code)
import qualified Language.Snobol4.Interpreter.Internal.StateMachine.Types as StMch
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.Arrays
import Language.Snobol4.Interpreter.Internal.StateMachine.Tables
import Language.Snobol4.Interpreter.Internal.StateMachine.Functions
import Language.Snobol4.Interpreter.Internal.StateMachine.Keywords
import Language.Snobol4.Interpreter.Internal.StateMachine.UserData
import Language.Snobol4.Interpreter.Internal.StateMachine.Variables
--import Language.Snobol4.Interpreter.Internal.StateMachine.Run
import Language.Snobol4.Interpreter.Internal.StateMachine.ObjectCode
import Language.Snobol4.Interpreter.Internal.StateMachine.Convert
import Language.Snobol4.Interpreter.Internal.StateMachine.Labels
import Language.Snobol4.Interpreter.Internal.StateMachine.Error
import Language.Snobol4.Interpreter.Internal.StateMachine.Patterns
import Language.Snobol4.Interpreter.Primitives.Prototypes


-- | The names and initial values of the primitive variables
primitiveVars :: [(Snobol4String, Data expr)]
primitiveVars =
    [ ("NULL",  StringData nullString)
    , ("REM",   TempPatternData $ RTabPattern $ EvaluatedThunk 0)
    , ("FAIL",  TempPatternData FailPattern)
    , ("FENCE", TempPatternData FencePattern)
    , ("ABORT", TempPatternData AbortPattern)
    , ("ARB", TempPatternData ArbPattern)
    , ("BAL", TempPatternData BalPattern)
    , ("SUCCEED", TempPatternData SucceedPattern)
    ]

-- | Initial values of unary operators
primitiveUnOps :: ( NewSnobol4Machine m
                  , InterpreterShell m
                  , LocalVariablesClass m
                  )
               => [(Operator, OpSyn (ProgramType m) m)]
primitiveUnOps =
    [ (And, PrimitiveOperator unOp_and)
    , (Dollar, PrimitiveOperator unOp_dollar)
    , (At, PrimitiveOperator unOp_at)
    ]

-- | Initial values of binary operators
primitiveBinOps :: ( NewSnobol4Machine m
                   , InterpreterShell m
                   , LocalVariablesClass m
                   )
                => [(Operator, OpSyn (ProgramType m) m)]
primitiveBinOps =
    [ (Pipe, PrimitiveOperator binOp_pipe)
    , (Blank, PrimitiveOperator binOp_blank)
    , (Minus, PrimitiveOperator binOp_minus)
    , (Plus, PrimitiveOperator binOp_plus)
    , (Slash, PrimitiveOperator binOp_slash)
    , (Star, PrimitiveOperator binOp_star)
    , (Bang, PrimitiveOperator binOp_bang)
    , (DoubleStar, PrimitiveOperator binOp_doubleStar)
    , (Dot, PrimitiveOperator binOp_dot)
    , (Dollar, PrimitiveOperator binOp_dollar)
    ]

-- | The names and actions of the primitive functions
primitiveFunctions
    :: ( InterpreterShell m
       , NewSnobol4Machine m
       , LocalVariablesClass m
       , ProgramClass (ProgramType m)
       , Ord (ExprType m)
       ) 
    => [Function (ProgramType m) m]
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
    , PrimitiveFunction "TAB"       tab
    , PrimitiveFunction "TABLE"     table
    , PrimitiveFunction "TIME"      time
    , PrimitiveFunction "TRACE"     trace
    , PrimitiveFunction "TRIM"      trim
    , PrimitiveFunction "VALUE"     value
    ]

-- | Initial values of protected keywords
primitiveProtectedKeywords :: [(Snobol4String, Data expr)]
primitiveProtectedKeywords =
    [ ("ALPHABET", keyword_alphabet)
    , ("ABORT", keyword_abort)
    , ("ARB", keyword_arb)
    , ("BAL", keyword_bal)
    , ("ERRTYPE", keyword_errtype)
    , ("FAIL", keyword_fail)
    , ("FENCE", keyword_fence)
    , ("FNCLEVEL", keyword_fnclevel)
    , ("LASTNO", keyword_lastno)
    , ("REM", keyword_rem)
    , ("RTNTYPE", keyword_rtntype)
    , ("STCOUNT", keyword_stcount)
    , ("STFCOUNT", keyword_stfcount)
    , ("STNO", keyword_stno)
    , ("SUCCEED", keyword_succeed)
    ]

-- | Initial values of unprotected keywords
primitiveUnprotectedKeywords =
    [ ("ABEND", keyword_abend)
    , ("ANCHOR", keyword_anchor)
    , ("CODE", keyword_code)
    , ("DUMP", keyword_dump)
    , ("ERRLIMIT", keyword_errlimit)
    , ("FTRACE", keyword_ftrace)
    , ("FULLSCAN", keyword_fullscan)
    , ("INPUT", keyword_input)
    , ("MAXLNGTH", keyword_maxlngth)
    , ("OUTPUT", keyword_output)
    , ("STLIMIT", keyword_stlimit)
    , ("TRACE", keyword_trace)
    , ("TRIM", keyword_trim)
    ]
    
-- | Raise an error if a value is Nothing
maybeError :: Monad m 
           => ProgramError 
           -> Maybe a 
           -> InterpreterGeneric program m a
maybeError _ (Just x) = return x
maybeError err _ = programError err

-- | Raise an error if an action yields Nothing
maybeErrorM :: Monad m 
            => ProgramError 
            -> InterpreterGeneric program m (Maybe a) 
            -> InterpreterGeneric program m a
maybeErrorM err m = m >>= maybeError err 

-- | Add the primitive variables and functions to the current interpreter state
addPrimitives :: forall program instruction m 
               . ( InterpreterShell m
                 , NewSnobol4Machine m
                 , LocalVariablesClass m
                 , ProgramClass (ProgramType m)
                 , Ord (ExprType m)
                 )
              => InterpreterGeneric (ProgramType m) m ()
addPrimitives = do
    let funcs :: [Function (ProgramType m) m]
        funcs = primitiveFunctions
        funcMap :: M.Map Snobol4String (Function (ProgramType m) m)
        funcMap = M.fromList $ zip (map primName funcs) funcs 
    mapM_ (uncurry varWrite) primitiveVars
    mapM_ (uncurry setBinOpSyn) primitiveBinOps
    mapM_ (uncurry setUnOpSyn) primitiveUnOps
    putFunctions funcMap
    putProtectedKeywords $ M.fromList primitiveProtectedKeywords
    putUnprotectedKeywords $ M.fromList primitiveUnprotectedKeywords

-- | Generalization for lt, le, eq, ne, ge, and gt
numericalPredicate :: ( InterpreterShell m
--                      , LocalVariablesClass m
--                      , NewSnobol4Machine m
                      ) 
                  => (forall a . (Eq a, Ord a, Num a) => a -> a -> Bool)
                  -> [Data (ExprType m)] 
                  -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
numericalPredicate pred [a,b] = do
    args <- raiseArgs a b
    {-(a'',b'') <- case a' of
        StringData a -> (,) <$> (IntegerData <$> toInteger a') <*> (IntegerData <$> toInteger b')
        _ -> return (a',b')-}
    result <- case args of
        Just (IntegerData a', IntegerData b') -> return $ a' `pred` b'
        Just (RealData a', RealData b') -> return $ a' `pred` b'
        _ -> programError IllegalDataType
    return $ if result
        then Just $ StringData ""
        else Nothing
numericalPredicate pred [a] = numericalPredicate pred [a,IntegerData 0]
numericalPredicate _ _ = programError IncorrectNumberOfArguments

-- | The any function, returns a pattern which matches any one of the provided
-- characters
any :: ( InterpreterShell m 
       , NewSnobol4Machine m
       ) 
    => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
any [a] = do
    cs <- toLazyString a
    case cs of
        (EvaluatedThunk "") -> programError NullStringInIllegalContext
        _ -> return $ Just $ TempPatternData $ AnyPattern cs
any [] = any [StringData ""]
any _ = programError IncorrectNumberOfArguments

-- | The apply function, calls a function by name with arguments
apply :: ( InterpreterShell m
         
         , LocalVariablesClass m 
         )
      => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
{-
apply (nameArg:args) = do
    name <- toString nameArg
    StMch.call name args
-}
apply _ = programError IncorrectNumberOfArguments

-- | The array function, creates an array with the provided dimensions and initial value
array :: ( InterpreterShell m 
         , NewSnobol4Machine m
         ) 
      => [(Data (ExprType m))] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
array [dimStr,val] = do
    str <- toString dimStr
    parseResult <- parseT $ unmkString str
    case parseResult of
        Left _ -> return Nothing
        Right (ArrayPrototype dims) -> Just <$> (arraysNew'' dims val)
array [dimStr] = array [dimStr, StringData ""]
array [] = programError NullStringInIllegalContext
array _ = programError IncorrectNumberOfArguments

-- | The arbno function, returns a pattern which matches an arbitrary number of
-- repetitions of the provided pattern
arbno :: ( InterpreterShell m ) 
      => [(Data (ExprType m))] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
arbno (a:_) = do
    p <- toLazyPattern a
    return $ Just $ TempPatternData $ ArbNoPattern p
arbno [] = arbno [StringData ""]

-- | The arg function, returns the name of the nth argument of a function
arg :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
    => [(Data (ExprType m))] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
arg [fname,n] = do
    fname' <- toString fname
    n' <- maybeErrorM IllegalArgumentToPrimitiveFunction $ toInteger n
    funcResult <- funcLookup fname'
    case funcResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just (UserFunction func) -> case drop (unmkInteger n') $ formalArgs func of
            [] -> programError IncorrectNumberOfArguments
            (arg:_) -> return $ Just $ StringData arg
        Just _ -> programError IllegalArgumentToPrimitiveFunction
arg _ = programError IncorrectNumberOfArguments

-- | TODO
backspace :: ( InterpreterShell m ) 
          => [(Data (ExprType m))] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
backspace = const $ programError ErrorInSnobol4System

-- | The break function, returns a pattern which matches the longest string
-- containing none of the provided characters
break :: ( InterpreterShell m ) 
      => [(Data (ExprType m))] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
break (a:_) = do
    s <- toLazyString a
    case s of
        (EvaluatedThunk "") -> programError NullStringInIllegalContext
        _ ->  return $ Just $ TempPatternData $ BreakPattern s
break [] = break [StringData ""]

-- | The clear function, resets the values of all natural variables
clear :: ( InterpreterShell m, LocalVariablesClass m ) 
      => [(Data (ExprType m))] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
clear [] = do
    ns <- naturalVarNames
    forM ns clearVar
    return $ Just $ StringData nullString
clear _ = programError IncorrectNumberOfArguments

-- | The code function, parses a string containing source code and creates a
-- code object
code :: ( InterpreterShell m ) 
      => [(Data (ExprType m))] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
code [src] = do
    src' <- toString src
    result <- parseT $ unmkString src'
    case result of
        Left _ -> return Nothing
        Right stmts -> do
            newKey <- codesNew $ newCode stmts
            return $ Just $ CodeData newKey
code _ = programError IncorrectNumberOfArguments

-- | TODO
collect :: ( InterpreterShell m ) 
        => [(Data (ExprType m))] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
collect = const $ programError ErrorInSnobol4System

-- | The convert function, attempts to convert the first argument to the type
-- specified by the second as a string
convert :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
        => [(Data (ExprType m))] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
convert [toConvert,resultType] = do
    resultType' <- toString resultType
    case toConvert of
        StringData{}
            | resultType' == datatypeNameString -> return $ Just toConvert
            | resultType' == datatypeNameInteger -> liftM (liftM IntegerData) $ toInteger toConvert
            | resultType' == datatypeNameReal -> liftM (liftM RealData) $ toReal toConvert
            | resultType' == datatypeNameExpression -> return $ Just $ StringData datatypeNameExpression
            | resultType' == datatypeNameCode -> return $ Just $ StringData datatypeNameCode
            | otherwise -> return Nothing
        IntegerData{}
            | resultType' == datatypeNameString -> Just . StringData <$> toString toConvert
            | resultType' == datatypeNameInteger -> return $ Just toConvert
            | resultType' == datatypeNameReal -> liftM (liftM RealData) $ toReal toConvert
            | otherwise -> return Nothing
        RealData{}
            | resultType' == datatypeNameString -> Just . StringData <$> toString toConvert
            | resultType' == datatypeNameInteger -> liftM (liftM IntegerData) $ toInteger toConvert
            | resultType' == datatypeNameReal -> return $ Just toConvert
            | otherwise -> return Nothing
        TempPatternData{}
            | resultType' == datatypeNameString -> return $ Just $ StringData datatypeNamePattern
            | resultType' == datatypeNamePattern -> return $ Just toConvert
            | otherwise -> return Nothing
        PatternData{}
            | resultType' == datatypeNameString -> return $ Just $ StringData datatypeNamePattern
            | resultType' == datatypeNamePattern -> return $ Just toConvert
            | otherwise -> return Nothing
        ArrayData k 
            | resultType' == datatypeNameString -> do
                result <- arraysLookup k
                case result of
                    Nothing -> programError ErrorInSnobol4System
                    Just arr -> return $ Just $ StringData $ arrayFormalIdent arr
            | resultType' == datatypeNameArray -> return $ Just toConvert
            | resultType' == datatypeNameTable -> do
                result <- arraysLookup k
                case result of
                    Nothing -> programError ErrorInSnobol4System
                    Just arr -> do
                        tabResult <- arrayToTable arr
                        case tabResult of
                            Nothing -> return Nothing
                            Just tab -> do
                                k' <- tablesNew' tab
                                return $ Just $ TableData k'
            | otherwise -> return Nothing
        TableData k
            | resultType' == datatypeNameString -> do
                result <- tablesLookup k
                case result of
                    Nothing -> programError ErrorInSnobol4System
                    Just tab -> return $ Just $ StringData $ tableFormalIdent tab
            | resultType' == datatypeNameArray -> do
                result <- tablesLookup k
                case result of
                    Nothing -> programError ErrorInSnobol4System
                    Just tab -> do
                        arrResult <- tableToArray tab
                        case arrResult of
                            Nothing -> return Nothing
                            Just arr -> do
                                k' <- arraysNew' arr
                                return $ Just $ ArrayData k'
            | resultType' == datatypeNameTable -> return $ Just toConvert
            | otherwise -> return Nothing
        Name _ 
            | resultType' == datatypeNameString -> return $ Just $ StringData datatypeNameName
            | resultType' == datatypeNameName -> return $ Just toConvert
            | otherwise -> return Nothing
        CodeData _ 
            | resultType' == datatypeNameString -> return $ Just $ StringData datatypeNameCode
            | otherwise -> return Nothing
        UserData k -> do
            result <- userDataLookup k
            case result of
                Nothing -> programError ErrorInSnobol4System
                Just userData 
                    | resultType' == datatypeNameString -> do
                        return $ Just $ StringData $ datatypeNameUser userData
                    | resultType' == datatypeNameUser userData -> return $ Just toConvert
                    | otherwise -> return Nothing
        ReferenceId{} -> programError ErrorInSnobol4System
        ReferenceAggregate{} -> programError ErrorInSnobol4System
        ReferenceUserData{} -> programError ErrorInSnobol4System
        ReferenceKeyword{} -> programError ErrorInSnobol4System
convert _ = programError IncorrectNumberOfArguments

-- | The copy function, creates a new instance of the given array
copy :: ( InterpreterShell m ) 
     => [(Data (ExprType m))] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
copy [arg] = do
    case arg of
        ArrayData k -> do
            result <- arraysCopy k
            case result of
                Nothing -> programError ErrorInSnobol4System
                Just k' -> return $ Just $ ArrayData k'
        _ -> programError IllegalDataType
copy _ = programError ErrorInSnobol4System

-- | Uncurry a 3-arity function
uncurry3 :: (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3 f (a,b,c) = f a b c

-- | The data function, parses a data prototype to create a user-defined
-- datatype
data_ :: ( InterpreterShell m ) 
      => [(Data (ExprType m))] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
data_ [arg] = do
    protoStr <- toString arg
    parseResult <- parseT $ unmkString protoStr
    DataPrototype dataName fieldNames <- case parseResult of
        Left _ -> programError ErroneousPrototype
        Right datatype -> return datatype
    let datatype = Snobol4Datatype dataName fieldNames
    datatypesNew datatype
    forM (zip3 fieldNames (repeat dataName) [0..]) $ uncurry3 selectorFunctionsNew
    constructorFunctionsNew dataName (length fieldNames)
    return $ Just $ StringData $ nullString
data_ _ = programError IncorrectNumberOfArguments

-- | The datatype function, returns the string representation of the type of
-- the argument
datatype :: ( InterpreterShell m ) 
         => [(Data (ExprType m))] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
datatype [arg] = do
    result <- case arg of
        UserData k -> do
            result <- userDataLookup k
            case result of
                Nothing -> programError ErrorInSnobol4System
                Just userData -> do
                    datatypeResult <- datatypesLookup $ datatypeNameUser userData
                    case datatypeResult of
                        Nothing -> programError ErrorInSnobol4System
                        Just datatype -> return $ Just $ datatypeName datatype
        StringData{} -> return $ Just datatypeNameString
        PatternData{} -> return $ Just datatypeNamePattern
        --TempPatternData (UnevaluatedExprPattern _) -> return $ Just datatypeNameExpression
        TempPatternData{} -> return $ Just datatypeNamePattern
        IntegerData{} -> return $ Just datatypeNameInteger
        RealData{} -> return $ Just datatypeNameReal
        ArrayData{} -> return $ Just datatypeNameArray
        TableData{} -> return $ Just datatypeNameTable
        Name{} -> return $ Just datatypeNameName
        CodeData{} -> return $ Just datatypeNameCode
        ReferenceId{} -> return $ Nothing
        ReferenceAggregate{} -> return $ Nothing
        ReferenceUserData{} -> return $ Nothing
        ReferenceKeyword{} -> return $ Nothing
    case result of
        Just x -> return $ Just $ StringData $ mkString x
        Nothing -> programError ErrorInSnobol4System
            
datatype _ = programError IncorrectNumberOfArguments

-- | The date function, returns the current date in mm/dd/yyyy format
date :: ( InterpreterShell m ) 
     => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
date [] = Just . StringData . mkString <$> (lift Shell.date)
date _ = programError IncorrectNumberOfArguments

-- | The define function, creates a user-defined function
define :: ( InterpreterShell m ) 
       => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
define [arg,labelArg] = do
    protoStr <- toString arg
    labelStr <- toString labelArg
    parseResult <- parseT $ unmkString protoStr
    FunctionPrototype funcName argNames localNames <- case parseResult of
        Left _ -> programError ErroneousPrototype
        Right func -> return func
    labelResult <- labelLookup labelStr
    Label addr <- case labelResult of
        Just l -> return l
        Nothing -> programError EntryPointOfFunctionNotLabel
    let func = Function funcName argNames localNames addr
    functionsNew func
    return $ Just $ StringData nullString
define [arg] = do
    protoStr <- toString arg
    parseResult <- parseT $ unmkString protoStr
    FunctionPrototype funcName argNames localNames <- case parseResult of
        Left _ -> programError ErroneousPrototype
        Right func -> return func
    labelResult <- labelLookup funcName
    Label addr <- case labelResult of
        Just l -> return l
        Nothing -> programError EntryPointOfFunctionNotLabel
    let func = Function funcName argNames localNames addr
    functionsNew func
    return $ Just $ StringData nullString
define _ = programError ErrorInSnobol4System

-- | TODO
detach :: ( InterpreterShell m ) 
       => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
detach = const $ programError ErrorInSnobol4System

-- | The differ function, returns the null string if the two arguments are not
-- the same
differ :: ( InterpreterShell m, Eq (ExprType m) ) 
       => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
differ [a,b] = do
    if a /= b
        then return $ Just $ StringData ""
        else return Nothing
differ [a] = differ [a,StringData nullString]
differ _ = programError IncorrectNumberOfArguments

-- | TODO
dump :: ( InterpreterShell m ) 
     => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
dump = const $ programError ErrorInSnobol4System

-- | The dupl function, creates a string by repeating a sub-string
dupl :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
     => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
dupl [a,b] = do
    a' <- toString a
    b' <- maybeErrorM IllegalArgumentToPrimitiveFunction $ toInteger b
    return $ Just $ StringData $ foldl (<>) "" $ genericReplicate b' a'
dupl _ = programError IncorrectNumberOfArguments

-- | TODO
endfile :: ( InterpreterShell m ) 
        => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
endfile = const $ programError ErrorInSnobol4System

-- | Equality predicate
eq :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
   => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
eq = numericalPredicate (==)

-- | The eval function, evaluates an unevaluated expression
eval :: ( InterpreterShell m
        , NewSnobol4Machine m
        , LocalVariablesClass m 
        )  
     => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
eval [a] = do
    expr <- case a of
        ExprData expr -> return expr
        _ -> do
            a' <- toString a
            result <- parseT (unmkString a')
            case result of
                Left err -> programError IllegalArgumentToPrimitiveFunction
                Right expr -> registerExpr expr
    StMch.eval expr
eval _ = programError IncorrectNumberOfArguments

-- | The field function, returns the name of the nth field of a user-defined
-- data type
field :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
      => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
field [dname,n] = do
    dname' <- toString dname
    n' <- maybeErrorM IllegalArgumentToPrimitiveFunction $ toInteger n
    datatype <- maybeErrorM IllegalArgumentToPrimitiveFunction $ datatypesLookup dname'
    case drop (unmkInteger n') $ datatypeFieldNames datatype of
        [] -> programError IncorrectNumberOfArguments
        (arg:_) -> return $ Just $ StringData arg
field _ = programError ErrorInSnobol4System

-- | Greater than or equal predicate
ge :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
   => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
ge = numericalPredicate (>=)

-- | Greater than predicate
gt :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
   => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
gt = numericalPredicate (>)

-- | The ident function, returns the null string if the two arguments are the
-- same
ident :: ( InterpreterShell m
         
         , Eq (ExprType m)
         ) 
      => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
ident [a,b] = do
    if a == b
        then return $ Just $ StringData ""
        else return Nothing
ident [a] = ident [a,StringData nullString]
ident _ = programError IncorrectNumberOfArguments

-- | Integer predicate
integer :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
        => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
integer [a] = do
    result <- toInteger a
    return $ Just $ StringData ""
integer [] = return $ Just $ StringData ""
integer _ = programError IncorrectNumberOfArguments

-- | The item function, indexes an array or table
item :: ( InterpreterShell m
--        , NewSnobol4Machine m
        , LocalVariablesClass m
        , Ord (ExprType m)
        ) 
     => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
item (name:keys) = do
    name' <- toString name
    execLookup $ LookupAggregate name' keys
item _ = programError IncorrectNumberOfArguments

-- | Less than or equal predicate
le :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
   => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
le = numericalPredicate (<=)

-- | The length function, returns a pattern with matches the provided number
-- of characters
len :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
    => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
len (a:_) = do
    i <- maybeErrorM IllegalArgumentToPrimitiveFunction $ toLazyInteger a
    if ((0 <=) <$> i) `orLazy` True
        then return $ Just $ TempPatternData $ LengthPattern i
        else programError NegativeNumberInIllegalContext
len [] = len [StringData ""]

-- | The lgt function, returns the null string if the first argument comes after
-- the second, lexically
lgt :: ( InterpreterShell m ) 
    => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
lgt [a,b] = do
    a' <- toString a
    b' <- toString b
    if a' > b'
        then return $ Just $ StringData ""
        else return Nothing
lgt _ = programError IncorrectNumberOfArguments

-- | The local function, returns the name of the nth local variable of a user-defined function
local :: ( InterpreterShell m, LocalVariablesClass m ) 
      => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
local [fname,n] = do
    fname' <- toString fname
    n' <- maybeErrorM IllegalArgumentToPrimitiveFunction $ toInteger n
    funcResult <- funcLookup fname'
    case funcResult of
        Nothing -> programError UndefinedFunctionOrOperation
        Just (UserFunction func) -> case drop (unmkInteger n') $ localNames func of
            [] -> programError IncorrectNumberOfArguments
            (arg:_) -> return $ Just $ StringData arg
        Just _ -> programError IllegalArgumentToPrimitiveFunction
local _ = programError ErrorInSnobol4System

-- | Less than predicate
lt :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
   => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
lt = numericalPredicate (<)

-- | Inequality predicate
ne :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
   => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
ne = numericalPredicate (/=)

-- | The notany function, returns a pattern which matches one character not
-- provided
notany :: ( InterpreterShell m ) 
       => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
notany (a:_) = do
    cs <- toLazyString a
    case cs of
        (EvaluatedThunk "") -> programError NullStringInIllegalContext
        _ -> return $ Just $ TempPatternData $ NotAnyPattern cs
notany [] = notany [StringData ""]

-- | TODO
opsyn :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
      => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
opsyn = const $ programError ErrorInSnobol4System

-- | The pos function, returns a pattern that succeeds if the cursor is at the
-- given column
pos :: ( InterpreterShell m, LocalVariablesClass m ) 
    => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
pos [a] = do
    result <- maybeErrorM IllegalArgumentToPrimitiveFunction $ toLazyInteger a
    return $ Just $ TempPatternData $ PosPattern result
pos _ = programError IncorrectNumberOfArguments

-- | TODO 
prototype :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
          => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
prototype = const $ programError ErrorInSnobol4System

-- | The remainder primitive, returns the the remainder of the second argument
-- divided by the first
remdr :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
      => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
remdr [a,b] = do
    a' <- maybeErrorM IllegalArgumentToPrimitiveFunction $ toInteger a
    b' <- maybeErrorM IllegalArgumentToPrimitiveFunction $ toInteger b
    return $ Just $ IntegerData $ b' `rem` a'
remdr _ = programError IncorrectNumberOfArguments

-- | The replace function, returns a string formed by replacing instances of a
-- substring with another
replace :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
        => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
replace [x,y,z] = do
    x' <- toString x
    y' <- toString y
    z' <- toString z
    if snobol4Length y' /= snobol4Length z'
        then programError IllegalArgumentToPrimitiveFunction
        else return $ Just $ StringData $ snobol4Replace x' y' z'
replace _ = programError IncorrectNumberOfArguments

-- | TODO 
rewind :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
       => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
rewind = const $ programError ErrorInSnobol4System

-- | The rpos function, creates a pattern which succeeds if the cursor is the
-- current position from the right
rpos :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
      => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
rpos [a] = do
    result <- maybeErrorM IllegalArgumentToPrimitiveFunction $ toLazyInteger a
    return $ Just $ TempPatternData $ RPosPattern result
rpos _ = programError IncorrectNumberOfArguments

-- | The rtab function, returns a pattern which matches the null string if the
-- cursor is after the provided column, measured from the right
rtab :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
     => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
rtab (a:_) = do
    i <- maybeErrorM IllegalArgumentToPrimitiveFunction $ toLazyInteger a
    if ((0 <=) <$> i) `orLazy` True
        then return $ Just $ TempPatternData $ RTabPattern i
        else programError NegativeNumberInIllegalContext
rtab [] = rtab [StringData ""]

-- | The size function, returns the length of the argument
size :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
     => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
size [arg] = Just . IntegerData . snobol4Length <$> toString arg
size _ = programError IncorrectNumberOfArguments

-- | The span function, returns a pattern which matches the longest string
-- containing only the provided characters
span :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
     => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
span (a:_) = do
    s <- toLazyString a
    case s of
        (EvaluatedThunk "") -> programError NullStringInIllegalContext
        _ -> return $ Just $ TempPatternData $ SpanPattern s
span [] = span [StringData ""]

-- | TODO 
stoptr :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
       => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
stoptr = const $ programError ErrorInSnobol4System

-- | The tab function, returns a pattern which matches the null string if the
-- cursor is before the provided column, measured from the left
tab :: ( InterpreterShell m
    
    , LocalVariablesClass m  
    ) 
    => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
tab (a:_) = do
    i <- maybeErrorM IllegalArgumentToPrimitiveFunction $ toLazyInteger a
    if ((0 <=) <$> i) `orLazy` True
        then return $ Just $ TempPatternData $ TabPattern i
        else programError NegativeNumberInIllegalContext
tab [] = tab [StringData ""]

-- | The table function, returns an empty table
table :: ( InterpreterShell m, LocalVariablesClass m ) 
      => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
table (a:_) = do
    i <- maybeErrorM IllegalArgumentToPrimitiveFunction $ toInteger a
    if i >= 0
        then Just . TableData <$> tablesNew
        else programError NegativeNumberInIllegalContext
table _ = Just . TableData <$> tablesNew

-- | The time primitive, returns the number of seconds ellapsed since the
-- beginning of the program
time :: ( InterpreterShell m ) 
     => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
time [] = Just . IntegerData . mkInteger <$> (lift Shell.time)
time _ = programError IncorrectNumberOfArguments

-- | TODO 
trace :: ( InterpreterShell m ) 
      => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
trace = const $ programError ErrorInSnobol4System

-- | The trim function, removes whitespace from a string
trim :: ( InterpreterShell m ) 
     => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
trim [a] = do
    a' <- toString a
    return $ Just $ StringData $ snobol4Trim a' 
trim _ = programError IncorrectNumberOfArguments

-- | TODO
unload :: ( InterpreterShell m ) 
       => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
unload = const $ programError ErrorInSnobol4System

-- | The value function, looks up a variable by name
value :: ( InterpreterShell m ) 
      => [Data (ExprType m)] -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
value [arg] = do
    name <- toString arg
    return $ Just $ Name $ LookupId name
value _ = programError IncorrectNumberOfArguments

-- | Binary + operator
binOp_plus :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)]
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
binOp_plus [x,y] = liftM Just $ arithmetic (+) (+) x y
binOp_plus _ = programError IncorrectNumberOfArguments

-- | Binary - operator
binOp_minus :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)]
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
binOp_minus [x,y] = liftM Just $ arithmetic (-) (-) x y
binOp_minus _ = programError IncorrectNumberOfArguments

-- | Binary * operator
binOp_star :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)]
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
binOp_star [x,y] = liftM Just $ arithmetic (*) (*) x y
binOp_star _ = programError IncorrectNumberOfArguments

-- | Binary / operator
binOp_slash :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)]
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
binOp_slash [x,y] = liftM Just $ arithmetic (div) (/) x y
binOp_slash _ = programError IncorrectNumberOfArguments

-- | Binary ! operator
binOp_bang :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)]
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
binOp_bang [x,y] = liftM Just $ arithmetic (^) (**) x y
binOp_bang _ = programError IncorrectNumberOfArguments

-- | Binary ** operator
binOp_doubleStar :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)]
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
binOp_doubleStar [x,y] = liftM Just $ arithmetic (^) (**) x y
binOp_doubleStar _ = programError IncorrectNumberOfArguments

-- | Binary ' ' operator
binOp_blank :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)]
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
binOp_blank [x, StringData ""] = return $ Just x
binOp_blank [StringData "", y] = return $ Just y
binOp_blank [x@TempPatternData{},y] = do
    x' <- toLazyPattern x
    y' <- toLazyPattern y
    return $ Just $ TempPatternData $ ConcatPattern x' y'
binOp_blank [x, y@TempPatternData{}] = do
    x' <- toLazyPattern x
    y' <- toLazyPattern y
    return $ Just $ TempPatternData $ ConcatPattern x' y'
binOp_blank [x@PatternData{}, y] = do
    x' <- toLazyPattern x
    y' <- toLazyPattern y
    return $ Just $ TempPatternData $ ConcatPattern x' y'
binOp_blank [x, y@PatternData{}] = do
    x' <- toLazyPattern x
    y' <- toLazyPattern y
    return $ Just $ TempPatternData $ ConcatPattern x' y'
binOp_blank [x@ExprData{}, y] = do
    x' <- toLazyPattern x
    y' <- toLazyPattern y
    return $ Just $ TempPatternData $ ConcatPattern x' y'
binOp_blank [x, y@ExprData{}] = do
    x' <- toLazyPattern x
    y' <- toLazyPattern y
    return $ Just $ TempPatternData $ ConcatPattern x' y'
binOp_blank [x,y] = do
    x' <- toString x
    y' <- toString y
    return $ Just $ StringData $ x' <> y'
binOp_blank [x] = binOp_blank [x,StringData nullString]
binOp_blank _ = programError IncorrectNumberOfArguments

-- | Binary | operator
binOp_pipe :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)]
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
binOp_pipe [x, y] = do
    x' <- toLazyPattern x
    y' <- toLazyPattern y
    return $ Just $ TempPatternData $ AlternativePattern x' y'
binOp_pipe _ = programError IncorrectNumberOfArguments

-- | Binary $ operator
binOp_dollar :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)] 
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
binOp_dollar [x,y] = do
    pat <- toLazyPattern x
    ref <- case y of
        ReferenceId sym -> return $ LookupId sym
        ReferenceAggregate sym args -> return $ LookupAggregate sym args
        ReferenceInput -> return $ LookupInput
        ReferenceOutput -> return $ LookupOutput
        ReferencePunch -> return $ LookupPunch
        _ -> programError IllegalDataType
    return $ Just $ TempPatternData $ ImmediateAssignmentPattern pat ref
binOp_dollar _ = programError IncorrectNumberOfArguments

-- | Binary . operator
binOp_dot :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)]
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
binOp_dot [x,y] = do
    pat <- toLazyPattern x
    ref <- case y of
        ReferenceId sym -> return $ LookupId sym
        ReferenceKeyword sym -> return $ LookupKeyword sym
        ReferenceAggregate sym args -> return $ LookupAggregate sym args
        ReferenceInput -> return $ LookupInput
        ReferenceOutput -> return $ LookupOutput
        ReferencePunch -> return $ LookupPunch
        _ -> programError IllegalDataType
    return $ Just $ TempPatternData $ AssignmentPattern pat ref
binOp_dot _ = programError IncorrectNumberOfArguments

-- | Unary & operator
unOp_and :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)]
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
unOp_and [ReferenceId sym] = return $ Just $ ReferenceKeyword sym
unOp_and _ = programError UnknownKeyword

-- | Unary $ operator
unOp_dollar :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)]
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
unOp_dollar [Name (LookupId sym)] = return $ Just $ ReferenceId sym
unOp_dollar [x] = do
    sym <- toString x
    return $ Just $ ReferenceId sym
unOp_dollar _ = programError IncorrectNumberOfArguments

-- | Unary @ operator
unOp_at :: ( InterpreterShell m, LocalVariablesClass m )
        => [Data (ExprType m)]
        -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
unOp_at [Name l] = return $ Just $ TempPatternData $ HeadPattern l
unOp_at [ReferenceId sym] = return $ Just $ TempPatternData $ HeadPattern $ LookupId sym
unOp_at [ReferenceKeyword sym] = return $ Just $ TempPatternData $ HeadPattern $ LookupKeyword sym
unOp_at [ReferenceAggregate sym args ] = return $ Just $ TempPatternData $ HeadPattern $ LookupAggregate sym args
unOp_at [ReferenceInput] = return $ Just $ TempPatternData $ HeadPattern $ LookupInput
unOp_at [ReferenceOutput] = return $ Just $ TempPatternData $ HeadPattern $ LookupOutput
unOp_at [ReferencePunch] = return $ Just $ TempPatternData $ HeadPattern $ LookupPunch
unOp_at [ReferenceUserData k sym ix] = return $ Just $ TempPatternData $ HeadPattern $ LookupUserData k sym ix
unOp_at _ = programError IncorrectNumberOfArguments


-- | Initial value of &ALPHABET
keyword_alphabet :: Data expr
keyword_alphabet = StringData $ mkString ['A'..'Z']

-- | Initial value of &ABORT
keyword_abort :: Data expr
keyword_abort = TempPatternData AbortPattern

-- | Initial value of &ARB
keyword_arb :: Data expr
keyword_arb = TempPatternData ArbPattern

-- | Initial value of &BAL
keyword_bal :: Data expr
keyword_bal = TempPatternData BalPattern

-- | Initial value of &ERRTYPE
keyword_errtype :: Data expr
keyword_errtype = IntegerData 0

-- | Initial value of &FAIL
keyword_fail :: Data expr
keyword_fail = TempPatternData FailPattern

-- | Initial value of &FENCE
keyword_fence :: Data expr
keyword_fence = TempPatternData FencePattern

-- | Initial value of &FNCLEVEL
keyword_fnclevel :: Data expr
keyword_fnclevel = IntegerData 0

-- | Initial value of &LASTNO
keyword_lastno :: Data expr
keyword_lastno = IntegerData (-1)

-- | Initial value of &REM
keyword_rem :: Data expr
keyword_rem = TempPatternData $ RTabPattern $ EvaluatedThunk 0

-- | Initial value of &RTNTYPE
keyword_rtntype :: Data expr
keyword_rtntype = StringData nullString

-- | Initial value of &STCOUNT
keyword_stcount :: Data expr
keyword_stcount = IntegerData 0

-- | Initial value of &STFCOUNT
keyword_stfcount :: Data expr
keyword_stfcount = IntegerData 0

-- | Initial value of &STNO
keyword_stno :: Data expr
keyword_stno = IntegerData 0

-- | Initial value of &SUCCEED
keyword_succeed :: Data expr
keyword_succeed = TempPatternData SucceedPattern


-- | Initial value of &ABEND
keyword_abend = IntegerData 0

-- | Initial value of &ANCHOR
keyword_anchor = IntegerData 0

-- | Initial value of &CODE
keyword_code = IntegerData 0

-- | Initial value of &DUMP
keyword_dump = IntegerData 0

-- | Initial value of &ERRLIMIT
keyword_errlimit = IntegerData 0

-- | Initial value of &FTRACE
keyword_ftrace = IntegerData 0

-- | Initial value of &FULLSCAN
keyword_fullscan = IntegerData 0

-- | Initial value of &INPUT
keyword_input = IntegerData 1

-- | Initial value of &MAXLNGTH
keyword_maxlngth = IntegerData 5000

-- | Initial value of &OUTPUT
keyword_output = IntegerData 1

-- | Initial value of &STLIMIT
keyword_stlimit = IntegerData 50000

-- | Initial value of &TRACE
keyword_trace = IntegerData 0

-- | Initial value of &TRIM
keyword_trim = IntegerData 0
