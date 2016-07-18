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

import Prelude hiding (len, span, break, any, notany, toInteger)

import Text.Read (readMaybe)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Array (Array)
import qualified Data.Array as A

import Text.Parsec ( (<|>), runParserT, ParsecT )
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Internal.Types

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
    [ PrimitiveFunction "LEN"       len
    , PrimitiveFunction "SPAN"      span
    , PrimitiveFunction "BREAK"     break
    , PrimitiveFunction "ANY"       any
    , PrimitiveFunction "NOTANY"    notany
    , PrimitiveFunction "TAB"       rtab
    , PrimitiveFunction "RTAB"      rtab
    , PrimitiveFunction "ARBNO"     arbno
    , PrimitiveFunction "TABLE"     table
    , PrimitiveFunction "ARRAY"     array
    ]

-- | The length function, returns a pattern with matches the provided number
-- of characters
len :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
len (a:_) = do
    i <- toInteger a
    if i >= 0
        then return $ Just $ PatternData $ LengthPattern i
        else liftEval $ programError ProgramError
len [] = len [StringData ""]

-- | The span function, returns a pattern which matches the longest string
-- containing only the provided characters
span :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
span (a:_) = do
    s <- toString a
    case s of
        "" -> liftEval $ programError ProgramError
        _ -> return $ Just $ PatternData $ SpanPattern s
span [] = span [StringData ""]

-- | The break function, returns a pattern which matches the longest string
-- containing none of the provided characters
break :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
break (a:_) = do
    s <- toString a
    case s of
        "" -> liftEval $ programError ProgramError 
        _ -> return $ Just $ PatternData $ BreakPattern s
break [] = break [StringData ""]

-- | The any function, returns a pattern which matches any one of the provided
-- characters
any :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
any (a:_) = do
    cs <- toString a
    case cs of
        "" -> liftEval $ programError ProgramError 
        _ -> return $ Just $ PatternData $ AnyPattern cs
any [] = any [StringData ""]

-- | The notany function, returns a pattern which matches one character not
-- provided
notany :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
notany (a:_) = do
    cs <- toString a
    case cs of
        "" -> liftEval $ programError ProgramError 
        _ -> return $ Just $ PatternData $ AnyPattern cs
notany [] = notany [StringData ""]

-- | The tab function, returns a pattern which matches the null string if the
-- cursor is before the provided column, measured from the left
tab :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
tab (a:_) = do
    i <- toInteger a
    if i >= 0
        then return $ Just $ PatternData $ TabPattern i
        else liftEval $ programError ProgramError
tab [] = tab [StringData ""]

-- | The rtab function, returns a pattern which matches the null string if the
-- cursor is after the provided column, measured from the right
rtab :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
rtab (a:_) = do
    i <- toInteger a
    if i >= 0
        then return $ Just $ PatternData $ RTabPattern i
        else liftEval $ programError ProgramError
rtab [] = rtab [StringData ""]

-- | The arbno function, returns a pattern which matches an arbitrary number of
-- repetitions of the provided pattern
arbno :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
arbno (a:_) = do
    p <- toPattern a
    return $ Just $ PatternData $ ArbNoPattern p
arbno [] = arbno [StringData ""]

-- | The table function, returns an empty table
table :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
table (a:_) = do
    i <- toInteger a
    if i >= 0
        then return $ Just $ TableData $ M.empty
        else liftEval $ programError ProgramError
table _ = return $ Just $ TableData $ M.empty

-- | The dimension of an array
data Dimension
    = Range Int Int
    | Count Int

-- | A list of dimensions
type Dimensions = [Dimension]

-- | Parser which matches a positive or negative number
bound :: Monad m => ParsecT String u m Int
bound = do
    sign <- P.option "" $ P.string "-"
    digits <- P.many P.digit
    let str = sign ++ digits
        val = readMaybe str :: Maybe Int
    case val of
        Just val -> return val
        Nothing -> P.unexpected str

-- | Parser which matches two positive or negative numbers with a colon
-- separating them
rangeDimension :: Monad m => ParsecT String u m Dimension
rangeDimension = do
    l <- bound
    P.char ':'
    r <- bound
    return $ Range l r

-- | Parser which matches a positive number
countDimension :: Monad m => ParsecT String u m Dimension
countDimension = do
    str <- P.many P.digit
    let val = readMaybe str :: Maybe Int
    case val of
        Just val -> return $ Count val
        Nothing -> P.unexpected str

-- | Parser which matches either a range dimension or a count dimension
dimension :: Monad m => ParsecT String u m Dimension
dimension = P.try rangeDimension <|> countDimension

-- | Parser which matches a list of dimensions separated by commas
dimensions :: Monad m => ParsecT String u m Dimensions
dimensions = P.sepBy1 dimension (P.char ',')

-- | Construct an array with the provided dimensions and initial value
mkArray :: Dimensions -> Data -> Data
mkArray [] v = v
mkArray (Range l r:ds) v = ArrayData $ A.array (l,r) $ map (\x -> (x,v')) [l..r]
  where
    v' = mkArray ds v
mkArray (Count c:ds) v = mkArray (Range 0 c:ds) v

-- | The array function, creates an array with the provided dimensions and initial value
array :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
array [dimStr,item] = do
    str <- toString dimStr
    dims <- runParserT dimensions () "" str
    case dims of
        Left err -> return Nothing
        Right dims -> do
             return $ Just $ mkArray dims item
array [dimStr] = array [dimStr, StringData ""]
array _ = liftEval $ programError ProgramError
