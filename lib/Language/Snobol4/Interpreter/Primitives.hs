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
    , PrimitiveFunction "TABLE"     table
    , PrimitiveFunction "ARRAY"     array
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

table :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
table (a:_) = do
    IntegerData i <- toInteger a
    if i >= 0
        then return $ Just $ TableData $ M.empty
        else liftEval $ programError ProgramError
table _ = return $ Just $ TableData $ M.empty

data Dimension
    = Range Int Int
    | Count Int

type Dimensions = [Dimension]

bound :: Monad m => ParsecT String u m Int
bound = do
    sign <- P.option "" $ P.string "-"
    digits <- P.many P.digit
    let str = sign ++ digits
        val = readMaybe str :: Maybe Int
    case val of
        Just val -> return val
        Nothing -> P.unexpected str

rangeDimension :: Monad m => ParsecT String u m Dimension
rangeDimension = do
    l <- bound
    P.char ':'
    r <- bound
    return $ Range l r

countDimension :: Monad m => ParsecT String u m Dimension
countDimension = do
    str <- P.many P.digit
    let val = readMaybe str :: Maybe Int
    case val of
        Just val -> return $ Count val
        Nothing -> P.unexpected str

dimension :: Monad m => ParsecT String u m Dimension
dimension = P.try rangeDimension <|> countDimension

dimensions :: Monad m => ParsecT String u m Dimensions
dimensions = P.sepBy1 dimension (P.char ',')

mkArray :: Dimensions -> Data -> Data
mkArray [] v = v
mkArray (Range l r:ds) v = ArrayData $ A.array (l,r) $ map (\x -> (x,v')) [l..r]
  where
    v' = mkArray ds v
mkArray (Count c:ds) v = mkArray (Range 0 c:ds) v

array :: InterpreterShell m => [Data] -> Evaluator m (Maybe Data)
array [dimStr,item] = do
    StringData str <- toString dimStr
    dims <- runParserT dimensions () "" str
    case dims of
        Left err -> return Nothing
        Right dims -> do
             return $ Just $ mkArray dims item
array [dimStr] = array [dimStr, StringData ""]
array _ = liftEval $ programError ProgramError
