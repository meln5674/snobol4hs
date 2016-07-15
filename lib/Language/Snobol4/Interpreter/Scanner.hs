{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.Interpreter.Scanner where

import Control.Monad.Trans

import Text.Parsec ( (<|>), ParsecT, runParserT )
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P


import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Internal.Types

newtype Scanner m a
    = Scanner
    { runScanner
        :: ParsecT String [(Lookup,Data)] (Evaluator m) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)
    
    
matchPat :: InterpreterShell m => Pattern -> Scanner m String
matchPat (AssignmentPattern p l) = do
    result <- matchPat p
    Scanner $ P.modifyState $ ((l,StringData result):)
    return result
matchPat (ImmediateAssignmentPattern p l) = do
    result <- matchPat p
    Scanner $ lift $ assign l $ StringData result
    return result
matchPat (LiteralPattern s) = Scanner $ P.string s
matchPat (AlternativePattern a b) = do
    Scanner $ (runScanner $ matchPat a) <|> (runScanner $ matchPat b)
matchPat (ConcatPattern a b) = do
    a' <- matchPat a 
    b' <- matchPat b
    return $ a' ++ b'
matchPat (LengthPattern len) = Scanner $ P.count len P.anyChar
matchPat EverythingPattern = Scanner $ P.many P.anyChar

posToIndex :: Int -> Int -> [String] -> Maybe Int
posToIndex 0 0 [] = Just 0
posToIndex _ _ [] = Nothing
posToIndex 0 column [line]
    | length line < column = Just column
    | otherwise = Nothing 
posToIndex _ _ [line] = Nothing
posToIndex row column (line:lines)
    | length line < column = do
        x <- posToIndex (row - 1) column lines
        return $ x + column + 1 -- NEWLINE LENGTH?
    | otherwise = Nothing


matchPatOuter :: InterpreterShell m
              => String 
              -> Pattern 
              -> Scanner m ScanResult
matchPatOuter s p = do
    startPos <- Scanner $ P.getPosition
    result <- matchPat p
    endPos <- Scanner $ P.getPosition
    endState <- Scanner $ P.getState
    let startColumn = P.sourceColumn startPos - 1
        startRow = P.sourceLine startPos - 1
        endColumn = P.sourceColumn endPos - 1
        endRow = P.sourceLine endPos - 1
        rows = lines s
        Just startIndex = posToIndex startRow startColumn rows
        Just endIndex = posToIndex endRow endColumn rows
        s' = take startIndex s ++ result ++ drop endIndex s
    return $ Scan (StringData s') endState

scanPattern :: InterpreterShell m 
            => String 
            -> Pattern 
            -> Evaluator m ScanResult
scanPattern s pat = do
    result <- runParserT (runScanner $ matchPatOuter s pat) [] "" s
    case result of
        Left _ -> return NoScan
        Right result -> return result
