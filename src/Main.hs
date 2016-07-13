module Main where

import System.Environment

import Language.Snobol4

main :: IO ()
main = do
    [path] <- getArgs
    parseResult <- parseFile path
    case parseResult of
        Right code -> runProgram code >> return ()
        Left err -> print err
