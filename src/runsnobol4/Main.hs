

module Main where

import System.Environment

import Language.Snobol4
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Shell.Console

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> doMain path
        [] -> putStrLn "Error: No path specified"
        _ -> putStrLn "Error: Multiple paths specified"

doMain :: String -> IO ()
doMain path = do
    parseResult <- parseFile path
    case parseResult of
        Right code -> do
            result <- shell $ (run code :: ConsoleShell ProgramResult)
            print result
        Left err -> print err