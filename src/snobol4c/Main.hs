module Main where

import System.Environment

import Language.Snobol4
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Shell.Console


main :: IO ()
main = do
    [path] <- getArgs
    parseResult <- parseFile path
    case parseResult of
        Right code -> do
            result <- shell $ (run code :: ConsoleShell ProgramError)
            print result
        Left err -> print err
