module Main where

import Control.Monad.Trans

import System.Console.Haskeline

import Language.Snobol4
import Language.Snobol4.Interpreter.Shell

import Shell

-- | Wrapper around outputStrLn
outputStrLn' :: String -> HaskelineShell ()
outputStrLn' = HaskelineShell . lift . outputStrLn

-- | Wrapper around getInputLine
getInputLine' :: String -> HaskelineShell (Maybe String)
getInputLine' = HaskelineShell . lift . getInputLine


-- | Get the string to print for data from the interpreter
showData :: Data -> String
showData (StringData s) = show s ++ " :: STRING"
showData (IntegerData i) = show i ++ " :: INTEGER"
showData (RealData r) = show r ++ " :: REAL"
showData (PatternData _) = "[PATTERN]"
showData (ArrayData _) = "[ARRAY]"
showData (TableData _) = "[TABLE]"
showData (Name _) = "[NAME]"

-- | Main loop
loopMain :: PausedInterpreter HaskelineShell -> HaskelineShell ()
loopMain st = do
    inputResult <- getInputLine' ">>"
    case inputResult of
        Nothing -> loopMain st
        Just inputLine -> do
            result <- parseT (inputLine ++ "\n")
            case result of
                Right stmt -> do
                    (st', execResult) <- exec stmt st
                    let toOutput = showData <$> execResult
                    case toOutput of
                        Just str -> outputStrLn' str
                        Nothing -> return ()
                    case isTerminated st' of
                        Just err -> do
                            outputStrLn' $ "ERROR: " ++ show err
                            loopMain st
                        Nothing -> loopMain st'
                Left err -> do
                    outputStrLn' $ "ERROR: " ++ show err
                    loopMain st
    return ()
    
-- | Entry Point    
main :: IO ()
main = shell $ do
    start
    st <- load $ Program []
    loopMain st

