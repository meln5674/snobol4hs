{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import System.Console.Haskeline

import Language.Snobol4
import Language.Snobol4.Parser
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Shell.Console


newtype HaskelineShell a
    = HaskelineShell
    { runHaskelineShell
        :: StateT (String,String,String) (InputT IO) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance InterpreterShell HaskelineShell where
    input = HaskelineShell $ do
        i <- lift $ getInputLine "INPUT>>"
        let i' = maybe "" id i
        modify $ \(_,o,p) -> (i',o,p)
        return i'
    output o = HaskelineShell $ do
        lift $ outputStrLn o
        modify $ \(i,_,p) -> (i,o,p)
    punch p = HaskelineShell $ do
        lift $ outputStrLn p
        modify $ \(i,o,_) -> (i,o,p)
    lastOutput = HaskelineShell $ gets $ \(_,o,_) -> o
    lastPunch = HaskelineShell $ gets $ \(_,_,p) -> p

instance InterpreterShellRun HaskelineShell IO where
    start = HaskelineShell $ put ("","","")
    shell = runInputT defaultSettings 
          . flip evalStateT ("","","") 
          . runHaskelineShell

main :: IO ()
main = shell $ do
    st <- load []
    loopMain st

loopMain :: PausedInterpreter -> HaskelineShell ()
loopMain st = do
    i <- HaskelineShell $ lift $ getInputLine ">>"
    case i of
        Nothing -> loopMain st
        Just i -> do
            result <- parseStatementT (i ++ "\n")
            case result of
                Right stmt -> do
                    st' <- exec stmt st
                    loopMain st'
                Left err -> do
                    HaskelineShell $ lift $ outputStrLn $ "ERROR: " ++ show err
                    loopMain st
    return ()
    
    
