{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import System.Environment

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List

import Numeric

import qualified Data.ByteString as BS

import Data.Serialize

import System.Console.Haskeline

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import Language.Snobol4.Parser

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Shell.Console
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Types

import Language.Snobol4.VM.Bytecode.Interpreter
import Language.Snobol4.VM.Bytecode.Interpreter.StackMachine.Internal (StackMachine(..))

import Language.Snobol4.VM.Bytecode
import Language.Snobol4.Syntax.AST hiding (getProgram)

deriving instance MonadException ConsoleShell
deriving instance MonadException m => MonadException (Interpreter m)
deriving instance MonadException m => MonadException (StackMachine expr m)


data DebugShellState = DebugShellState
    { inputs :: [String]
    , lastInput :: String
    , outputs :: [String]
    , punches :: [String]
    } 
newtype DebugShell a = DebugShell { runDebugShell :: StateT DebugShellState ConsoleShell a }
   deriving (Functor, Applicative, Monad, MonadIO, MonadException )

getLastInput :: DebugShell String
getLastInput = DebugShell $ gets lastInput

putLastInput :: String -> DebugShell ()
putLastInput x = DebugShell $ modify $ \st -> st { lastInput = x }

getInputs :: DebugShell [String]
getInputs = DebugShell $ gets inputs

putInputs :: [String] -> DebugShell ()
putInputs xs = DebugShell $ modify $ \st -> st { inputs = xs }

modifyInputs :: ([String] -> [String]) -> DebugShell ()
modifyInputs f = DebugShell $ modify $ \st -> st { inputs = f $ inputs st }

getOutputs :: DebugShell [String]
getOutputs = DebugShell $ gets outputs

putOutputs :: [String] -> DebugShell ()
putOutputs xs = DebugShell $ modify $ \st -> st { outputs = xs }

modifyOutputs :: ([String] -> [String]) -> DebugShell ()
modifyOutputs f = DebugShell $ modify $ \st -> st { outputs = f $ outputs st }

getPunches :: DebugShell [String]
getPunches = DebugShell $ gets punches

putPunches :: [String] -> DebugShell ()
putPunches xs = DebugShell $ modify $ \st -> st { punches = xs }

modifyPunches :: ([String] -> [String]) -> DebugShell ()
modifyPunches f = DebugShell $ modify $ \st -> st { punches = f $ punches st }

emptyDebugState = DebugShellState [] "" [] []

instance InterpreterShell DebugShell where
    input = do
        getInputs >>= \case
            [] -> return Nothing
            (x:xs) -> do
                putInputs xs
                putLastInput x
                return $ Just x
    output x = modifyOutputs (x:)
    lastOutput = getOutputs >>= \case
        [] -> return ""
        (x:_) -> return x
    punch x = modifyPunches (x:)
    lastPunch = getPunches >>= \case
        [] -> return ""
        (x:_) -> return x
    date = DebugShell $ lift date
    time = DebugShell $ lift time

instance InterpreterShellRun DebugShell where
    type BaseMonad DebugShell = IO
    start = DebugShell $ lift start
    shell = shell . flip evalStateT emptyDebugState . runDebugShell
        


instance (MonadException m) => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                    in fmap runExceptT $ f run'

data Args
    = Args
    { path :: FilePath
    }

parseArgs :: [String] -> Either String Args
parseArgs [] = Left "No path specified"
parseArgs [x] = Right $ Args x
parseArgs _ = Left "Too many arguments"

executeCmd :: String -> InputT (Interpreter DebugShell) Bool
executeCmd "" = return False
executeCmd "quit" = return True
executeCmd "step" = do
    lift $ step
    prog <- lift $ liftM getCompiledProgram $ getProgram
    pc <- lift $ liftM (unmkInteger . getAddress) $ getProgramCounter
    flip V.imapM prog $ \ix inst -> do
        when (pc - 5 < ix && ix < pc + 5) $ do
            if ix == pc
                then outputStr ">>> "
                else outputStr "    "
            outputStrLn $ "0x" ++ (showHex ix $ ": " ++ show inst)
    return False
executeCmd "program" = do
    prog <- lift $ liftM getCompiledProgram $ getProgram
    pc <- lift $ liftM (unmkInteger . getAddress) $ getProgramCounter
    flip V.imapM prog $ \ix inst -> do
        when (pc - 5 < ix && ix < pc + 5) $ do
            if ix == pc
                then outputStr ">>> "
                else outputStr "    "
            outputStrLn $ "0x" ++ (showHex ix $ ": " ++ show inst)
    return False
executeCmd "stack" = do
    stack <- lift getStack
    frameStart <- lift getCallStackFrameStart
    forM_ (zip [0..] stack) $ \(i,inst) -> do
        outputStr $ show inst
        when (i == frameStart) $ outputStr "------"
        outputStrLn ""
    return False
executeCmd "input" = do
    inputLine <- getInputLine ">>>"
    case inputLine of
        Just line -> lift $ lift $ lift $ modifyInputs (line:)
        Nothing -> return ()
    return False
executeCmd "inputs" = do
    inputLines <- lift $ lift $ lift $ liftM reverse getInputs
    mapM outputStrLn inputLines
    return False
executeCmd "outputs" = do
    outputLines <- lift $ lift $ lift $ liftM reverse getOutputs
    mapM outputStrLn outputLines
    return False
executeCmd "punches" = do
    punchLines <- lift $ lift $ lift $ liftM reverse getPunches
    mapM outputStrLn punchLines
    return False
executeCmd _ = do
    outputStrLn "Unknown command"
    return False


mainLoop :: InputT (Interpreter DebugShell) ()
mainLoop = do
    userInput <- getInputLine ">"
    case userInput of
        Nothing -> return ()
        Just cmdLine -> do
            quit <- executeCmd cmdLine
            if quit
                then return ()
                else mainLoop
                
    

doMain :: [String] -> ExceptT String IO ()
doMain args = do
    Args{path} <- ExceptT $ return $ parseArgs args
    (program,table) <- ExceptT $ liftM decode $ BS.readFile path
    let _ = program :: CompiledProgram
        _ = table :: SymbolTable
    result <- lift $ shell $ do
        start
        runVM $ runInputT defaultSettings $ do
            outputStrLn "Loading program"
            lift $ initVM program table
            outputStrLn "Program loaded"
            mainLoop
    case result of
        Left err -> throwE $ show err
        Right x -> return x

main :: IO ()
main = do
    args <- getArgs
    void $ runExceptT $ catchE (doMain args) (lift . putStrLn)


