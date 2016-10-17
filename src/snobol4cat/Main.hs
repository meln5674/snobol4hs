{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import System.Environment

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List

import qualified Data.ByteString as BS

import Data.Serialize

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Language.Snobol4.Parser

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Types

import Language.Snobol4.VM.Bytecode.Compiler
import Language.Snobol4.VM.Bytecode.Compiler.Simple

import Language.Snobol4.VM.Bytecode
import Language.Snobol4.Syntax.AST

data Args
    = Args
    { path :: FilePath
    }

parseArgs :: [String] -> Either String Args
parseArgs [] = Left "No path specified"
parseArgs [x] = Right $ Args x
parseArgs _ = Left "Too many arguments"

doMain :: [String] -> ExceptT String IO ()
doMain args = do
    Args{path} <- ExceptT $ return $ parseArgs args
    (program,table) <- ExceptT $ liftM decode $ BS.readFile path
    let _ = program :: CompiledProgram
        _ = table :: SymbolTable
    lift $ mapM_ print $ getCompiledProgram program
    lift $ print table

main :: IO ()
main = do
    args <- getArgs
    void $ runExceptT $ catchE (doMain args) (lift . putStrLn)

