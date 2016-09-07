{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    { sourcePath :: FilePath
    , targetPath :: FilePath
    }

parseArgs :: [String] -> Either String Args
parseArgs [] = Left "No source path specified"
parseArgs [path] = Right Args{ sourcePath = path, targetPath = "a.snobol4c" }
parseArgs [srcpath,"-o",tarpath] = Right Args { sourcePath = srcpath, targetPath = tarpath }
parseArgs _ = Left "Too many arguments"

doMain :: [String] -> ExceptT String IO ()
doMain args = do
    Args{sourcePath,targetPath} <- ExceptT $ return $ parseArgs args
    source <- lift $ readFile sourcePath
    ast <- ExceptT $ return $ either (Left . show) Right (parse source)
    (program,table) <- ExceptT $ return $ case simpleCompiler ast of
        CompileFailed errors -> Left 
                     $ intercalate "\n" 
                     $ flip map errors 
                     $ \(addr, err) -> show (getAddress addr) ++ ": " ++ show err
        CompileSucceeded x y -> Right (x,y)
    lift $ BS.writeFile targetPath $ encode (program,table)


main :: IO ()
main = do
    args <- getArgs
    void $ runExceptT $ catchE (doMain args) $ lift . putStrLn
