{-# LANGUAGE NamedFieldPuns #-}
import System.Environment

import qualified Data.ByteString as BS

import Data.Serialize

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Shell.Console

import Language.Snobol4.VM.Bytecode.Interpreter

data Args
    = Args
    { objectPath :: FilePath
    }

parseArgs :: [String] -> Either String Args
parseArgs [] = Left "No object file specified"
parseArgs [path] = Right Args{objectPath=path}
parseArgs _ = Left "Too many arguments"

doMain :: [String] -> ExceptT String IO ()
doMain args = do
    Args{objectPath} <- ExceptT $ return $ parseArgs args
    (program,table) <- ExceptT $ liftM decode $ BS.readFile objectPath
    result <- lift (shell (run program table :: ConsoleShell (Maybe ProgramError)) :: IO (Maybe ProgramError))
    case result of
        Nothing -> return ()
        Just err -> throwE $ show err
    

main :: IO ()
main = do
    args <- getArgs
    void $ runExceptT $ catchE (doMain args) (lift . putStrLn)
    
