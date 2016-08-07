{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import System.Exit

import Control.Monad.State

import Test.HUnit hiding (State)

import Language.Snobol4.Interpreter.Shell (InterpreterShell, InterpreterShellRun)
import qualified Language.Snobol4.Interpreter.Shell as Shell
import Language.Snobol4.Interpreter
import Language.Snobol4.Syntax.AST
import Language.Snobol4.Parser
import Language.Snobol4.Interpreter.Internal.Types

data MockShellState = MockShellState
    { lastOutput :: String
    , lastPunch :: String
    , totalOutput :: [String]
    , totalPunch :: [String]
    , totalInput :: [String]
    }

emptyState = MockShellState "" "" [] [] []

newtype MockShell a = MockShell
    { runMockShell
        :: State MockShellState a
    }
  deriving (Functor, Applicative, Monad)

newtype MockShellBase a = MockShellBase 
    { getMockShellBase
        :: (a,[String],[String],Variables)
    }

data MockShellResult = MockShellResult
    (Either ProgramError (Maybe Data))
    [String]
    [String]
    Variables

data MockShellSuccess = MockShellSuccess (Maybe Data) [String] [String] Variables

data MockShellFailure = MockShellFailure ProgramError

getResult :: MockShellResult -> Maybe (Maybe Data)
getResult (MockShellResult (Right x) _ _ _) = Just x
getResult _ = Nothing

getError :: MockShellResult -> Maybe ProgramError
getError (MockShellResult (Left err) _ _ _) = Just err
getError _ = Nothing

getSuccess :: MockShellResult -> Maybe MockShellSuccess
getSuccess (MockShellResult (Right x) outputs punches vars) = Just $ MockShellSuccess x outputs punches vars
getSuccess _ = Nothing

getFailure :: MockShellResult -> Maybe MockShellFailure
getFailure (MockShellResult (Left err) _ _ _) = Just $ MockShellFailure err
getFailure _ = Nothing

instance InterpreterShell MockShell where
    input = MockShell $ do
        inputs <- gets totalInput
        case inputs of
            [] -> undefined -- FIX THIS
            (i:is) -> do
                modify $ \st -> st {totalInput = is}
                return i
    lastOutput = MockShell $ gets lastOutput
    lastPunch = MockShell $ gets lastPunch
    output s = MockShell $ modify $ \st -> st {totalOutput = s : totalOutput st}
    punch s = MockShell $ modify $ \st -> st {totalPunch = s : totalOutput st}
        
instance InterpreterShellRun MockShell MockShellBase where
    start = return ()
    shell m = MockShellBase (x,reverse $ totalOutput st, reverse $ totalPunch st,vars)
      where
        ((x,vars),st) = flip runState emptyState $ runMockShell $ m




runMock :: [String] -> Program -> MockShellResult
runMock inputLines code = MockShellResult x outputLines punchLines
  where
    MockShellBase (x,outputLines,punchLines) = Shell.shell $ do
        MockShell $ modify $ \st -> st { totalInput = reverse inputLines}
        run' code


testProgram :: String -> [String] -> (MockShellResult -> Maybe String) -> Test
testProgram toParse toInput f = TestCase $ case parse toParse of
    Right code -> case f $ runMock toInput code of
        Just msg -> assertFailure msg
        Nothing -> assertBool "" True
    Left err -> assertFailure $ "Failed to parse source: " ++ show err


expectSuccess :: String -> [String] -> (MockShellSuccess -> Maybe String) -> Test
expectSuccess toParse toInput f = testProgram toParse toInput $ \result ->
    case getSuccess result of
        Just s -> f s
        Nothing -> let Just (MockShellFailure err) = getFailure result
                   in  Just $ "Expected to succeed, but got: " ++ show err

expectFailure :: String -> [String] -> Test
expectFailure toParse toInput = testProgram toParse toInput $ \result ->
    case getFailure result of
        Just err -> Nothing
        Nothing -> Just $ "Expected to fail, but did not"


allTests = TestList []

main :: IO ()
main = do
    counts <- runTestTT allTests
    if errors counts /= 0 || failures counts /= 0
        then exitFailure
        else exitSuccess
