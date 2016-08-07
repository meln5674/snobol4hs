{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit

import qualified Data.Map as M

import Control.Monad.State

import Test.HUnit hiding (State, Label)
import qualified Test.HUnit as HUnit

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Primitives
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Shell.Console
import Language.Snobol4.Interpreter.Internal.StateMachine
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Functions
import Language.Snobol4.Interpreter.Internal.StateMachine.Labels
import Language.Snobol4.Interpreter.Internal.StateMachine.Run

test_define = TestLabel "define" $ TestCase $ do
    let doTest :: ConsoleShell (Either ProgramError (Either EvalStop (Maybe (Function ConsoleShell))))
        doTest = interpret emptyState $ unliftEval $ do
            liftEval $ putLabels $ M.fromList [("TEST.INIT", Label $ Address 0)]
            define [ StringData "TEST(A)B", StringData "TEST.INIT" ]
            liftEval $ funcLookup "TEST"
    result <- shell doTest :: IO (Either ProgramError (Either EvalStop (Maybe (Function ConsoleShell))))
    let expected = Right $ Right $ Just $ UserFunction "TEST" ["A"] ["B"] $ Address 0
    assertEqual "" expected result

allTests = TestList [test_define]

main :: IO ()
main = do
    counts <- runTestTT allTests
    if errors counts /= 0 || failures counts /= 0
        then exitFailure
        else exitSuccess

