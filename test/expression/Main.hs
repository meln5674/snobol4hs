module Main where

import System.Exit

import Test.HUnit

allTests = TestList []

main :: IO ()
main = do
    counts <- runTestTT allTests
    if errors counts /= 0 || failures counts /= 0
        then exitFailure
        else exitSuccess
