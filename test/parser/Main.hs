{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.Exit

import Data.Proxy

import Test.HUnit

import Language.Snobol4.Parser
import Language.Snobol4.Syntax.AST

import Examples
import ASTs

-- | Primitive parsing test function
-- `testLex x s f` succeeds if applying `f` to the result of parsing `s` returns
--      nothing. 
-- If it returns a string, it fails with that string as the error message.
testParse :: Parsable t 
          => String 
          -> (Either ParseError t -> Maybe String) 
          -> Test
testParse s f = TestCase $ case f $ parse s of
    Just err -> assertFailure $ show err
    Nothing -> assertBool "" True

-- | Primitive parsing test function
-- `expectSuccess s f g` succeeds if parsing `s` succeeds, and then if applying `f`
--      to the result returns nothing.
-- If applying `f` to the result returns a string, the test fails with that
--      message with a prefix explaining the parsing succeeded, but the predicate
--      failed.
-- If parsing fails, `g` is applied to the parse error to obtain the error
--      message, which is then prefixed explaining that parsing should have passed
--      but did not
expectSuccess :: Parsable t
              => String 
              -> (t -> Maybe String)
              -> (ParseError -> String) 
              -> Test
expectSuccess s onSuccess onFailure = testParse s $ \case
    Right x -> case onSuccess x of
        Just msg -> Just $ "Parsing succeeded, but predicate failed: " ++ msg
        Nothing -> Nothing
    Left err -> Just $ "Expected to succeed, but got : " ++ onFailure err

-- | Primitve parsing test function
-- `expectFailure s f` succeeds if parsing `s` fails. If parsing succeeds, the
--      test fails, and `f` is applied to the result produced to obtain
--      the error message, which is then prefixed to explain that parsing should
--      have failed, but did not.
expectFailure :: Parsable t
              => String 
              -> (t -> String) 
              -> Test
expectFailure s f = testParse s $ \case
    Right toks -> Just $ "Expected to fail, but got : " ++ f toks
    Left _ -> Nothing

data ParseTest t
    = ExpectSuccess 
    { parseString :: String
    , resultPredicate :: t -> Maybe String
    , errorFormatter :: ParseError -> String
    }
    | ExpectFailure
    { parseString :: String
    , resultFormatter :: t -> String
    }

mkParseTest (ExpectSuccess s f g) = expectSuccess s f g
mkParseTest (ExpectFailure s f) = expectFailure s f


simpleParseTest s t msg = mkParseTest $ ExpectSuccess
    { parseString = s
    , resultPredicate = \t' -> if t == t'
        then Nothing
        else Just $ "Expected " ++ msg ++ ", but got: " ++ show t'
    , errorFormatter = \err -> "Expected " ++ msg ++ ", but got: " ++ show err
    }


test_pg1_1 = simpleParseTest ex_pg1_1 ast_pg1_1 ""

test_pg1_2 = simpleParseTest ex_pg1_2 ast_pg1_2 ""

test_pg1_3 = simpleParseTest ex_pg1_3 ast_pg1_3 ""

test_pg2_1 = simpleParseTest ex_pg2_1 ast_pg2_1 ""

test_pg2_2 = simpleParseTest ex_pg2_2 ast_pg2_2 ""

test_pg2_3 = simpleParseTest ex_pg2_3 ast_pg2_3 ""

test_pg2_4 = simpleParseTest ex_pg2_4 ast_pg2_4 ""

test_pg2_5 = simpleParseTest ex_pg2_5 ast_pg2_5 ""

test_pg3_1 = simpleParseTest ex_pg3_1 ast_pg3_1 ""

test_pg3_2 = simpleParseTest ex_pg3_2 ast_pg3_2 ""

test_pg3_3 = simpleParseTest ex_pg3_3 ast_pg3_3 ""

test_pg3_4 = simpleParseTest ex_pg3_4 ast_pg3_4 ""

test_pg4_1 = simpleParseTest ex_pg4_1 ast_pg4_1 ""

test_pg4_2 = simpleParseTest ex_pg4_2 ast_pg4_2 ""

test_pg4_3 = mkParseTest $ ExpectFailure
    { parseString = ex_pg4_3
    , resultFormatter = show :: Expr -> String
    }

test_pg4_4 = mkParseTest $ ExpectFailure
    { parseString = ex_pg4_4
    , resultFormatter = show :: Expr -> String
    }

test_pg4_5 = simpleParseTest ex_pg4_5 ast_pg4_5 ""

test_pg5_1 = simpleParseTest ex_pg5_1 ast_pg5_1 ""

test_pg5_2 = simpleParseTest ex_pg5_2 ast_pg5_2 ""

test_pg5_3 = simpleParseTest ex_pg5_3 ast_pg5_3 ""

test_pg5_4 = simpleParseTest ex_pg5_4 ast_pg5_4 ""

test_pg5_5 = simpleParseTest ex_pg5_5 ast_pg5_5 ""

test_pg5_6 = simpleParseTest ex_pg5_6 ast_pg5_6 ""

test_pg5_7 = simpleParseTest ex_pg5_7 ast_pg5_7 ""

test_pg5_8 = simpleParseTest ex_pg5_8 ast_pg5_8 ""

test_pg5_9 = simpleParseTest ex_pg5_9 ast_pg5_9 ""

test_pg5_10 = simpleParseTest ex_pg5_10 ast_pg5_10 ""

test_pg6_1 = simpleParseTest ex_pg6_1 ast_pg6_1 ""

test_pg6_2 = simpleParseTest ex_pg6_2 ast_pg6_2 ""

test_pg6_3 = simpleParseTest ex_pg6_3 ast_pg6_3 ""

test_pg6_4 = simpleParseTest ex_pg6_4 ast_pg6_4 ""

test_pg6_5 = simpleParseTest ex_pg6_5 ast_pg6_5 ""

test_pg7_1 = simpleParseTest ex_pg7_1 ast_pg7_1 ""

test_pg7_2 = simpleParseTest ex_pg7_2 ast_pg7_2 ""

test_pg7_3 = simpleParseTest ex_pg7_3 ast_pg7_3 ""

test_pg7_4 = simpleParseTest ex_pg7_4 ast_pg7_4 ""

test_pg7_5 = simpleParseTest ex_pg7_5 ast_pg7_5 ""

test_pg7_6 = simpleParseTest ex_pg7_6 ast_pg7_6 ""

test_pg7_7 = simpleParseTest ex_pg7_7 ast_pg7_7 ""

test_pg7_8 = simpleParseTest ex_pg7_8 ast_pg7_8 ""

test_pg8_1 = simpleParseTest ex_pg8_1 ast_pg8_1 ""

test_pg8_2 = simpleParseTest ex_pg8_2 ast_pg8_2 ""

test_pg8_3 = simpleParseTest ex_pg8_3 ast_pg8_3 ""

test_pg8_4 = simpleParseTest ex_pg8_4 ast_pg8_4 ""

test_pg8_5 = simpleParseTest ex_pg8_5 ast_pg8_5 ""

test_pg8_6 = simpleParseTest ex_pg8_6 ast_pg8_6 ""

test_pg8_7 = simpleParseTest ex_pg8_7 ast_pg8_7 ""

test_pg8_8 = simpleParseTest ex_pg8_8 ast_pg8_8 ""

test_pg9_1 = simpleParseTest ex_pg9_1 ast_pg9_1 ""

test_pg9_2 = simpleParseTest ex_pg9_2 ast_pg9_2 ""

test_pg9_3 = simpleParseTest ex_pg9_3 ast_pg9_3 ""

test_pg9_4 = simpleParseTest ex_pg9_4 ast_pg9_4 ""

test_pg9_5 = simpleParseTest ex_pg9_5 ast_pg9_5 ""

test_pg9_6 = simpleParseTest ex_pg9_6 ast_pg9_6 ""

test_pg9_7 = simpleParseTest ex_pg9_7 ast_pg9_7 ""

test_pg10_1 = simpleParseTest ex_pg10_1 ast_pg10_1 ""

test_pg10_2 = simpleParseTest ex_pg10_2 ast_pg10_2 ""

test_pg10_3 = simpleParseTest ex_pg10_3 ast_pg10_3 ""


allTests = TestList
    [ TestLabel "Page 1, #1" test_pg1_1
    , TestLabel "Page 1, #2" test_pg1_2
    , TestLabel "Page 1, #3" test_pg1_3
    , TestLabel "Page 2, #1" test_pg2_1
    , TestLabel "Page 2, #2" test_pg2_2
    , TestLabel "Page 2, #3" test_pg2_3
    , TestLabel "Page 2, #4" test_pg2_4
    , TestLabel "Page 2, #5" test_pg2_5
    , TestLabel "Page 3, #1" test_pg3_1
    , TestLabel "Page 3, #2" test_pg3_2
    , TestLabel "Page 3, #3" test_pg3_3
    , TestLabel "Page 3, #4" test_pg3_4
    , TestLabel "Page 4, #1" test_pg4_1
    , TestLabel "Page 4, #2" test_pg4_2
    , TestLabel "Page 4, #3" test_pg4_3
    , TestLabel "Page 4, #4" test_pg4_4
    , TestLabel "Page 4, #5" test_pg4_5
    , TestLabel "Page 5, #1" test_pg5_1
    , TestLabel "Page 5, #2" test_pg5_2
    , TestLabel "Page 5, #3" test_pg5_3
    , TestLabel "Page 5, #4" test_pg5_4
    , TestLabel "Page 5, #5" test_pg5_5
    , TestLabel "Page 5, #6" test_pg5_6
    , TestLabel "Page 5, #7" test_pg5_7
    , TestLabel "Page 5, #8" test_pg5_8
    , TestLabel "Page 5, #9" test_pg5_9
    , TestLabel "Page 5, #10" test_pg5_10
    , TestLabel "Page 6, #1" test_pg6_1
    , TestLabel "Page 6, #2" test_pg6_2
    , TestLabel "Page 6, #3" test_pg6_3
    , TestLabel "Page 6, #4" test_pg6_4
    , TestLabel "Page 6, #5" test_pg6_5
    , TestLabel "Page 7, #1" test_pg7_1
    , TestLabel "Page 7, #2" test_pg7_2
    , TestLabel "Page 7, #3" test_pg7_3
    , TestLabel "Page 7, #4" test_pg7_4
    , TestLabel "Page 7, #5" test_pg7_5
    , TestLabel "Page 7, #6" test_pg7_6
    , TestLabel "Page 7, #7" test_pg7_7
    , TestLabel "Page 7, #8" test_pg7_8
    , TestLabel "Page 8, #1" test_pg8_1
    , TestLabel "Page 8, #2" test_pg8_2
    , TestLabel "Page 8, #3" test_pg8_3
    , TestLabel "Page 8, #4" test_pg8_4
    , TestLabel "Page 8, #5" test_pg8_5
    , TestLabel "Page 8, #6" test_pg8_6
    , TestLabel "Page 8, #7" test_pg8_7
    , TestLabel "Page 9, #1" test_pg9_1
    , TestLabel "Page 9, #2" test_pg9_2
    , TestLabel "Page 9, #3" test_pg9_3
    , TestLabel "Page 9, #4" test_pg9_4
    , TestLabel "Page 9, #5" test_pg9_5
    , TestLabel "Page 9, #6" test_pg9_6
    , TestLabel "Page 9, #7" test_pg9_7
    , TestLabel "Page 10, #1" test_pg10_1
    , TestLabel "Page 10, #2" test_pg10_2
    , TestLabel "Page 10, #3" test_pg10_3
    ]

main :: IO ()
main = do
    counts <- runTestTT allTests
    if errors counts /= 0 || failures counts /= 0
        then exitFailure
        else exitSuccess
