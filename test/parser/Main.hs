{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.Exit

import Data.Proxy

import Test.HUnit

import Language.Snobol4.Parser
import Language.Snobol4.Syntax.AST


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


test1 = simpleParseTest " V = 5\n" ast ""
  where
    ast = assignStmt "V" (LitExpr (Int 5))

test2 = simpleParseTest " W = 14 + (16 - 10)\n" ast ""
  where
    ast = assignStmt "W"
        $ BinaryExpr 
            (LitExpr (Int 14)) 
            Plus 
            (ParenExpr 
                (BinaryExpr 
                    (LitExpr (Int 16)) 
                    Minus 
                    (LitExpr (Int 10))
                )
            )

assignStmt name val = Stmt Nothing (Just (IdExpr name)) Nothing (Just val) Nothing

allTests = TestList
    [ test1
    , test2
    ]

main :: IO ()
main = do
    counts <- runTestTT allTests
    if errors counts /= 0 || failures counts /= 0
        then exitFailure
        else exitSuccess
