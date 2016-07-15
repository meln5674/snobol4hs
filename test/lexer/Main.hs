{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding (lex)

import System.Exit

import Test.HUnit

import Language.Snobol4.Lexer

-- | Primitive lexing test function
-- `testLex s f` succeeds if applying `f` to the result of lexing `s` returns
--      nothing.
-- If it returns a string, it fails with that string as the error message.
testLex :: String -> (Either ParseError [Located Token SourcePos] -> Maybe String) -> Test
testLex s f = TestCase $ case f $ lex s of
    Just err -> assertFailure $ show err
    Nothing -> assertBool "" True

-- | Primitive lexing test function
-- `expectSuccess s f g` succeeds if lexing `s` succeeds, and then if applying `f`
--      to the list of tokens returns nothing.
-- If applying `f` to the tokens returns a string, the test fails with that
--      message with a prefix explaining the lexing succeeded, but the predicate
--      failed.
-- If lexing fails, `g` is applied to the parse error to obtain the error
--      message, which is then prefixed explaining that lexing should have passed
--      but did not
expectSuccess :: String 
              -> ([Located Token SourcePos] -> Maybe String)
              -> (ParseError -> String) 
              -> Test
expectSuccess s onSuccess onFailure = testLex s $ \case
    Right toks -> case onSuccess toks of
        Just msg -> Just $ "Lexing succeeded, but predicate failed: " ++ msg
        Nothing -> Nothing
    Left err -> Just $ "Expected to succeed, but got : " ++ onFailure err

-- | Primitve lexing test function
-- `expectFailure s f` succeeds if lexing `s` fails. If lexing succeeds, the
--      test fails, and `f` is applied to the list of tokens produced to obtain
--      the error message, which is then prefixed to explain that lexing should
--      have failed, but did not.
expectFailure :: String -> ([Located Token SourcePos] -> String) -> Test
expectFailure s f = testLex s $ \case
    Right toks -> Just $ "Expected to fail, but got : " ++ f toks
    Left _ -> Nothing

data LexTest
    = ExpectSuccess 
    { lexString :: String
    , tokenPredicate :: [Located Token SourcePos] -> Maybe String
    , errorFormatter :: ParseError -> String
    }
    | ExpectFailure
    { lexString :: String
    , tokenErrorFormatter :: [Located Token SourcePos] -> String
    }

mkLexTest :: LexTest -> Test
mkLexTest (ExpectSuccess s f g) = expectSuccess s f g
mkLexTest (ExpectFailure s f) = expectFailure s f

checkTokenList :: [Token] -> [Located Token SourcePos] -> Bool
checkTokenList [] [] = True
checkTokenList (t:ts) ((Located t' _):ls) | t == t' = True
checkTokenList _ _ = False

simpleLexTest s ts msg = mkLexTest $ ExpectSuccess
    { lexString = s
    , tokenPredicate = \ts' -> if checkTokenList ts ts'
        then Nothing
        else Just $ "Expected " ++ msg ++ ", but got: " ++ show ts'
    , errorFormatter = \err -> "Expected " ++ msg ++ ", but got: " ++ show err
    }

test_LParen         = simpleLexTest "("     [LParen]        "a left parenthesis" 
test_RParen         = simpleLexTest ")"     [RParen]        "a right parenthesis" 
test_LAngle         = simpleLexTest "<"     [LAngle]        "a left angle bracket" 
test_RAngle         = simpleLexTest ">"     [RAngle]        "a right angle bracket" 
test_Exponentiate   = simpleLexTest "**"    [Exponentiate]  "an exponentiate"
test_Not            = simpleLexTest "~"     [Operator "~"]  "a not"
test_Question       = simpleLexTest "?"     [Operator "?"]  "a question"
test_Dollar         = simpleLexTest "$"     [Operator "$"]  "a dollar"
test_Dot            = simpleLexTest "."     [Operator "."]  "a dot"
test_Bang           = simpleLexTest "!"     [Operator "!"]  "a bang"
test_Percent        = simpleLexTest "%"     [Operator "%"]  "a percent"
test_Star           = simpleLexTest "*"     [Operator "*"]  "a star"
test_Slash          = simpleLexTest "/"     [Operator "/"]  "a slash"
test_Hash           = simpleLexTest "#"     [Operator "#"]  "a hash"
test_Plus           = simpleLexTest "+"     [Operator "+"]  "a plus"
test_Minus          = simpleLexTest "-"     [Operator "-"]  "a minus"
test_At             = simpleLexTest "@"     [Operator "@"]  "an at"
test_Pipe           = simpleLexTest "|"     [Operator "|"]  "a pipe"
test_And            = simpleLexTest "&"     [Operator "&"]  "an and"



test_ = simpleLexTest "" [] ""

allTests = TestList 
    [ test_LParen
    , test_RParen
    , test_LAngle
    , test_RAngle
    , test_Exponentiate
    , test_Not
    , test_Question
    , test_Dollar
    , test_Dot
    , test_Bang
    , test_Percent
    , test_Star
    , test_Slash
    , test_Hash
    , test_Plus
    , test_At
    , test_Pipe
    , test_And
    ]


main :: IO ()
main = do
    counts <- runTestTT allTests
    if errors counts /= 0 || failures counts /= 0
        then exitFailure
        else exitSuccess
