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


test_pg1_1 = simpleParseTest " V = 5\n" ast ""
  where
    ast = assignStmt Nothing (IdExpr "V") (LitExpr (Int 5)) Nothing

test_pg1_2 = simpleParseTest " W = 14 + (16 - 10)\n" ast ""
  where
    ast = assignStmt Nothing (IdExpr "W")
         (BinaryExpr 
            (LitExpr (Int 14)) 
            Plus 
            (ParenExpr 
                (BinaryExpr 
                    (LitExpr (Int 16)) 
                    Minus 
                    (LitExpr (Int 10))
                )
            )
         )
         Nothing

test_pg1_3 = simpleParseTest " V = \'DOG\'\n" ast ""
  where
    ast = assignStmt Nothing (IdExpr "V") (LitExpr (String "DOG")) Nothing

test_pg2_1 = simpleParseTest " RESULT = ANS_1\n" ast ""
  where
    ast = assignStmt Nothing (IdExpr "RESULT") (IdExpr "ANS_1") Nothing

test_pg2_2 = simpleParseTest " M = 4\n N = 5\n P = N * M / (N - 1)\n" ast ""
  where
    ast = Program
        [ assignStmt Nothing (IdExpr "M") (LitExpr (Int 4)) Nothing
        , assignStmt Nothing (IdExpr "N") (LitExpr (Int 5)) Nothing
        , assignStmt Nothing (IdExpr "P")
            (BinaryExpr
                (BinaryExpr
                    (IdExpr "N")
                    Star
                    (IdExpr "M")
                )
                Slash
                (ParenExpr
                    (BinaryExpr
                        (IdExpr "N")
                        Minus
                        (LitExpr (Int 1))
                    )
                )
            )
            Nothing
        ]

test_pg2_3 = simpleParseTest " Q2 = -P / -N\n" ast ""
  where
    ast = assignStmt Nothing (IdExpr "Q2")
        (BinaryExpr
            (PrefixExpr Minus (IdExpr "P"))
            Slash
            (PrefixExpr Minus (IdExpr "N"))
        )
        Nothing

test_pg2_4 = simpleParseTest " X = 2 ** 3 ** 2\n" ast ""
  where
    ast = assignStmt Nothing (IdExpr "X")
        (BinaryExpr
            (LitExpr (Int 2))
            DoubleStar
            (BinaryExpr
                (LitExpr (Int 3))
                DoubleStar
                (LitExpr (Int 2))
            )
        )
        Nothing

test_pg2_5 = simpleParseTest " X = 2 ** (3 ** 2)\n" ast ""
  where
    ast = assignStmt Nothing (IdExpr "X")
        (BinaryExpr
            (LitExpr (Int 2))
            DoubleStar
            (ParenExpr
                (BinaryExpr
                    (LitExpr (Int 3))
                    DoubleStar
                    (LitExpr (Int 2))
                )
            )
        )
        Nothing

test_pg3_1 = simpleParseTest " PI = 3.14159\n CIRUM = 2. * PI * 5.\n" ast ""
  where
    ast = Program
        [ assignStmt Nothing (IdExpr "PI") (LitExpr (Real 3.14159)) Nothing
        , assignStmt Nothing (IdExpr "CIRUM")
            (BinaryExpr
                (BinaryExpr
                    (LitExpr (Real 2.0))
                    Star
                    (IdExpr "PI")
                )
                Star
                (LitExpr (Real 5.0))
            )
            Nothing
        ]

test_pg3_2 = simpleParseTest " SUM = 16.4 + 2\n" ast ""
  where
    ast = assignStmt Nothing (IdExpr "SUM") 
        (BinaryExpr
            (LitExpr (Real 16.4))
            Plus
            (LitExpr (Int 2))
        )
        Nothing

test_pg3_3 = simpleParseTest " SCREAM = \'HELP\'\n" ast ""
  where
    ast = assignStmt Nothing (IdExpr "SCREAM") (LitExpr (String "HELP")) Nothing

test_pg3_4 = simpleParseTest code ast ""
  where
    code = " PLEA = \'HE SHOUTED, \"HELP.\"\'\n QUOTE = \'\"\'\n APOSTROPHE = \"\'\"\n"
    ast = Program
        [ assignStmt Nothing (IdExpr "PLEA") (LitExpr (String "HE SHOUTED, \"HELP.\"")) Nothing
        , assignStmt Nothing (IdExpr "QUOTE") (LitExpr (String "\"")) Nothing
        , assignStmt Nothing (IdExpr "APOSTROPHE") (LitExpr (String "\'")) Nothing
        ]

test_pg4_1 = simpleParseTest code ast ""
  where
    code = " NULL =\n"
    ast = assignStmt Nothing (IdExpr "NULL") NullExpr Nothing

test_pg4_2 = simpleParseTest code ast ""
  where
    code  = " Z = \'10\'\n X = 5 * -Z + \'10.6\'\n"
    ast = Program
        [ assignStmt Nothing (IdExpr "Z") (LitExpr (String "10")) Nothing
        , assignStmt Nothing (IdExpr "X")
            (BinaryExpr
                (BinaryExpr
                    (LitExpr (Int 5))
                    Star
                    (PrefixExpr Minus (IdExpr "Z"))
                )
                Plus
                (LitExpr (String "10.6"))
            )
            Nothing
        ]

test_pg4_3 = mkParseTest $ ExpectFailure
    { parseString = "1,253,465"
    , resultFormatter = show :: Expr -> String
    }

test_pg4_4 = mkParseTest $ ExpectFailure
    { parseString = ".364 E-03"
    , resultFormatter = show :: Expr -> String
    }

test_pg4_5 = simpleParseTest code ast ""
  where
    code = " TYPE = \'SEMI\'\n OBJECT = TYPE \'GROUP\'\n"
    ast = Program
        [ assignStmt Nothing (IdExpr "TYPE") (LitExpr (String "SEMI")) Nothing
        , assignStmt Nothing (IdExpr "OBJECT")
            (BinaryExpr 
                (IdExpr "TYPE") 
                Blank 
                (LitExpr (String "GROUP"))
            )
            Nothing
        ]

test_pg5_1 = simpleParseTest code ast ""
  where
    code = " FIRST = \'WINTER\'\n SECOND = \'SPRING\'\n TWO_SEASONS = FIRST \',\' SECOND\n"
    ast = Program
        [ assignStmt Nothing (IdExpr "FIRST") (LitExpr (String "WINTER")) Nothing
        , assignStmt Nothing (IdExpr "SECOND") (LitExpr (String "SPRING")) Nothing
        , assignStmt Nothing (IdExpr "TWO_SEASONS")
            (BinaryExpr
                (BinaryExpr
                    (IdExpr "FIRST")
                    Blank
                    (LitExpr (String ","))
                )
                Blank
                (IdExpr ("SECOND"))
            )
            Nothing
        ]
allTests = TestList
    [ test_pg1_1
    , test_pg1_2
    , test_pg1_3
    , test_pg2_1
    , test_pg2_2
    , test_pg2_3
    , test_pg2_4
    , test_pg2_5
    , test_pg3_1
    , test_pg3_2
    , test_pg3_3
    , test_pg3_4
    , test_pg4_1
    , test_pg4_2
    , test_pg4_3
    , test_pg4_4
    , test_pg4_5
    , test_pg5_1
    ]

main :: IO ()
main = do
    counts <- runTestTT allTests
    if errors counts /= 0 || failures counts /= 0
        then exitFailure
        else exitSuccess
