{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.List
import qualified Data.Vector as V


import System.Exit

import Control.Monad.State

import Test.HUnit hiding (State)

import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Shell.Mock
import Language.Snobol4.Syntax.AST
import Language.Snobol4.Parser
import Language.Snobol4.VM
import Language.Snobol4.VM.Bytecode.Compiler.Simple

import Examples

{-
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
-}

assertMaybe :: Maybe String -> Assertion
assertMaybe = assertString . maybe "" id

compileMockTest :: Monad m => MockTest -> m (Either ParseError CompilerResult)
compileMockTest test = liftM (liftM simpleCompiler) $ parseT (testProgram test) 
    

runMock :: Monad m 
        => MockTest 
        -> CompiledProgram 
        -> SymbolTable 
        -> m (Maybe ProgramError, MockShellResults)
runMock test prog tbl = getMockResultsT $ shell $ do
    start
    mapM addInput (testInputs test)
    setDate (testDate test)
    setTime (testTime test)
    run prog tbl

makeMockTest :: MockTest -> Test
makeMockTest test = TestLabel (testLabel test) $ TestCase $ do
    compileResult <- compileMockTest test
    case compileResult of
        Left err -> assertString $ show err
        Right (CompileFailed errs) -> 
            assertString $ intercalate "\n" $ flip map errs $ \(addr,err) ->
                show addr ++ ": " ++ show err
        Right (CompileSucceeded prog tbl) -> do
            (runResult,shellResults) <- runMock test prog tbl
            case (runResult,test) of
                (Nothing,ExpectSuccess{successPredicate=pred}) ->
                    assertMaybe (pred shellResults)
                (Just err,ExpectFailure{failurePredicate=pred}) ->
                    assertMaybe (pred err shellResults)
                (Just err,ExpectSuccess{}) ->
                    assertString $ "Expected to succeed but failed with " ++ show err
                (Nothing,ExpectFailure{}) ->
                    assertString "Expected to fail but succeeded"

{-
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
-}

match :: (Eq a)
      => (MockShellResults -> a) 
      -> (a -> [String]) 
      -> String 
      -> MockShellResults 
      -> MockShellResults 
      -> Maybe [String]
match select format title expected actual = if (select expected /= select actual)
    then Just $
        ("Expected " ++ title ++ " to be:")
            : format (select expected)
        ++ "But got:"
            : format (select actual)
    else Nothing

matchInputs = match mockInputs (V.toList . fmap ("\t"++)) "remaining inputs"
matchOutputs = match mockOutputs (V.toList . fmap ("\t"++)) "outputs"
matchPunches = match mockPunches (V.toList . fmap ("\t"++)) "punches"
matchDate = match mockDate return "date"
matchTime = match mockTime (return . show) "time"

simplePredicate :: MockShellResults -> MockShellResults -> Maybe String
simplePredicate expected actual = fmap (intercalate "\n") $ msum $ map (($ (expected,actual)) . uncurry)
    [ matchInputs
    , matchOutputs
    , matchPunches
    , matchDate
    , matchTime
    ]
            
            

test_pg20 = ExpectSuccess
    { testLabel = "Page 20"
    , testProgram = ex_pg20_5
    , testInputs = [ "THE WORLD OF THE FUTURE WILL BE AN EVER MORE DEMANDING STRUGGLE AGAINST THE LIMITATIONS OF OUR INTELLIGENCE, ...."
                   , "N. WEINER"
                   ]
    , testDate = ""
    , testTime = 0
    , successPredicate = simplePredicate MockShellResults
        { mockInputs = V.empty
        , mockOutputs = V.fromList
            [ "THE WORLD OF THE FUTURE WILL BE AN EVER MORE DEMANDING STRUGGLE AGAINST THE LIMITATIONS OF OUR INTELLIGENCE, ...."
            , "N. WEINER"
            , ""
            , "A OCCURS 5 TIMES"
            , "B OCCURS 1 TIMES"
            , "C OCCURS 1 TIMES"
            , "D OCCURS 3 TIMES"
            , "E OCCURS 15 TIMES"
            , "F OCCURS 3 TIMES"
            , "G OCCURS 5 TIMES"
            , "H OCCURS 3 TIMES"
            , "I OCCURS 9 TIMES"
            , "L OCCURS 7 TIMES"
            , "M OCCURS 3 TIMES"
            , "N OCCURS 9 TIMES"
            , "O OCCURS 6 TIMES"
            , "R OCCURS 7 TIMES"
            , "S OCCURS 3 TIMES"
            , "T OCCURS 9 TIMES"
            , "U OCCURS 4 TIMES"
            , "V OCCURS 1 TIMES"
            , "W OCCURS 3 TIMES"
            ]
        , mockPunches = V.empty
        , mockDate = ""
        , mockTime = 0
        }
    }

test_pg36_1 = ExpectSuccess
    { testLabel = "Page 36"
    , testProgram = ex_pg36
    , testInputs = [ "THE BEADS ARE RED."
                   , "BRED AND BORED."
                   , "BEAUTY AND THE BEAST."
                   ]
    , testDate = ""
    , testTime = 0
    , successPredicate = simplePredicate MockShellResults
        { mockInputs = V.empty
        , mockOutputs = V.fromList
            [ "THE BEADS ARE RED."
            , "    ----"
            , ""
            , "BRED AND BORED."
            , " ---"
            , ""
            , "BEAUTY AND THE BEAST."
            , "P FAILED TO MATCH."
            , ""
            ]
        , mockPunches = V.empty
        , mockDate = ""
        , mockTime = 0
        }
    }

test_pg34 = ExpectSuccess
    { testLabel = "Page 34"
    , testProgram = ex_pg34
    , testInputs = []
    , testDate = ""
    , testTime = 0
    , successPredicate = simplePredicate MockShellResults
        { mockInputs = V.empty
        , mockOutputs = V.fromList
            [ "R"
            , "E"
            , "EA"
            , "D"
            ]
        , mockPunches = V.empty
        , mockDate = ""
        , mockTime = 0
        }
    }

test_pg37 = ExpectSuccess
    { testLabel = "Page 37"
    , testProgram = ex_pg37
    , testInputs =
        [ "1290 SEP. 27 CHINA, CHIHLI           100,000"
        , "1293 MAY  20 JAPAN, KAMARKURA         30,000"
        , "1531 JAN. 26 PORTUGAL, LIBSON         30,000"
        ]        
    , testDate = ""
    , testTime = 0
    , successPredicate = simplePredicate MockShellResults
        { mockInputs = V.empty
        , mockOutputs = V.fromList
            [ "SEP. 27, 1290   CHINA, CHIHLI           100,000"
            , "MAY  20, 1293   JAPAN, KAMARKURA         30,000"
            , "JAN. 26, 1531   PORTUGAL, LIBSON         30,000"
            ]        
        , mockPunches = V.empty
        , mockDate = ""
        , mockTime = 0
        }
    }


test_pg39 = ExpectSuccess
    { testLabel = "Page 39"
    , testProgram = ex_pg39
    , testInputs =
        [ "ACTINIUM     AC    89    227*      1899    DEBIERNE"
        , "ALUMINUM     AL    13     26.9815  1825    OERSTED"
        , "AMERICIUM    AM    95    243*      1944    SEABORG"
        , "ANTIMONY     SB    51    121.75    1450    VALENTINE"
        ]        
    , testDate = ""
    , testTime = 0
    , successPredicate = simplePredicate MockShellResults
        { mockInputs = V.empty
        , mockOutputs = V.empty
        , mockPunches = V.fromList
            [ "ACTINIUM:AC:89:227*:1899:DEBIERNE"
            , "ALUMINUM:AL:13:26.9815:1825:OERSTED"
            , "AMERICIUM:AM:95:243*:1944:SEABORG"
            , "ANTIMONY:SB:51:121.75:1450:VALENTINE"
            ]
        , mockDate = ""
        , mockTime = 0
        }
    }


test_pg42 = ExpectSuccess
    { testLabel = "Page 42"
    , testProgram = ex_pg42
    , testInputs =
        [ " 1 WILLIAM T. CAHILL         REP   COLLINGSWOOD"
        , " 2 THOMAS C. MCGRATH, JR.    DEM   MARGATE CITY"
        , " 3 JAMES J. HOWARD           DEM   WALL"
        , "14 DOMINICK V. DANIELS       DEM   JERSEY CITY"
        , "15 EDWARD J. PATTEN          DEM   PERTH AMBOY"
        ]
    , testDate = ""
    , testTime = 0
    , successPredicate = simplePredicate MockShellResults
        { mockInputs = V.empty
        , mockOutputs = V.fromList
            [ "WILLIAM T. CAHILL               COLLINGSWOOD"
            , "THOMAS C. MCGRATH, JR.          MARGATE CITY"
            , "JAMES J. HOWARD                         WALL"
            , "DOMINICK V. DANIELS              JERSEY CITY"
            , "EDWARD J. PATTEN                 PERTH AMBOY"
            ]
        , mockPunches = V.fromList
            [ "WILLIAM T. CAHILL               COLLINGSWOOD"
            , "THOMAS C. MCGRATH, JR.          MARGATE CITY"
            , "JAMES J. HOWARD                         WALL"
            , "DOMINICK V. DANIELS              JERSEY CITY"
            , "EDWARD J. PATTEN                 PERTH AMBOY"
            ]
        , mockDate = ""
        , mockTime = 0
        }
    }



test_pg46 = ExpectSuccess
    { testLabel = "Page 46"
    , testProgram = ex_pg46
    , testInputs =
        [ " XXXXXXXXX   XXXXXXX     XXXXX         X "
        , "             XXXXXXX     XXXXX         X "
        , " XXXXXXXXX   XXXXXXX     XXXXX         X "
        , " XXXXXXXXX               XXXXX         X "
        , " XXXXXXXXX   XXXXXXX     XXXXX         X "
        , " XXXXXXXXX   XXXXXXX     XXXXX           "
        , " XXXXXXXXX   XXXXXXX     XXXXX         X "
        , " XXXXXXXXX   XXXXXXX     XXXXX         XX"
        , " XXXXXXXXX   XXXXXXX     XXXXX         X "
        , "XXXXXXXXXX   XXXXXXX     XXXXX         X "
        , " XXXXXXXXX   XXXXXXX     XXXXX         X "
        , " XXXXXXXXX   XXXXXXX     XXXXX         X         X "
        ]
    , testDate = ""
    , testTime = 0
    , successPredicate = simplePredicate MockShellResults
        { mockInputs = V.empty
        , mockOutputs = V.fromList
            [ "CARDS WITH IMPROPER FORMAT ARE:"
            , ""
            , "#2               XXXXXXX     XXXXX         X "
            , "#4   XXXXXXXXX               XXXXX         X "
            , "#6   XXXXXXXXX   XXXXXXX     XXXXX           "
            , "#8   XXXXXXXXX   XXXXXXX     XXXXX         XX"
            , "#10 XXXXXXXXXX   XXXXXXX     XXXXX         X "
            , "#12  XXXXXXXXX   XXXXXXX     XXXXX         X         X "
            ]
        , mockPunches = V.empty
        , mockDate = ""
        , mockTime = 0
        }
    }

test_pg48 = ExpectSuccess
    { testLabel = "Page 48"
    , testProgram = ex_pg48
    , testInputs =
        [ "ALPHA"
        , "BETA"
        , "ABRACADABRA"
        , "ABSOLUTELY"
        , "AWFUL"
        , "ALBEIT"
        ]
    , testDate = ""
    , testTime = 0
    , successPredicate = simplePredicate MockShellResults
        { mockInputs = V.empty
        , mockOutputs = V.fromList
            [ "ACCEPTABLE WORDS ARE:"
            , ""
            , " BETA"
            , " ABSOLUTELY"
            , " ALBEIT"
            ]
        , mockPunches = V.empty
        , mockDate = ""
        , mockTime = 0
        }
    }

test_pg50 = ExpectSuccess
    { testLabel = "Page 50"
    , testProgram = ex_pg50
    , testInputs =
        [ "THESE TWO STRINGS ARE ALMOST ALIKE."
        , "THE TWO STRINGS AREN'T ALIKE."
        ]
    , testDate = ""
    , testTime = 0
    , successPredicate = simplePredicate MockShellResults
        { mockInputs = V.empty
        , mockOutputs = V.fromList
            [ "TWO, STRINGS, ALIKE, "
            ]
        , mockPunches = V.empty
        , mockDate = ""
        , mockTime = 0
        }
    }

test_pg51_1 = ExpectSuccess
    { testLabel = "Page 51 #1"
    , testProgram = ex_pg51_1
    , testInputs =
        [ "THESE TWO STRINGS ARE ALMOST ALIKE."
        , "THE TWO STRINGS AREN'T ALIKE."
        ]
    , testDate = ""
    , testTime = 0
    , successPredicate = simplePredicate MockShellResults
        { mockInputs = V.empty
        , mockOutputs = V.fromList
            [ "TWO, STRINGS, ALIKE, "
            ]
        , mockPunches = V.empty
        , mockDate = ""
        , mockTime = 0
        }
    }

test_pg51_2 = ExpectSuccess
    { testLabel = "Page 51 #2"
    , testProgram = ex_pg51_2
    , testInputs =
        [ "TWO STRINGS FOR TESTING"
        , "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        ]
    , testDate = ""
    , testTime = 0
    , successPredicate = simplePredicate MockShellResults
        { mockInputs = V.empty
        , mockOutputs = V.fromList
            [ "TWOSTRINGSFORTESTING"
            ]
        , mockPunches = V.empty
        , mockDate = ""
        , mockTime = 0
        }
    }


{-
test_pg = ExpectSuccess
    { testLabel = "Page "
    , testProgram = ex_pg
    , testInputs =
        [ 
        ]
    , testDate = ""
    , testTime = 0
    , successPredicate = simplePredicate MockShellResults
        { mockInputs = V.empty
        , mockOutputs = V.fromList
            [ 
            ]
        , mockPunches = V.empty
        , mockDate = ""
        , mockTime = 0
        }
    }
-}




allMockTests = 
    [ test_pg20
    , test_pg34
    , test_pg36_1
    , test_pg37
    , test_pg39
    , test_pg42
    , test_pg46
    , test_pg48
    , test_pg50
    , test_pg51_1
    , test_pg51_2
    ]

allTests = TestList $ map makeMockTest allMockTests


data MockTest
    = ExpectSuccess
    { testLabel :: String
    , testProgram :: String
    , testInputs :: [String]
    , testDate :: String
    , testTime :: Int
    , successPredicate :: MockShellResults -> Maybe String
    }
    | ExpectFailure
    { testLabel :: String
    , testProgram :: String
    , testInputs :: [String]
    , testDate :: String
    , testTime :: Int
    , failurePredicate :: ProgramError -> MockShellResults -> Maybe String
    }

main :: IO ()
main = do
    counts <- runTestTT allTests
    if errors counts /= 0 || failures counts /= 0
        then exitFailure
        else exitSuccess
