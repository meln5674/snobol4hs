{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module          : Language.Snobol4.Interpreter
Description     : The SNOBOL4 Interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portatibility   : Unknown

-}

module Language.Snobol4.Interpreter 
    ( step
    , exec
    , evalExpr
    , interpret
    , run
    , load
    , runProgram
    ) where

import Prelude hiding (toInteger)

import Text.Read hiding (lift, String, step, get)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import Language.Snobol4.Syntax.AST

import Language.Snobol4.Interpreter.Types

newtype Evaluator m a 
    = Evaluator
    { runEvaluator
        :: ExceptT ProgramError (ExceptT EvalStop (StateT ProgramState m)) a
    
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans Evaluator where
    lift m = Evaluator $ lift $ lift $ lift $ m

newtype Interpreter m a
    = Interpreter
    { runInterpreter
        :: ExceptT ProgramError (StateT ProgramState m) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans Interpreter where
    lift m = Interpreter $ lift $ lift $ m

liftEval :: Monad m => Interpreter m a -> Evaluator m a
liftEval = Evaluator . ExceptT . lift . runExceptT . runInterpreter

unliftEval :: Monad m => Evaluator m a -> Interpreter m (Either EvalStop a)
unliftEval (Evaluator m) = do
    x <- Interpreter $ lift $ runExceptT $ runExceptT $ m
    case x of
        Left stop -> return $ Left stop
        Right (Left err) -> programError err
        Right (Right val) -> return $ Right val

-- | Terminate the program with an error
programError :: Monad m => ProgramError -> Interpreter m a
programError = Interpreter . throwE

-- | Fail the evaluation
failEvaluation :: Monad m => Evaluator m a
failEvaluation = Evaluator
               $ lift 
               $ throwE EvalFailed

-- | Complete the evaluation
finishEvaluation :: Monad m => Evaluator m a
finishEvaluation = Evaluator
                 $ lift 
                 $ throwE EvalSuccess

getProgramState :: Monad m => Interpreter m ProgramState
getProgramState = getsProgramState id

putProgramState :: Monad m => ProgramState -> Interpreter m ()
putProgramState = modifyProgramState . const

getsProgramState :: Monad m => (ProgramState -> a) -> Interpreter m a
getsProgramState = Interpreter . lift . gets

modifyProgramState :: Monad m 
                   => (ProgramState 
                   -> ProgramState) 
                   -> Interpreter m ()
modifyProgramState = Interpreter . lift . modify

getVariables :: Monad m => Interpreter m (Map String Data)
getVariables = getsProgramState variables

getStatements :: Monad m => Interpreter m (Vector Stmt)
getStatements = getsProgramState statements

getLabels :: Monad m => Interpreter m (Map String Int)
getLabels = getsProgramState labels

getProgramCounter :: Monad m => Interpreter m Int
getProgramCounter = getsProgramState programCounter

putVariables vars = modifyProgramState $ \st -> st { variables = vars }
putStatements stmts = modifyProgramState $ \st -> st { statements = stmts }
putLabels lbls = modifyProgramState $ \st -> st { labels = lbls }
putProgramCounter pc = modifyProgramState $ \st -> st { programCounter = pc }

modifyVariables f = modifyProgramState $
        \st -> st { variables = f $ variables st }
modifyStatements f = modifyProgramState $
    \st -> st { statements = f $ statements st }
modifyLabels f = modifyProgramState $
    \st -> st { labels = f $ labels st }
modifyProgramCounter f = modifyProgramState $
    \st -> st { programCounter = f $ programCounter st }

-- Fetches the next statement to execute
fetch :: Monad m => Interpreter m Stmt
fetch = (V.!) <$> getStatements <*> getProgramCounter

-- Find the index of the statement with a label
labelLookup :: Monad m => String -> Interpreter m (Maybe Int)
labelLookup lbl = M.lookup lbl <$> getLabels

-- Retreive the value of a variable
varLookup :: Monad m => String -> Interpreter m (Maybe Data)
varLookup id = M.lookup id <$> getVariables

-- Write the value of a variable
varWrite :: Monad m => String -> Data -> Interpreter m ()
varWrite id val = modifyVariables (M.insert id val)

isStringable :: Data -> Bool
isStringable (StringData _) = True
isStringable (IntegerData _) = True
isStringable (RealData _) = True
isStringable (PatternData (LiteralPattern _)) = True
isStringable (PatternData (ConcatPattern a b)) 
    = isStringable (PatternData a) && isStringable (PatternData b)
isStringable _ = False

isString :: Data -> Bool
isString (StringData _) = True
isString _ = False

isInteger :: Data -> Bool
isInteger (IntegerData _) = True
isInteger _ = False

isReal :: Data -> Bool
isReal (RealData _) = True
isReal _ = False

{-
isIntegerable :: Data -> Bool
isIntegerable (StringData _) = True
isIntegerable (IntegerData _) = True
isIntegerable (RealData _) = True
isIntegerable _ = False


isRealable :: Data -> Bool
isRealable (StringData _) = True
isRealable (IntegerData _) = True
isRealable (RealData _) = True
isRealable _ = False

isPatternable :: Data -> Bool
isPatternable (PatternData _) = True
isPatternable x = isStringable x
-}

toString :: Monad m => Data -> Evaluator m Data
toString (StringData s) = return $ StringData s
toString (IntegerData i) = return $ StringData $ show i
toString (RealData r) = return $ StringData $ show r
toString (PatternData (LiteralPattern s)) = return $ StringData s
toString (PatternData (ConcatPattern a b)) = do
    StringData a' <- toString $ PatternData a
    StringData b' <- toString $ PatternData b
    return $ StringData $ a' ++ b'
toString _ = liftEval $ programError ProgramError

toPattern :: Monad m => Data -> Evaluator m Pattern
toPattern (PatternData p) = return p
toPattern x = do
    StringData s <- toString x
    return $ LiteralPattern $ s

toInteger :: Monad m => Data -> Evaluator m Data
toInteger (IntegerData i) = return $ IntegerData i
toInteger x = do
    StringData s <- toString x
    case readMaybe s of
        Just i -> return $ IntegerData i
        Nothing -> failEvaluation

toReal :: Monad m => Data -> Evaluator m Data
toReal (RealData i) = return $ RealData i
toReal x = do
    StringData s <- toString x
    case readMaybe s of
        Just i -> return $ RealData i
        Nothing -> failEvaluation
    

raiseArgs :: Monad m => Data -> Data -> Evaluator m (Data, Data)
raiseArgs a b
    | isString a && isString b = return (a,b)
    | isInteger a && isInteger b = return (a,b)
    | isReal a && isReal b = return (a,b)
    
    | isString a && isInteger b = do
        a' <- toInteger a
        return (a',b)
    | isInteger a && isString b = do
        b' <- toInteger b
        return (a,b')
    
    | isString a && isReal b = do
        a' <- toReal a
        return (a',b)
    | isReal a && isString b = do
        b' <- toReal b
        return (a,b')
    
    | isInteger a && isReal b = do
        a' <- toReal a
        return (a',b)
    | isReal a && isInteger b = do
        b' <- toReal b
        return (a,b')
    
    | otherwise = liftEval $ programError ProgramError

lowerArgs :: Monad m => Data -> Data -> Evaluator m (Data, Data)
lowerArgs a b
    | isString a && isString b = return (a,b)
    | isInteger a && isInteger b = return (a,b)
    | isReal a && isReal b = return (a,b)
    
    | isString a && isInteger b = do
        b' <- toString b
        return (a,b')
    | isInteger a && isString b = do
        a' <- toString a
        return (a',b)
    
    | isString a && isReal b = do
        b' <- toString b
        return (a,b')
    | isReal a && isString b = do
        a' <- toString a
        return (a',b)
    
    | isInteger a && isReal b = do
        b' <- toInteger b
        return (a,b')
    | isReal a && isInteger b = do
        a' <- toInteger a
        return (a',b)
    
    | otherwise = liftEval $ programError ProgramError

evalOp :: Monad m => Operator -> Data -> Data -> Evaluator m Data
evalOp Plus a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a''+b'')
        (RealData a'', RealData b'') -> return $ RealData (a''+b'')
        _ -> liftEval $ programError ProgramError
evalOp Minus a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a''-b'')
        (RealData a'', RealData b'') -> return $ RealData (a''-b'')
        _ -> liftEval $ programError ProgramError
evalOp Star a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a''*b'')
        (RealData a'', RealData b'') -> return $ RealData (a''*b'')
        _ -> liftEval $ programError ProgramError
evalOp Slash a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a'' `div` b'')
        (RealData a'', RealData b'') -> return $ RealData (a'' / b'')
        _ -> liftEval $ programError ProgramError
evalOp Bang a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a'' ^ b'')
        (RealData a'', RealData b'') -> return $ RealData (a'' ** b'')
        _ -> liftEval $ programError ProgramError
evalOp DoubleStar a b = evalOp Bang a b
evalOp Pipe a b = do
    a' <- toPattern a
    b' <- toPattern b
    return $ PatternData $ AlternativePattern a' b'

evalConcat :: Monad m => Data -> Data -> Evaluator m Data
evalConcat a b = do
    a' <- toPattern a
    b' <- toPattern b
    return $ PatternData $ ConcatPattern a' b'


evalExpr :: MonadIO m => Expr -> Evaluator m Data
-- TODO: PrefixExpr
-- TODO: UnevaluatedExpr
evalExpr (IdExpr "INPUT") = StringData <$> (liftIO $ getLine)
evalExpr (IdExpr name) = do
    d <- liftEval $ varLookup name
    case d of
        Just d -> return d
        Nothing -> failEvaluation
evalExpr (LitExpr (Int i)) = return $ IntegerData i
evalExpr (LitExpr (Real r)) = return $ RealData r
evalExpr (LitExpr (String s)) = return $ StringData s
-- TODO: CallExpr
-- TODO: RefExpr
evalExpr (ParenExpr expr) = evalExpr expr
evalExpr (BinaryExpr a op b) = do
    a' <- evalExpr a
    b' <- evalExpr b
    evalOp op a' b'
evalExpr (ConcatExpr a b) = do
    a' <- evalExpr a
    b' <- evalExpr b
    evalConcat a' b'
evalExpr NullExpr = return $ StringData ""

-- Execute a subject and return the lookup for it
execSub :: MonadIO m => Expr -> Evaluator m Lookup
execSub (IdExpr "INPUT") = return $ Input
execSub (IdExpr "OUTPUT") = return $ Output
execSub (IdExpr "PUNCH") = return $ Punch
execSub (IdExpr s) = return $ Lookup s
execSub (PrefixExpr Dollar expr) = do
    expr' <- evalExpr expr
    StringData s <- toString expr'
    return $ Lookup s
execSub _ = liftEval $ programError ProgramError

-- Execute a pattern, and return the pattern structure for it
execPat :: Monad m => Expr -> Evaluator m Pattern
execPat _ = liftEval $ programError ProgramError

-- Execute a replacement, and return the new data
execRepl :: MonadIO m => Lookup -> Pattern -> Expr -> Evaluator m ()
execRepl (Lookup s) EverythingPattern expr = do
    val <- evalExpr expr
    liftEval $ varWrite s val
execRepl Output EverythingPattern expr = do
    val <- evalExpr expr
    StringData str <- toString val
    liftIO $ putStrLn str
execRepl _ _ _ = liftEval $ programError ProgramError

-- Execute a goto
execGoto :: Monad m => EvalStop -> Goto -> Evaluator m ()
execGoto _ _ = liftEval $ programError ProgramError

-- Execute one of the steps above, ignoring if it is missing
execMaybe :: Monad m 
          => (x -> m y) 
          -> Maybe x 
          -> m (Maybe y)
execMaybe f (Just x) = Just <$> f x
execMaybe _ _ = return Nothing

catchEval :: Monad m 
          => Evaluator m a 
          -> (EvalStop -> Interpreter m a)
          -> Interpreter m a
catchEval m h = do
    result <- unliftEval m
    case result of
        Right val -> return val
        Left stop -> h stop

-- | Execute a statement in the interpreter
exec :: MonadIO m => Stmt -> Interpreter m ()
exec (EndStmt _) = programError NormalTermination
exec (Stmt _ sub pat obj go) = flip catchEval handler $ do
    subResult <- execMaybe execSub sub
    lookup <- case subResult of
        Just lookup -> return lookup
        Nothing -> finishEvaluation
    patResult <- execMaybe execPat pat
    pattern <- case patResult of
        Just pattern -> return pattern
        Nothing -> return EverythingPattern
    execMaybe (execRepl lookup pattern) obj
    finishEvaluation
  where
    handler r = do
        gotoResult <- catchEval (execMaybe (execGoto r) go) 
                   $ \_ -> programError ProgramError
        case gotoResult of
            Just _ -> return ()
            Nothing -> modifyProgramCounter (+1)
            

                        
                        
-- Execute the next statement
step :: MonadIO m => Interpreter m ()
step = fetch >>= exec

-- | Load a program into the interpreter
load :: Monad m => Program -> Interpreter m ()
load stmts = putStatements $ V.fromList stmts

-- | Run the interpreter continuously by fetching the next statement 
-- until the program ends
run :: MonadIO m => Interpreter m ProgramError
run = do
    result <- Interpreter $ lift $ runExceptT $ runInterpreter $ step
    case result of
        Right () -> run
        Left err -> return err
    
-- | Execute an interpreter action
interpret :: MonadIO m 
          => ProgramState 
          -> Interpreter m a 
          -> m (Either ProgramError a)
interpret st m = flip evalStateT st
        $ runExceptT 
        $ runInterpreter m

-- | Run a SNOBOL4 program
runProgram :: MonadIO m => Program -> m ProgramError
runProgram code = do
    result <- interpret emptyState $ load code >> run
    case result of
        Right result -> return result
