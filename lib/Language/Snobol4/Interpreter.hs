{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

newtype InterpreterInternal m a
    = InterpreterInternal
    { runInterpreterInternal
        :: ExceptT EvalStop (StateT ProgramState m) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans InterpreterInternal where
    lift m = InterpreterInternal $ lift $ lift m

newtype Interpreter m a
    = Interpreter
    { runInterpreter
        :: ExceptT ProgramError (InterpreterInternal m) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans Interpreter where
    lift m = Interpreter $ lift $ lift $ m

programError :: Monad m => ProgramError -> Interpreter m a
programError = Interpreter . throwE

failEvaluation :: Monad m => Interpreter m a
failEvaluation = Interpreter 
               $ lift 
               $ InterpreterInternal 
               $ throwE EvalFailed

finishEvaluation :: Monad m => Interpreter m a
finishEvaluation = Interpreter 
                 $ lift 
                 $ InterpreterInternal 
                 $ throwE EvalSuccess

getsProgramStateInternal :: Monad m 
                         => (ProgramState -> a) 
                         -> InterpreterInternal m a
getsProgramStateInternal = InterpreterInternal . lift . gets

modifyProgramStateInternal :: Monad m 
                           => (ProgramState -> ProgramState) 
                           -> InterpreterInternal m ()
modifyProgramStateInternal = InterpreterInternal . lift . modify

getProgramState :: Monad m => Interpreter m ProgramState
getProgramState = getsProgramState id

putProgramState :: Monad m => ProgramState -> Interpreter m ()
putProgramState = modifyProgramState . const

getsProgramState :: Monad m => (ProgramState -> a) -> Interpreter m a
getsProgramState = Interpreter . lift . getsProgramStateInternal

modifyProgramState :: Monad m 
                   => (ProgramState 
                   -> ProgramState) 
                   -> Interpreter m ()
modifyProgramState = Interpreter . lift . modifyProgramStateInternal

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

toString :: Monad m => Data -> Interpreter m Data
toString (StringData s) = return $ StringData s
toString (IntegerData i) = return $ StringData $ show i
toString (RealData r) = return $ StringData $ show r
toString (PatternData (LiteralPattern s)) = return $ StringData s
toString (PatternData (ConcatPattern a b)) = do
    StringData a' <- toString $ PatternData a
    StringData b' <- toString $ PatternData b
    return $ StringData $ a' ++ b'
toString _ = programError ProgramError

toPattern :: Monad m => Data -> Interpreter m Pattern
toPattern (PatternData p) = return p
toPattern x = do
    StringData s <- toString x
    return $ LiteralPattern $ s

toInteger :: Monad m => Data -> Interpreter m Data
toInteger (IntegerData i) = return $ IntegerData i
toInteger x = do
    StringData s <- toString x
    case readMaybe s of
        Just i -> return $ IntegerData i
        Nothing -> failEvaluation

toReal :: Monad m => Data -> Interpreter m Data
toReal (RealData i) = return $ RealData i
toReal x = do
    StringData s <- toString x
    case readMaybe s of
        Just i -> return $ RealData i
        Nothing -> failEvaluation
    

raiseArgs :: Monad m => Data -> Data -> Interpreter m (Data, Data)
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
    
    | otherwise = programError ProgramError

lowerArgs :: Monad m => Data -> Data -> Interpreter m (Data, Data)
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
    
    | otherwise = programError ProgramError

evalOp :: Monad m => Operator -> Data -> Data -> Interpreter m Data
evalOp Plus a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a''+b'')
        (RealData a'', RealData b'') -> return $ RealData (a''+b'')
        _ -> programError ProgramError
evalOp Minus a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a''-b'')
        (RealData a'', RealData b'') -> return $ RealData (a''-b'')
        _ -> programError ProgramError
evalOp Star a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a''*b'')
        (RealData a'', RealData b'') -> return $ RealData (a''*b'')
        _ -> programError ProgramError
evalOp Slash a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a'' `div` b'')
        (RealData a'', RealData b'') -> return $ RealData (a'' / b'')
        _ -> programError ProgramError
evalOp Bang a b = do
    (a',b') <- raiseArgs a b
    case (a',b') of
        (IntegerData a'', IntegerData b'') -> return $ IntegerData (a'' ^ b'')
        (RealData a'', RealData b'') -> return $ RealData (a'' ** b'')
        _ -> programError ProgramError
evalOp DoubleStar a b = evalOp Bang a b
evalOp Pipe a b = do
    a' <- toPattern a
    b' <- toPattern b
    return $ PatternData $ AlternativePattern a' b'

evalConcat :: Monad m => Data -> Data -> Interpreter m Data
evalConcat a b = do
    a' <- toPattern a
    b' <- toPattern b
    return $ PatternData $ ConcatPattern a' b'


evalExpr :: MonadIO m => Expr -> Interpreter m Data
-- TODO: PrefixExpr
-- TODO: UnevaluatedExpr
evalExpr (IdExpr "INPUT") = StringData <$> (liftIO $ getLine)
evalExpr (IdExpr name) = do
    d <- varLookup name
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
execSub :: MonadIO m => Expr -> Interpreter m Lookup
execSub (IdExpr "INPUT") = return $ Input
execSub (IdExpr "OUTPUT") = return $ Output
execSub (IdExpr "PUNCH") = return $ Punch
execSub (IdExpr s) = return $ Lookup s
execSub (PrefixExpr Dollar expr) = do
    expr' <- evalExpr expr
    StringData s <- toString expr'
    return $ Lookup s
execSub _ = programError ProgramError

-- Execute a pattern, and return the pattern structure for it
execPat :: Monad m => Expr -> Interpreter m Pattern
execPat _ = programError ProgramError

-- Execute a replacement, and return the new data
execRepl :: MonadIO m => Lookup -> Pattern -> Expr -> Interpreter m ()
execRepl (Lookup s) EverythingPattern expr = do
    val <- evalExpr expr
    varWrite s val
execRepl Output EverythingPattern expr = do
    val <- evalExpr expr
    StringData str <- toString val
    liftIO $ putStrLn str
execRepl _ _ _ = programError ProgramError

-- Execute a goto
execGoto :: Monad m => EvalStop -> Goto -> Interpreter m ()
execGoto _ _ = programError ProgramError

-- Execute one of the steps above, ignoring if it is missing
execMaybe :: Monad m 
          => (x -> Interpreter m y) 
          -> Maybe x 
          -> Interpreter m (Maybe y)
execMaybe f (Just x) = Just <$> f x
execMaybe _ _ = return Nothing

catchEval :: Monad m 
          => Interpreter m a 
          -> (EvalStop -> Interpreter m a)
          -> Interpreter m a
catchEval m h = do
    st <- getProgramState
    (m', st') <- lift $ runStateT inner st
    putProgramState st'
    m'
  where
    inner = do
        result <- runExceptT 
               $ runInterpreterInternal
               $ runExceptT
               $ runInterpreter m
        case result of
            Right (Right x) -> return $ return x
            Right (Left e) -> return $ programError e
            Left x -> return $ h x


-- Execute a statement in the interpreter
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

load :: Monad m => [Stmt] -> Interpreter m ()
load stmts = putStatements $ V.fromList stmts



run :: MonadIO m => Interpreter m ProgramError
run = do
    st <- getProgramState
    (result, st') <- lift $ flip runStateT st
        $ runExceptT 
        $ runInterpreterInternal 
        $ runExceptT 
        $ runInterpreter
        $ step
    case result of
        Right (Right x) -> do
            putProgramState st'
            run
        Right (Left x) -> return x

interpret :: MonadIO m 
          => ProgramState 
          -> Interpreter m a 
          -> m (Either (Either ProgramError EvalStop) a)
interpret st m = do
    result <- flip evalStateT st
        $ runExceptT 
        $ runInterpreterInternal 
        $ runExceptT 
        $ runInterpreter m
    case result of
        Right (Right x) -> return $ Right x
        Right (Left x) -> return $ Left (Left x)
        Left x -> return $ Left (Right x) 

runProgram :: MonadIO m => Program -> m ProgramError
runProgram code = do
    result <- interpret emptyState $ load code >> run
    case result of
        Right result -> return result
