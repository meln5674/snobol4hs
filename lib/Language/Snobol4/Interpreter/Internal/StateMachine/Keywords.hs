{-# LANGUAGE OverloadedStrings #-}
module Language.Snobol4.Interpreter.Internal.StateMachine.Keywords where

import Prelude hiding (toInteger)

import qualified Data.Map as M

import Control.Monad

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Shell

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Convert
import Language.Snobol4.Interpreter.Internal.StateMachine.Error
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState

noProtectedKeywords :: ProtectedKeywords expr
noProtectedKeywords = M.empty

noUnprotectedKeywords :: UnprotectedKeywords expr
noUnprotectedKeywords = M.empty

getProtectedKeywords :: ( InterpreterShell m
                        )
                     => InterpreterGeneric (ProgramType m) m (ProtectedKeywords (ExprType m))
getProtectedKeywords = getsProgramState protectedKeywords

getUnprotectedKeywords :: ( InterpreterShell m
                          )
                       => InterpreterGeneric (ProgramType m) m (UnprotectedKeywords (ExprType m))
getUnprotectedKeywords = getsProgramState unprotectedKeywords

putProtectedKeywords :: ( InterpreterShell m
                        )
                     => ProtectedKeywords (ExprType m)
                     -> InterpreterGeneric (ProgramType m) m ()
putProtectedKeywords kws = modifyProgramState $ \st -> st { protectedKeywords = kws }

putUnprotectedKeywords :: ( InterpreterShell m
                          )
                       => UnprotectedKeywords (ExprType m)
                       -> InterpreterGeneric (ProgramType m) m ()
putUnprotectedKeywords kws = modifyProgramState $ \st -> st { unprotectedKeywords = kws }

modifyProtectedKeywords :: ( InterpreterShell m
                           )
                        => ( ProtectedKeywords (ExprType m) -> ProtectedKeywords (ExprType m) )
                        -> InterpreterGeneric (ProgramType m) m ()
modifyProtectedKeywords f = modifyProgramState $ \st -> st { protectedKeywords = f $ protectedKeywords st }

modifyUnprotectedKeywords :: ( InterpreterShell m
                             )
                          => ( UnprotectedKeywords (ExprType m) -> UnprotectedKeywords (ExprType m) )
                          -> InterpreterGeneric (ProgramType m) m ()
modifyUnprotectedKeywords f = modifyProgramState $ \st -> st { unprotectedKeywords = f $ unprotectedKeywords st }

lookupKeyword :: ( InterpreterShell m 
                 )
           => Snobol4String 
           -> InterpreterGeneric (ProgramType m) m (Data (ExprType m))
lookupKeyword sym = do
    protected <- getProtectedKeywords
    unprotected <- getUnprotectedKeywords
    case M.lookup sym protected of
        Just value -> return value
        Nothing -> case M.lookup sym unprotected of
            Just value -> return value
            Nothing -> programError UnknownKeyword

modifyKeyword :: ( InterpreterShell m 
                 )
              => Snobol4String
              -> Data (ExprType m)
              -> InterpreterGeneric (ProgramType m) m ()
modifyKeyword sym val = do
    protected <- getProtectedKeywords
    unprotected <- getUnprotectedKeywords
    case M.lookup sym protected of
        Just _ -> modifyProtectedKeywords $ M.insert sym val
        Nothing -> case M.lookup sym unprotected of
            Just _ -> modifyUnprotectedKeywords $ M.insert sym val
            Nothing -> programError UnknownKeyword

assignKeyword :: ( InterpreterShell m 
                 )
              => Snobol4String
              -> Data (ExprType m)
              -> InterpreterGeneric (ProgramType m) m ()
assignKeyword sym val = do
    protected <- getProtectedKeywords
    unprotected <- getUnprotectedKeywords
    case M.lookup sym protected of
        Just _ -> programError UnknownKeyword
        Nothing -> modifyUnprotectedKeywords $ M.insert sym val


getAnchorMode :: ( InterpreterShell m 
                 , LocalVariablesClass m
                 ) => InterpreterGeneric (ProgramType m) m Bool
getAnchorMode = do
    anchorValue <- lookupKeyword "ANCHOR" >>= toInteger
    return $ maybe True (0 /=) anchorValue

incFunctionLevel :: ( InterpreterShell m 
                 , LocalVariablesClass m
                 ) => InterpreterGeneric (ProgramType m) m ()
incFunctionLevel = do
    functionLevel <- lookupKeyword "FNCLEVEL" >>= toInteger
    case functionLevel of
        Just x -> modifyKeyword "FNCLEVEL" $ IntegerData $ x + 1
        Nothing -> programError ErrorInSnobol4System

decFunctionLevel :: ( InterpreterShell m 
                 , LocalVariablesClass m
                 ) => InterpreterGeneric (ProgramType m) m ()
decFunctionLevel = do
    functionLevel <- lookupKeyword "FNCLEVEL" >>= toInteger
    case functionLevel of
        Just x -> modifyKeyword "FNCLEVEL" $ IntegerData $ x - 1
        Nothing -> programError ErrorInSnobol4System

setReturnType :: ( InterpreterShell m 
                 , LocalVariablesClass m
                 ) 
              => Snobol4String
              -> InterpreterGeneric (ProgramType m) m ()
setReturnType = modifyKeyword "RTNTYPE" . StringData

incFailCount :: ( InterpreterShell m 
                 , LocalVariablesClass m
                 ) 
              => InterpreterGeneric (ProgramType m) m ()
incFailCount = do
    failCount <- lookupKeyword "STFCOUNT" >>= toInteger
    case failCount of
        Just x -> modifyKeyword "STFCOUNT" $ IntegerData $ x + 1
        Nothing -> programError ErrorInSnobol4System

getOutputSwitch :: ( InterpreterShell m 
                 , LocalVariablesClass m
                 ) => InterpreterGeneric (ProgramType m) m Bool
getOutputSwitch = liftM (maybe True (0/=)) $ lookupKeyword "OUTPUT" >>= toInteger

getInputSwitch :: ( InterpreterShell m 
                 , LocalVariablesClass m
                 ) => InterpreterGeneric (ProgramType m) m Bool
getInputSwitch = liftM (maybe True (0/=)) $ lookupKeyword "INPUT" >>= toInteger

getTrimMode :: ( InterpreterShell m 
                 , LocalVariablesClass m
                 ) => InterpreterGeneric (ProgramType m) m Bool
getTrimMode = liftM (maybe True (0/=)) $ lookupKeyword "TRIM" >>= toInteger
