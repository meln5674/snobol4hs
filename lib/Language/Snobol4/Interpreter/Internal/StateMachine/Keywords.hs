{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Keywords
Description     : Interpreter keywords
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}
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

-- | Empty set of protected keywords
noProtectedKeywords :: ProtectedKeywords expr
noProtectedKeywords = M.empty

-- | Empty set of unprotected keywords
noUnprotectedKeywords :: UnprotectedKeywords expr
noUnprotectedKeywords = M.empty

-- | Get protected keywords
getProtectedKeywords :: ( InterpreterShell m
                        )
                     => InterpreterGeneric (ProgramType m) m (ProtectedKeywords (ExprType m))
getProtectedKeywords = getsProgramState protectedKeywords

-- | Get unprotected keywords
getUnprotectedKeywords :: ( InterpreterShell m
                          )
                       => InterpreterGeneric (ProgramType m) m (UnprotectedKeywords (ExprType m))
getUnprotectedKeywords = getsProgramState unprotectedKeywords

-- | Set protected keywords
putProtectedKeywords :: ( InterpreterShell m
                        )
                     => ProtectedKeywords (ExprType m)
                     -> InterpreterGeneric (ProgramType m) m ()
putProtectedKeywords kws = modifyProgramState $ \st -> st { protectedKeywords = kws }

-- | Set unprotected keywords
putUnprotectedKeywords :: ( InterpreterShell m
                          )
                       => UnprotectedKeywords (ExprType m)
                       -> InterpreterGeneric (ProgramType m) m ()
putUnprotectedKeywords kws = modifyProgramState $ \st -> st { unprotectedKeywords = kws }

-- | Apply a function to protected keywords
modifyProtectedKeywords :: ( InterpreterShell m
                           )
                        => ( ProtectedKeywords (ExprType m) -> ProtectedKeywords (ExprType m) )
                        -> InterpreterGeneric (ProgramType m) m ()
modifyProtectedKeywords f = modifyProgramState $ \st -> st { protectedKeywords = f $ protectedKeywords st }

-- | Apply a function to unprotected keywords
modifyUnprotectedKeywords :: ( InterpreterShell m
                             )
                          => ( UnprotectedKeywords (ExprType m) -> UnprotectedKeywords (ExprType m) )
                          -> InterpreterGeneric (ProgramType m) m ()
modifyUnprotectedKeywords f = modifyProgramState $ \st -> st { unprotectedKeywords = f $ unprotectedKeywords st }

-- | Look up the value of a keyword
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

-- | Modify the value of a keyword
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

-- | Assign an unprotected keyword, terminates the program if the keyword is
-- protected
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

-- | Test if the interpreter is in anchor mode
getAnchorMode :: ( InterpreterShell m 
                 , LocalVariablesClass m
                 , NewSnobol4Machine m
                 ) => InterpreterGeneric (ProgramType m) m Bool
getAnchorMode = do
    anchorValue <- lookupKeyword "ANCHOR" >>= toInteger
    return $ maybe True (0 /=) anchorValue

-- | Increment the function level
incFunctionLevel :: ( InterpreterShell m 
                    , LocalVariablesClass m
                    , NewSnobol4Machine m
                    )
                  => InterpreterGeneric (ProgramType m) m ()
incFunctionLevel = do
    functionLevel <- lookupKeyword "FNCLEVEL" >>= toInteger
    case functionLevel of
        Just x -> modifyKeyword "FNCLEVEL" $ IntegerData $ x + 1
        Nothing -> programError ErrorInSnobol4System

-- | Decrement the function level
decFunctionLevel :: ( InterpreterShell m 
                 , LocalVariablesClass m
                 , NewSnobol4Machine m
                 ) => InterpreterGeneric (ProgramType m) m ()
decFunctionLevel = do
    functionLevel <- lookupKeyword "FNCLEVEL" >>= toInteger
    case functionLevel of
        Just x -> modifyKeyword "FNCLEVEL" $ IntegerData $ x - 1
        Nothing -> programError ErrorInSnobol4System

-- | Set the return type
setReturnType :: ( InterpreterShell m 
                 , LocalVariablesClass m
                 ) 
              => Snobol4String
              -> InterpreterGeneric (ProgramType m) m ()
setReturnType = modifyKeyword "RTNTYPE" . StringData

-- | Increase the counter of failed statements
incFailCount :: ( InterpreterShell m 
                 , LocalVariablesClass m
--                 , NewSnobol4Machine m
                 ) 
              => InterpreterGeneric (ProgramType m) m ()
incFailCount = do
    failCount <- lookupKeyword "STFCOUNT" >>= toInteger
    case failCount of
        Just x -> modifyKeyword "STFCOUNT" $ IntegerData $ x + 1
        Nothing -> programError ErrorInSnobol4System

-- | Test if outputs are enabled
getOutputSwitch :: ( InterpreterShell m 
                 , LocalVariablesClass m
--                 , NewSnobol4Machine m
                 ) => InterpreterGeneric (ProgramType m) m Bool
getOutputSwitch = liftM (maybe True (0/=)) $ lookupKeyword "OUTPUT" >>= toInteger

-- | Test if inputs are enabled
getInputSwitch :: ( InterpreterShell m 
                 , LocalVariablesClass m
--                 , NewSnobol4Machine m
                 ) => InterpreterGeneric (ProgramType m) m Bool
getInputSwitch = liftM (maybe True (0/=)) $ lookupKeyword "INPUT" >>= toInteger

-- | Test if the interpreter is in trim mode
getTrimMode :: ( InterpreterShell m 
                 , LocalVariablesClass m
--                 , NewSnobol4Machine m
                 ) => InterpreterGeneric (ProgramType m) m Bool
getTrimMode = liftM (maybe True (0/=)) $ lookupKeyword "TRIM" >>= toInteger

getFullscanMode :: ( InterpreterShell m 
                 , LocalVariablesClass m
--                 , NewSnobol4Machine m
                 ) => InterpreterGeneric (ProgramType m) m Bool
getFullscanMode = liftM (maybe True (0/=)) $ lookupKeyword "FULLSCAN" >>= toInteger
