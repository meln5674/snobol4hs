{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.UserData
Description     : Maintaining user-defined datatypes and their values
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.UserData where

import qualified Data.Map as M

import Control.Monad

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.Error
import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Error

-- | Empty collection of user-defined datatypes
noDatatypes :: Datatypes
noDatatypes = M.empty

-- | Empty collection of vaules of user-defined datatypes
noUserData :: (UserDatas expr)
noUserData = M.empty

-- | Get the user-defined datatypes known to the interpreter
getDatatypes :: InterpreterShell m => InterpreterGeneric program m Datatypes
getDatatypes = getsProgramState datatypes

-- | Get the values of user-defined datatypes known to the interpreter
getUserDatas :: InterpreterShell m => InterpreterGeneric program m (UserDatas (ExprType m))
getUserDatas = getsProgramState userDatas

-- | Apply a function to the the user-defined datatypes known to the interpreter
modifyDatatypes :: InterpreterShell m => (Datatypes -> Datatypes) -> InterpreterGeneric program m ()
modifyDatatypes f = modifyProgramState $
    \st -> st { datatypes = f $ datatypes st }

modifyUserDatas :: InterpreterShell m 
                => (UserDatas (ExprType m) -> UserDatas (ExprType m)) 
                -> InterpreterGeneric program m ()
modifyUserDatas f = modifyProgramState $ 
    \st -> st { userDatas = f $ userDatas st }

-- | Lookup a user-defined datatype
datatypesLookup :: InterpreterShell m => Snobol4String -> InterpreterGeneric program m (Maybe Snobol4Datatype)
datatypesLookup k = M.lookup k <$> getDatatypes

-- | Lookup a value of a user-defined datatype
userDataLookup :: InterpreterShell m => UserKey -> InterpreterGeneric program m (Maybe (Snobol4UserData (ExprType m)))
userDataLookup k = M.lookup k <$> getUserDatas

-- | Create a new user-defined datatype
datatypesNew :: InterpreterShell m => Snobol4Datatype -> InterpreterGeneric program m ()
datatypesNew datatype = modifyDatatypes $ M.insert (datatypeName datatype) datatype

userDatasNextKey :: InterpreterShell m => InterpreterGeneric program m UserKey
userDatasNextKey = liftM (maybe (toEnum 0) (succ . fst . fst) . M.maxViewWithKey) getUserDatas 



userDataConstruct :: InterpreterShell m
                  => Snobol4String
                  -> Snobol4Integer
                  -> [Data (ExprType m)]
                  -> InterpreterGeneric (ProgramType m) m UserKey
userDataConstruct name count args = do
    let args' = if unmkInteger count > length args
                    then args ++ replicate (unmkInteger count - length args) (StringData nullString)
                    else take (unmkInteger count) args
    key <- userDatasNextKey
    let value = Snobol4UserData name args'
    modifyUserDatas $ M.insert key value
    return key

userDataSelect :: InterpreterShell m 
               => UserKey
               -> Snobol4String 
               -> Snobol4Integer 
               -> InterpreterGeneric (ProgramType m) m (Maybe (Data (ExprType m)))
userDataSelect key dataName ix = do
    keyLookup <- userDataLookup key
    case keyLookup of
        Just data_
            | datatypeNameUser data_ == dataName -> 
                case drop (unmkInteger ix) (userDataFields data_) of
                    [] -> programError ErrorInSnobol4System
                    (x:_) -> return $ Just x
            | otherwise -> programError IllegalDataType
        Nothing -> return Nothing

userDataModify :: InterpreterShell m
               => UserKey
               -> Snobol4String
               -> Snobol4Integer
               -> Data (ExprType m)
               -> InterpreterGeneric (ProgramType m) m ()
userDataModify key dataName ix val = do
    keyLookup <- userDataLookup key
    case keyLookup of
        Just data_
            | datatypeNameUser data_ == dataName -> 
                case drop (unmkInteger ix) (userDataFields data_) of
                    [] -> programError ErrorInSnobol4System
                    (_:xs) -> modifyUserDatas $ M.insert key $ data_
                        { userDataFields = 
                            take (unmkInteger ix - 1) (userDataFields data_) ++ val : xs
                        }
            | otherwise -> programError IllegalDataType
        Nothing -> programError ErrorInSnobol4System
