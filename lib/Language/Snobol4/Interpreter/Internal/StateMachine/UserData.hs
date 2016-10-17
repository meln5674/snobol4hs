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

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Interpreter.Shell

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

-- | Lookup a user-defined datatype
datatypesLookup :: InterpreterShell m => Snobol4String -> InterpreterGeneric program m (Maybe Snobol4Datatype)
datatypesLookup k = M.lookup k <$> getDatatypes

-- | Lookup a value of a user-defined datatype
userDataLookup :: InterpreterShell m => UserKey -> InterpreterGeneric program m (Maybe (Snobol4UserData (ExprType m)))
userDataLookup k = M.lookup k <$> getUserDatas

-- | Create a new user-defined datatype
datatypesNew :: InterpreterShell m => Snobol4Datatype -> InterpreterGeneric program m ()
datatypesNew datatype = modifyDatatypes $ M.insert (datatypeName datatype) datatype


