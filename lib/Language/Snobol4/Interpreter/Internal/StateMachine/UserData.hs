module Language.Snobol4.Interpreter.Internal.StateMachine.UserData where

import qualified Data.Map as M

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Interpreter.Shell

noDatatypes :: Datatypes
noDatatypes = M.empty

noUserData :: UserDatas
noUserData = M.empty

getDatatypes :: InterpreterShell m => Interpreter m Datatypes
getDatatypes = getsProgramState datatypes

getUserDatas :: InterpreterShell m => Interpreter m UserDatas
getUserDatas = getsProgramState userDatas

modifyDatatypes :: InterpreterShell m => (Datatypes -> Datatypes) -> Interpreter m ()
modifyDatatypes f = modifyProgramState $
    \st -> st { datatypes = f $ datatypes st }

datatypeLookup :: InterpreterShell m => Snobol4String -> Interpreter m (Maybe Snobol4Datatype)
datatypeLookup name = M.lookup name <$> getDatatypes

userDataLookup :: InterpreterShell m => UserKey -> Interpreter m (Maybe Snobol4UserData)
userDataLookup k = M.lookup k <$> getUserDatas


datatypesNew :: InterpreterShell m => Snobol4Datatype -> Interpreter m ()
datatypesNew datatype = modifyDatatypes $ M.insert (datatypeName datatype) datatype

datatypesLookup :: InterpreterShell m => Snobol4String -> Interpreter m (Maybe Snobol4Datatype)
datatypesLookup k = M.lookup k <$> getDatatypes

