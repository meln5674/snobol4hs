module Language.Snobol4.Interpreter.Internal.StateMachine.Tables where

import qualified Data.Map as M

import Control.Monad

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC

noTables :: Tables
noTables = M.empty

-- | Get the tables known to the interpreter
getTables :: InterpreterShell m => Interpreter m Tables
getTables = getsProgramState tables

-- | Set the tables known to the interpreter
putTables :: InterpreterShell m => Tables -> Interpreter m ()
putTables tbls = modifyProgramState $ \st -> st { tables = tbls }

-- | Apply a function to the tables known to the interpreter
modifyTables :: InterpreterShell m
             => (Tables -> Tables)
             -> Interpreter m ()
modifyTables f = modifyProgramState $
    \st -> st { tables = f $ tables st }

-- | Allocate a new table
tablesNew :: InterpreterShell m => Interpreter m TableKey
tablesNew = tablesNew' emptyTable

tablesNew' :: InterpreterShell m => Snobol4Table -> Interpreter m TableKey
tablesNew' tab = do
    newKey <- (succ . fst . M.findMax) `liftM` getTables
    modifyTables $ M.insert newKey $ newRef tab
    return newKey

-- | Lookup a table
tablesLookup :: InterpreterShell m => TableKey -> Interpreter m (Maybe Snobol4Table)
tablesLookup k = fmap getRefItem <$> M.lookup k <$> getTables

-- | Apply a function to a table
tablesUpdate :: InterpreterShell m => (Snobol4Table -> Snobol4Table) -> TableKey -> Interpreter m ()
tablesUpdate f k = modifyTables $ M.adjust (fmap f) k

-- | Get the value of a table with the given key
tablesRead :: InterpreterShell m => Data -> TableKey -> Interpreter m (Maybe Data)
tablesRead k1 k2 = tablesLookup k2 >>= \x -> return $ x >>= readTable k1

-- | Set the value of a table with the given key
tablesWrite :: InterpreterShell m => Data -> Data -> TableKey -> Interpreter m ()
tablesWrite k v = tablesUpdate $ writeTable k v

tablesIncRef :: InterpreterShell m => TableKey -> Interpreter m ()
tablesIncRef k = modifyTables $ M.adjust incRefCount k

tablesDecRef :: InterpreterShell m => TableKey -> Interpreter m ()
tablesDecRef k = modifyTables $ M.update decRefCount k
