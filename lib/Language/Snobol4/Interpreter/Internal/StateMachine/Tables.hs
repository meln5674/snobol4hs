{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Tables
Description     : Maintaining tables
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.Tables where

import qualified Data.Map as M

import Control.Monad

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Internal.StateMachine.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.ProgramState
import Language.Snobol4.Interpreter.Internal.StateMachine.GC

-- | Empty collection of tables
noTables :: Tables
noTables = M.empty

-- | Get the tables known to the interpreter
getTables :: InterpreterShell m => InterpreterGeneric program m Tables
getTables = getsProgramState tables

-- | Set the tables known to the interpreter
putTables :: InterpreterShell m => Tables -> InterpreterGeneric program m ()
putTables tbls = modifyProgramState $ \st -> st { tables = tbls }

-- | Apply a function to the tables known to the interpreter
modifyTables :: InterpreterShell m
             => (Tables -> Tables)
             -> InterpreterGeneric program m ()
modifyTables f = modifyProgramState $
    \st -> st { tables = f $ tables st }

-- | Allocate an empty table
tablesNew :: InterpreterShell m => InterpreterGeneric program m TableKey
tablesNew = tablesNew' emptyTable

-- | Add a new table
tablesNew' :: InterpreterShell m => Snobol4Table -> InterpreterGeneric program m TableKey
tablesNew' tab = do
    newKey <- (succ . fst . M.findMax) `liftM` getTables
    modifyTables $ M.insert newKey $ newRef tab
    return newKey

-- | Lookup a table
tablesLookup :: InterpreterShell m => TableKey -> InterpreterGeneric program m (Maybe Snobol4Table)
tablesLookup k = fmap getRefItem <$> M.lookup k <$> getTables

-- | Apply a function to a table
tablesUpdate :: InterpreterShell m => (Snobol4Table -> Snobol4Table) -> TableKey -> InterpreterGeneric program m ()
tablesUpdate f k = modifyTables $ M.adjust (fmap f) k

-- | Get the value of a table with the given key
tablesRead :: InterpreterShell m => Data -> TableKey -> InterpreterGeneric program m (Maybe Data)
tablesRead k1 k2 = tablesLookup k2 >>= \x -> return $ x >>= readTable k1

-- | Set the value of a table with the given key
tablesWrite :: InterpreterShell m => Data -> Data -> TableKey -> InterpreterGeneric program m ()
tablesWrite k v = tablesUpdate $ writeTable k v

-- | Increment the number of references to a table
tablesIncRef :: InterpreterShell m => TableKey -> InterpreterGeneric program m ()
tablesIncRef k = modifyTables $ M.adjust incRefCount k

-- | Decrement the number of refernces to a table
tablesDecRef :: InterpreterShell m => TableKey -> InterpreterGeneric program m ()
tablesDecRef k = modifyTables $ M.update decRefCount k
