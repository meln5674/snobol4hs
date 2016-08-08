{-|
Module          : Language.Snobol4.Interpreter.Data.Table
Description     : Table Datatype
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Data.Table 
    ( module Language.Snobol4.Interpreter.Data.Table 
    , Snobol4Table
    , TableKey
    ) where

import Data.Map (Map)
import qualified Data.Map as M

import Language.Snobol4.Interpreter.Data.Types


-- | An empty table
emptyTable :: Snobol4Table
emptyTable = Snobol4Table M.empty

-- | Get the value of a table
readTable :: Data -> Snobol4Table -> Maybe Data
readTable k (Snobol4Table tbl) = M.lookup k tbl

-- | Set the value of a table
writeTable :: Data -> Data -> Snobol4Table -> Snobol4Table
writeTable k v (Snobol4Table tbl) = Snobol4Table $ M.insert k v tbl

-- | Get the formal identification of a table
-- TODO
tableFormalIdent :: Snobol4Table -> Snobol4String
tableFormalIdent tab = undefined
