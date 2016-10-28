{-|
Module          : Language.Snobol4.Interpreter.Data
Description     : Data types for SNOBOL4 programs
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown


-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Snobol4.Interpreter.Data 
    ( Data (..)
    , Snobol4Read (..)
    , module Language.Snobol4.Interpreter.Data.Lazy
    , module Language.Snobol4.Interpreter.Data.Integer
    , module Language.Snobol4.Interpreter.Data.String
    , module Language.Snobol4.Interpreter.Data.Real
    , module Language.Snobol4.Interpreter.Data.Pattern
    , module Language.Snobol4.Interpreter.Data.Array
    , module Language.Snobol4.Interpreter.Data.Table
    , module Language.Snobol4.Interpreter.Data.UserData
    , module Language.Snobol4.Interpreter.Data.ObjectCode
    , Lookup (..)
    , module Language.Snobol4.Interpreter.Data
    , (<>)
    ) where

import Data.Array
import Data.Map
import qualified Data.Map as M
import Data.Monoid
import Data.String

import Text.Read (readMaybe)

import Control.Monad

import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Interpreter.Data.String
import Language.Snobol4.Interpreter.Data.Real
import Language.Snobol4.Interpreter.Data.Integer
import Language.Snobol4.Interpreter.Data.Array
import Language.Snobol4.Interpreter.Data.Table
import Language.Snobol4.Interpreter.Data.UserData
import Language.Snobol4.Interpreter.Data.ObjectCode
import Language.Snobol4.Interpreter.Data.Pattern
import Language.Snobol4.Interpreter.Data.Lazy

import Language.Snobol4.Syntax.AST

-- | Formal identification for the string type
datatypeNameString :: Snobol4String
datatypeNameString = "STRING"

-- | Formal identification for the integer type
datatypeNameInteger :: Snobol4String
datatypeNameInteger = "INTEGER"

-- | Formal identification for the real type
datatypeNameReal :: Snobol4String
datatypeNameReal = "REAL"

-- | Formal identification for the pattern type
datatypeNamePattern :: Snobol4String
datatypeNamePattern = "PATTERN"

-- | Formal identification for the array type
datatypeNameArray :: Snobol4String
datatypeNameArray = "ARRAY"

-- | Formal identification for the table type
datatypeNameTable :: Snobol4String
datatypeNameTable = "TABLE"

-- | Formal identification for the name type
datatypeNameName :: Snobol4String
datatypeNameName = "NAME"

-- | Formal identification for the expression type
datatypeNameExpression :: Snobol4String
datatypeNameExpression = "EXPRESSION"

-- | Formal identification for the code type
datatypeNameCode :: Snobol4String
datatypeNameCode = "CODE"

-- | Check if data is a string
isString :: (Data expr) -> Bool
isString (StringData _) = True
isString _ = False

-- | Check if data is an integer
isInteger :: (Data expr) -> Bool
isInteger (IntegerData _) = True
isInteger _ = False

-- | Check if data is a real
isReal :: (Data expr) -> Bool
isReal (RealData _) = True
isReal _ = False
