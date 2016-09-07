{-|
Module          : Language.Snobol4.Interpreter.Data.Integer
Description     : Integer Datatype
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Data.Integer 
    ( module Language.Snobol4.Interpreter.Data.Integer 
    , Snobol4IntegerClass (..)
    , Snobol4Integer
    ) where

import Control.Monad

import Text.Read (readMaybe)

import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Interpreter.Data.String

-- |
instance Snobol4Read Snobol4Integer where
    snobol4Read (Snobol4String s) = liftM Snobol4Integer $ readMaybe s

-- |
instance Snobol4StringClass Snobol4Integer where
    mkString = Snobol4String . show . getInteger
    unmkString = Snobol4Integer . read . getString

-- |
instance Snobol4IntegerClass Snobol4Integer where
    mkInteger = id
    unmkInteger = id

-- |
instance Snobol4IntegerClass Int where
    mkInteger = Snobol4Integer
    unmkInteger = getInteger

instance Snobol4RealClass Snobol4Integer where
    mkReal = Snobol4Real . fromIntegral . getInteger
    unmkReal = Snobol4Integer . round . getReal
