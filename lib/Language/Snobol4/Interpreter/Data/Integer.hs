{-|
Module          : Language.Snobol4.Interpreter.Data.Integer
Description     : Integer Datatype
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.Interpreter.Data.Integer 
    ( module Language.Snobol4.Interpreter.Data.Integer 
    , Snobol4IntegerClass (..)
    , Snobol4Integer
    ) where

import Control.Monad

import Text.Read (readMaybe)

import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Interpreter.Data.String

-- | Try to read as a normal integer, then convert to wrapped type
instance Snobol4Read Snobol4Integer where
    snobol4Read (Snobol4String s) = liftM Snobol4Integer $ readMaybe s

-- | Convert the internal string to/from an integer
instance Snobol4StringClass Snobol4Integer where
    mkString = Snobol4String . show . getInteger
    unmkString = Snobol4Integer . read . getString

-- | Identity
instance Snobol4IntegerClass Snobol4Integer where
    mkInteger = id
    unmkInteger = id

-- | Convert to/from an integer that has the matching internal int
instance Snobol4IntegerClass Int where
    mkInteger = Snobol4Integer
    unmkInteger = getInteger

-- | Remove/Add the fractional part from the internal int
instance Snobol4RealClass Snobol4Integer where
    mkReal = Snobol4Real . fromIntegral . getInteger
    unmkReal = Snobol4Integer . round . getReal
