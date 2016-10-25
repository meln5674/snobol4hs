{-|
Module          : Language.Snobol4.Interpreter.Data.Real
Description     : Real Datatype
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Data.Real 
    ( module Language.Snobol4.Interpreter.Data.Real
    , Snobol4RealClass (..)
    , Snobol4Real
    ) where

import Control.Monad

import Text.Read (readMaybe)

import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Interpreter.Data.String

-- | Identity
instance Snobol4RealClass Snobol4Real where
    mkReal = id
    unmkReal = id

-- | Convert to/from the real that has the matching internal float
instance Snobol4RealClass Float where
    mkReal = Snobol4Real
    unmkReal = getReal

-- | Convert the internal float to/from a string
instance Snobol4StringClass Snobol4Real where
    mkString = Snobol4String . show . getReal
    unmkString = Snobol4Real . read . getString

-- | Try to read as a normal float, the convert to wrapped type
instance Snobol4Read Snobol4Real where
    snobol4Read (Snobol4String s) = liftM Snobol4Real $ readMaybe s

-- | Add/Remove the fractional part of the internal float
instance Snobol4IntegerClass Snobol4Real where
    mkInteger = Snobol4Integer . round . getReal
    unmkInteger = Snobol4Real . fromIntegral . getInteger
