module Language.Snobol4.Interpreter.Data.Real 
    ( module Language.Snobol4.Interpreter.Data.Real
    , Snobol4RealClass (..)
    , Snobol4Real
    ) where

import Control.Monad

import Text.Read (readMaybe)

import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Interpreter.Data.String

instance Snobol4RealClass Snobol4Real where
    mkReal = id
    unmkReal = id
instance Snobol4RealClass Float where
    mkReal = Snobol4Real
    unmkReal = getReal
instance Snobol4StringClass Snobol4Real where
    mkString = Snobol4String . show . getReal
    unmkString = Snobol4Real . read . getString

instance Snobol4Read Snobol4Real where
    snobol4Read (Snobol4String s) = liftM Snobol4Real $ readMaybe s
