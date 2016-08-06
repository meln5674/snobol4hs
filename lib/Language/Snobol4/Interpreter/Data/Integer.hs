module Language.Snobol4.Interpreter.Data.Integer 
    ( module Language.Snobol4.Interpreter.Data.Integer 
    , Snobol4IntegerClass (..)
    , Snobol4Integer
    ) where

import Control.Monad

import Text.Read (readMaybe)

import Language.Snobol4.Interpreter.Data.Types
import Language.Snobol4.Interpreter.Data.String

instance Snobol4Read Snobol4Integer where
    snobol4Read (Snobol4String s) = liftM Snobol4Integer $ readMaybe s

instance Snobol4StringClass Snobol4Integer where
    mkString = Snobol4String . show . getInteger
    unmkString = Snobol4Integer . read . getString

instance Snobol4IntegerClass Snobol4Integer where
    mkInteger = id
    unmkInteger = id
instance Snobol4IntegerClass Int where
    mkInteger = Snobol4Integer
    unmkInteger = getInteger
