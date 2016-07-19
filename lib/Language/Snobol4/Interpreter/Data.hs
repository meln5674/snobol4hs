{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Snobol4.Interpreter.Data 
    ( module Language.Snobol4.Interpreter.Data
    , (<>)
    ) where

import Data.Array
import Data.Map
import Data.Monoid
import Data.String

import Text.Read (readMaybe)

import Control.Monad

import Language.Snobol4.Syntax.AST

newtype Snobol4String = Snobol4String { getString :: String }
    deriving (Read, Eq, Ord, Monoid, IsString)
newtype Snobol4Integer = Snobol4Integer { getInteger :: Int }
    deriving (Read, Eq, Ord, Bounded, Enum, Num, Integral, Real, Ix)
newtype Snobol4Real = Snobol4Real { getReal :: Float }
    deriving (Read, Eq, Ord, Num, Enum, RealFrac, Real, Fractional, Floating, RealFloat)

instance Show Snobol4String where
    show = show . getString

instance Show Snobol4Integer where
    show = show . getInteger

instance Show Snobol4Real where
    show = show . getReal


class Snobol4Show s where
    snobol4Show :: s -> Snobol4String

class Snobol4Read s where
    snobol4Read :: Snobol4String -> Maybe s

--instance Show Snobol4String where
--    show (Snobol4String s) = s

instance Snobol4Show Snobol4String where
    snobol4Show = id

instance Snobol4Show String where
    snobol4Show = Snobol4String

instance Snobol4Show Snobol4Integer where
    snobol4Show = Snobol4String . show

instance Snobol4Show Snobol4Real where
    snobol4Show = Snobol4String . show

instance Snobol4Read Snobol4Integer where
    snobol4Read (Snobol4String s) = liftM Snobol4Integer $ readMaybe s

instance Snobol4Read Snobol4Real where
    snobol4Read (Snobol4String s) = liftM Snobol4Real $ readMaybe s

-- | A lookup request
data Lookup 
    -- | Lookup a variable by name
    = LookupId Snobol4String
    -- | Lookup an element in an array by index or a table by key
    | LookupAggregate Snobol4String [Data]
    -- | The output varaible
    | Output
    -- | The input variable
    | Input
    -- | The output variable
    | Punch
    -- | A lookup which returns a value
    | LookupLiteral Data
  deriving (Show, Eq, Ord)


-- | The data types allowed in a snobol4 program
data Data
    -- | A string
    = StringData Snobol4String
    -- | A pattern
    | PatternData Pattern
    -- | An integer
    | IntegerData Snobol4Integer
    -- | A real number
    | RealData Snobol4Real
    -- | An array
    | ArrayData ArrayKey
    -- | A table
    | TableData TableKey
    -- | Passing an expression by name
    | Name Lookup
  deriving (Eq, Ord)

instance Show Data where
    show (StringData s) = show s
    show (PatternData _) = "[PATTERN]"
    show (IntegerData i) = show i
    show (RealData f) = show f
    show (ArrayData _) = "[ARRAY]"
    show (TableData _) = "[TABLE]"
    show (Name _) = "[NAME]"

data Pattern
    -- | A pattern which records the matched value in the provided lookup on 
    -- success
    = AssignmentPattern Pattern Lookup
    -- | A pattern which records the matched value in the provided lookup 
    -- immediately after matching
    | ImmediateAssignmentPattern Pattern Lookup
    -- | A pattern to match a literal string
    | LiteralPattern Snobol4String
    -- | An alternative between two ore more patterns
    | AlternativePattern Pattern Pattern
    -- | A concatination of two or more patterns
    | ConcatPattern Pattern Pattern
    -- | A pattern which matches any string of N characters
    | LengthPattern Snobol4Integer
    -- | A pattern which matches anything
    | EverythingPattern
    -- | A pattern which contains an unevaluated expression
    | UnevaluatedExprPattern Expr
    -- | A pattern which assigns the cursor position to a variable and matches
    -- the null string
    | HeadPattern Lookup
    -- | A pattern which matches the longest string containing only certain
    -- characters
    | SpanPattern Snobol4String
    -- | A pattern which matches the longest string not containing certain
    -- characters
    | BreakPattern Snobol4String
    -- | A pattern which matches one character of a list of characters
    | AnyPattern Snobol4String
    -- | A pattern which matches one character not in a list of characters
    | NotAnyPattern Snobol4String
    -- | A pattern which succeeds if the cursor is before the given column
    -- measured from the start
    | TabPattern Snobol4Integer
    -- | A pattern which succeeds if the cursor is after the given column
    -- measured from the end
    | RTabPattern Snobol4Integer
    -- | A pattern which succeeds if the cursor is at the given column measured
    -- from the start
    | PosPattern Snobol4Integer
    -- | A pattern which succeeds if the cursor is at the given column measured
    -- from the end
    | RPosPattern Snobol4Integer
    -- | A pattern which always fails
    | FailPattern
    -- | A pattern which succeeds the first time, but fails any time after
    | FencePattern
    -- | A pattern which aborts the scanner
    | AbortPattern
    -- | A pattern which matches any string
    | ArbPattern
    -- | A pattern which matches any number of repetitions of another pattern
    | ArbNoPattern Pattern
  deriving (Show, Eq, Ord)

newtype Snobol4Identifier = Snobol4Identifier Snobol4String

newtype ArrayKey = ArrayKey Int deriving (Eq,Ord,Enum)
newtype TableKey = TableKey Int deriving (Eq,Ord,Enum)
 
newtype Snobol4Array = Snobol4Array { getArray :: Array Snobol4Integer Data }
newtype Snobol4Table = Snobol4Table { getTable :: Map Data Data }

nullString :: Snobol4String
nullString = Snobol4String ""

class Snobol4IntegerClass i where
    mkInteger :: i -> Snobol4Integer
    unmkInteger :: Snobol4Integer -> i

class Snobol4StringClass i where
    mkString :: i -> Snobol4String
    unmkString :: Snobol4String -> i

class Snobol4RealClass i where
    mkReal :: i -> Snobol4Real
    unmkReal :: Snobol4Real -> i

instance Snobol4StringClass Snobol4String where
    mkString = id
    unmkString = id
instance Snobol4StringClass String where
    mkString = Snobol4String
    unmkString = getString

instance Snobol4IntegerClass Snobol4Integer where
    mkInteger = id
    unmkInteger = id
instance Snobol4IntegerClass Int where
    mkInteger = Snobol4Integer
    unmkInteger = getInteger
    
instance Snobol4RealClass Snobol4Real where
    mkReal = id
    unmkReal = id
instance Snobol4RealClass Float where
    mkReal = Snobol4Real
    unmkReal = getReal

snobol4Head :: Snobol4String -> Snobol4String
snobol4Head (Snobol4String s) = Snobol4String $ [head s]

snobol4Length :: Snobol4String -> Snobol4Integer
snobol4Length (Snobol4String s) = Snobol4Integer $ length s

snobol4Take :: Snobol4Integer -> Snobol4String -> Snobol4String
snobol4Take (Snobol4Integer i) (Snobol4String s) = Snobol4String $ take i s

snobol4Drop :: Snobol4Integer -> Snobol4String -> Snobol4String
snobol4Drop (Snobol4Integer i) (Snobol4String s) = Snobol4String $ drop i s

snobol4Elem :: Snobol4String -> Snobol4String -> Bool
snobol4Elem (Snobol4String [c]) (Snobol4String s) = c `elem` s

snobol4NotElem :: Snobol4String -> Snobol4String -> Bool
snobol4NotElem (Snobol4String [c]) (Snobol4String s) = c `notElem` s