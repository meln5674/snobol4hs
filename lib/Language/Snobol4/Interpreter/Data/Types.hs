{-|
Module          : Language.Snobol4.Interpreter.Data.Types
Description     : Underlying types used in the SNOBOL4 datatypes
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Snobol4.Interpreter.Data.Types where

import Data.Array
import Data.Map
import qualified Data.Map as M
import Data.Monoid
import Data.String

import Language.Snobol4.Syntax.AST

-- | A SNOBOL4 Array
newtype Snobol4Array = Snobol4Array { getArray :: Array Snobol4Integer Data }
    deriving (Show)

-- | A SNOBOL4 String
newtype Snobol4String = Snobol4String { getString :: String }
    deriving (Read, Eq, Ord, Monoid, IsString)

-- | A SNOBOL4 Integer
newtype Snobol4Integer = Snobol4Integer { getInteger :: Int }
    deriving (Read, Eq, Ord, Bounded, Enum, Num, Integral, Real, Ix)

-- | A SNOBOL4 Real Number
newtype Snobol4Real = Snobol4Real { getReal :: Float }
    deriving (Read, Eq, Ord, Num, Enum, RealFrac, Real, Fractional, Floating, RealFloat)

-- | A SNOBOL4 Pattern
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
    -- | A pattern wihch maches any nonnull string with balanced parenthesis
    | BalPattern
    -- | A pattern which matches the null string and prevents backtracking
    | SucceedPattern
  deriving (Show, Eq, Ord)

-- | A SNOBOL4 Table
newtype Snobol4Table = Snobol4Table { getTable :: Map Data Data } deriving (Show)

-- | A SNOBOL4 Object Code Value
newtype Snobol4Code = Snobol4Code { getCode :: Program } deriving (Show)

-- | A user-defined datatype
data Snobol4Datatype
    = Snobol4Datatype
    { datatypeName :: Snobol4String
    , datatypeFieldNames :: [Snobol4String]
    }
  deriving (Show)

-- | An instance of a user-defined type
data Snobol4UserData
    = Snobol4UserData
    { datatypeNameUser :: Snobol4String
    , userDataFields :: [Data]
    }
  deriving (Show)

-- | Type of identifiers
newtype Snobol4Identifier = Snobol4Identifier Snobol4String

-- | Key for passing tables by reference
newtype TableKey = TableKey Int deriving (Eq,Ord,Enum,Show)
 
-- | Key for passing patterns by reference
newtype PatternKey = PatternKey Int deriving (Eq,Ord,Enum,Show)
 
-- | Key for passing object code by reference
newtype CodeKey = CodeKey Int deriving (Eq,Ord,Enum,Show)

-- | Key for passing user-defined data type values by reference
newtype UserKey = UserKey Int deriving (Eq,Ord,Enum,Show)
 
-- | Key for passing arrays by reference
newtype ArrayKey = ArrayKey Int deriving (Eq,Ord,Enum,Show)


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

-- | A SNOBOL4 Value
data Data
    -- | A string
    = StringData Snobol4String
    -- | A pattern reference
    | PatternData PatternKey
    -- | A pattern
    | TempPatternData Pattern
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
    -- | Object code created using the CODE primitive
    | CodeData CodeKey
    -- | Data of a user-defined type
    | UserData UserKey
  deriving (Eq, Ord)


-- | 
instance Show Snobol4String where
    show = show . getString

-- | 
instance Show Snobol4Integer where
    show = show . getInteger

-- | 
instance Show Snobol4Real where
    show = show . getReal

-- | 
instance Show Data where
    show (StringData s) = show s
    show (PatternData _) = "[PATTERN]"
    show (IntegerData i) = show i
    show (RealData f) = show f
    show (ArrayData _) = "[ARRAY]"
    show (TableData _) = "[TABLE]"
    show (Name _) = "[NAME]"

-- | Class of types which can be read from a string
class Snobol4Read s where
    snobol4Read :: Snobol4String -> Maybe s

-- | Class of types which can be made to and from integers
class Snobol4IntegerClass i where
    mkInteger :: i -> Snobol4Integer
    unmkInteger :: Snobol4Integer -> i

-- | Class of types which can be made to and from strings
class Snobol4StringClass i where
    mkString :: i -> Snobol4String
    unmkString :: Snobol4String -> i

-- | Class of types which can be made to and from real
class Snobol4RealClass i where
    mkReal :: i -> Snobol4Real
    unmkReal :: Snobol4Real -> i
