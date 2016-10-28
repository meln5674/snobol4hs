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
{-# LANGUAGE StrictData #-}
module Language.Snobol4.Interpreter.Data.Types where

import Data.Array
import Data.Map
import qualified Data.Map as M
import Data.Monoid
import Data.String

import Language.Snobol4.Syntax.AST

-- | A SNOBOL4 Array
newtype Snobol4Array expr = Snobol4Array { getArray :: Array Snobol4Integer (Data expr) }
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
data Pattern expr
    -- | A pattern which records the matched value in the provided lookup on 
    -- success
    = AssignmentPattern (LazyPattern expr) (Lookup expr)
    -- | A pattern which records the matched value in the provided lookup 
    -- immediately after matching
    | ImmediateAssignmentPattern (LazyPattern expr) (Lookup expr)
    -- | A pattern to match a literal string
    | LiteralPattern Snobol4String
    -- | An alternative between two ore more patterns
    | AlternativePattern (LazyPattern expr) (LazyPattern expr)
    -- | A concatination of two or more patterns
    | ConcatPattern (LazyPattern expr) (LazyPattern expr)
    -- | A pattern which matches any string of N characters
    | LengthPattern (LazyInteger expr)
    -- | A pattern which matches anything
    | EverythingPattern
     -- | A pattern which assigns the cursor position to a variable and matches
    -- the null string
    | HeadPattern (Lookup expr)
    -- | A pattern which matches the longest string containing only certain
    -- characters
    | SpanPattern (LazyString expr)
    -- | A pattern which matches the longest string not containing certain
    -- characters
    | BreakPattern (LazyString expr)
    -- | A pattern which matches one character of a list of characters
    | AnyPattern (LazyString expr)
    -- | A pattern which matches one character not in a list of characters
    | NotAnyPattern (LazyString expr)
    -- | A pattern which succeeds if the cursor is before the given column
    -- measured from the start
    | TabPattern (LazyInteger expr)
    -- | A pattern which succeeds if the cursor is after the given column
    -- measured from the end
    | RTabPattern (LazyInteger expr)
    -- | A pattern which succeeds if the cursor is at the given column measured
    -- from the start
    | PosPattern (LazyInteger expr)
    -- | A pattern which succeeds if the cursor is at the given column measured
    -- from the end
    | RPosPattern (LazyInteger expr)
    -- | A pattern which always fails
    | FailPattern
    -- | A pattern which succeeds the first time, but fails any time after
    | FencePattern
    -- | A pattern which aborts the scanner
    | AbortPattern
    -- | A pattern which matches any string
    | ArbPattern
    -- | A pattern which matches any number of repetitions of another pattern
    | ArbNoPattern (LazyPattern expr)
    -- | A pattern wihch maches any nonnull string with balanced parenthesis
    | BalPattern
    -- | A pattern which matches the null string and prevents backtracking
    | SucceedPattern
  deriving (Show, Eq, Ord)

-- | A SNOBOL4 Table
newtype Snobol4Table expr = Snobol4Table { getTable :: Map (Data expr) (Data expr) } deriving (Show)

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
data Snobol4UserData expr
    = Snobol4UserData
    { datatypeNameUser :: Snobol4String
    , userDataFields :: [Data expr]
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
data Lookup expr
    -- | Lookup a variable by name
    = LookupId Snobol4String
    -- | Lookup an element in an array by index or a table by key
    | LookupAggregate Snobol4String [Data expr]
    -- | The output varaible
    | LookupOutput
    -- | The input variable
    | LookupInput
    -- | The output variable
    | LookupPunch
    -- | A lookup which returns a value
    | LookupLiteral (Data expr)
    -- | A lookup which returns the vale of the specified keyword
    | LookupKeyword Snobol4String
    | LookupUserData UserKey Snobol4String Snobol4Integer
  deriving (Show, Eq, Ord)

-- | Data that may or may not be evaluated
data Lazy expr a
    = 
    -- | An unevaluated thunk, pointing to the expression to evaluate
      Thunk expr
    -- | An evaluated thunk containing the result of evaluating it
    | EvaluatedThunk a
  deriving (Eq,Ord,Show)
 
-- | Apply the function to an evaluated thunk, or ignore it if its unevaluated 
instance Functor (Lazy expr) where
    fmap _ (Thunk expr) = Thunk expr
    fmap f (EvaluatedThunk x) = EvaluatedThunk $ f x

-- | Get the contents of an evaluated thunk or a default value if unevaluated
orLazy :: Lazy expr a -> a -> a
orLazy (Thunk _) x = x
orLazy (EvaluatedThunk x) _ = x

-- | A lazy pattern
type LazyPattern expr = Lazy expr (Pattern expr)

-- | A lazy integer
type LazyInteger expr = Lazy expr Snobol4Integer

-- | A lazy string
type LazyString expr = Lazy expr Snobol4String

-- | A SNOBOL4 Value
data Data expr
    -- | A string
    = StringData Snobol4String
    -- | A pattern reference
    | PatternData PatternKey
    -- | A pattern
    | TempPatternData (Pattern expr)
    -- | An integer
    | IntegerData Snobol4Integer
    -- | A real number
    | RealData Snobol4Real
    -- | An array
    | ArrayData ArrayKey
    -- | A table
    | TableData TableKey
    -- | Passing an expression by name
    | Name (Lookup expr)
    | ExprData expr
    -- | A refernence to a variable
    | ReferenceId Snobol4String
    -- | A reference to a an aggregate, along with the number of arguments
    | ReferenceAggregate Snobol4String [Data expr]
    -- | A referenceTo
    | ReferenceUserData UserKey Snobol4String Snobol4Integer
    -- | A reference to a keyword
    | ReferenceKeyword Snobol4String
    | ReferenceInput
    | ReferenceOutput
    | ReferencePunch
    -- | Object code created using the CODE primitive
    | CodeData CodeKey
    -- | Data of a user-defined type
    | UserData UserKey
  deriving (Eq, Ord)


-- | Return the internal string
instance Show Snobol4String where
    show = show . getString

-- | Convert the internal integer to a string
instance Show Snobol4Integer where
    show = show . getInteger

-- | Convert the internal real to a string
instance Show Snobol4Real where
    show = show . getReal

-- | Convert the internal value to a string if possible, otherwise return the
-- formal identification in brackets
instance Show (Data expr) where
    show (StringData s) = show s
    show (PatternData _) = "[PATTERN]"
    show (TempPatternData (LiteralPattern s)) = show s
    show (TempPatternData _) = "[PATTERN]"
    show (IntegerData i) = show i
    show (RealData f) = show f
    show (ArrayData _) = "[ARRAY]"
    show (TableData _) = "[TABLE]"
    show (Name _) = "[NAME]"
    show (ReferenceId _) = "[REFERENCE]"
    show (ReferenceAggregate _ _) = "[REFERENCE]"
    show (ReferenceUserData _ _ _) = "[REFERENCE]"
    show (ReferenceKeyword _) = "[REFERENCE]"
    show (CodeData _) = "[CODE]"
    show (UserData _) = "[USERDATA]"
    show (ExprData _) = "[EXPRESSION]"

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
