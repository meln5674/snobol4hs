module Language.Snobol4.Parser.Types where

import qualified Text.Parsec as P

newtype ParseError = ParseError P.ParseError deriving Show

newtype SourcePos = SourcePos P.SourcePos deriving Show
 

