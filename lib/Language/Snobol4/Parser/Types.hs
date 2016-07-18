{-|
Module          : Language.Snobol4.Parser.Types
Description     : Types used by the parer and lexer
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

This module provides wrappers for the parse error and source position types to
make it opaque to the user what the underlying parser is
-}
module Language.Snobol4.Parser.Types where

import Data.List

import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P

-- | A parser error
newtype ParseError = ParseError P.ParseError deriving Show

-- | A location in a source document
newtype SourcePos = SourcePos P.SourcePos deriving Show

-- | Get the message from a parse error
errorMessage :: ParseError -> String
errorMessage (ParseError e) = intercalate ";" $ map P.messageString (P.errorMessages e)

-- | Get the location of an error
errorPos :: ParseError -> SourcePos
errorPos (ParseError e) = SourcePos $ P.errorPos e

-- | Get the name of the document of a source position
sourceName :: SourcePos -> String
sourceName (SourcePos p) = P.sourceName p

-- | Get the line number of a source position, 0-based
sourceLine :: SourcePos -> Int
sourceLine (SourcePos p) = P.sourceLine p - 1

-- | Get the column number of a source position, 0-based
sourceColumn :: SourcePos -> Int
sourceColumn (SourcePos p) = P.sourceColumn p - 1
