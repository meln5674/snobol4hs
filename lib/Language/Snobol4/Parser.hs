{-|
Module          : Language.Snobol4.Parser
Description     : Parser for the SNOBOL4 implementation
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

The Parser takes as input a list of SNOBOL4 tokens, which can be found at 
"Language.Snobol4.Lexer.Tokens", and produces a list of statements, which can 
be found at "Language.Snobol4.Syntax.AST", which describe the abstract syntax 
tree of a SNOBOL4 program.
-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Snobol4.Parser
    ( parseFile
    , ParseError
    , SourcePos
    , parse
    , parseT
    ) where


import Data.Functor.Identity

import Control.Monad

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Language.Snobol4.Syntax.AST

import Language.Snobol4.Parser.Types
import Language.Snobol4.Parser.Internal

import qualified Language.Snobol4.Lexer as L

-- | Parse a value in a transformer
parseT :: (Parsable a, Monad m) => String -> m (Either ParseError a)
parseT = runExceptT . (ExceptT . L.lexT False >=> ExceptT . parseFromToksT)

-- | Parse a value
parse :: Parsable a => String -> Either ParseError a
parse = runIdentity . parseT 

-- | Parse a source file
parseFile :: MonadIO m => FilePath -> m (Either L.ParseError Program)
parseFile = liftIO . readFile >=> parseT
