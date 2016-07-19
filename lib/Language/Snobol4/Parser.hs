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


parseT :: (Parsable a, Monad m) => String -> m (Either ParseError a)
parseT = runExceptT . (ExceptT . L.lexT False >=> ExceptT . parseFromToksT)

parse :: Parsable a => String -> Either ParseError a
parse = runIdentity . parseT 

{-
-- | Parse an expression
parseExpression :: String -> Either ParseError Expr
parseExpression = L.lex False >=> parseExpressionFromToks

-- | Parse an expression in a transformer
parseExpressionT :: Monad m => String -> m (Either ParseError Expr)
parseExpressionT 
    = runExceptT 
    . (ExceptT . L.lexT False >=> ExceptT . parseExpressionFromToksT)

-- | Parse a statement
parseStatement :: String -> Either ParseError Stmt
parseStatement = L.lex True >=> parseStatementFromToks

-- | Parse a statement in a transformer
parseStatementT :: Monad m => String -> m (Either ParseError Stmt)
parseStatementT
    = runExceptT 
    . (ExceptT . L.lexT True >=> ExceptT . parseStatementFromToksT)

-- | Parse a program
parseProgram :: String -> Either ParseError Program
parseProgram = L.lex True >=> parseProgramFromToks

-- | Parse a program in a transformer
parseProgramT :: Monad m => String -> m (Either ParseError Program)
parseProgramT
    = runExceptT 
    . (ExceptT . L.lexT True >=> ExceptT . parseProgramFromToksT)
-}

-- | Parse a source file
parseFile :: MonadIO m => FilePath -> m (Either L.ParseError Program)
parseFile = liftIO . readFile >=> parseT
