{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
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
module Language.Snobol4.Parser
    ( parseStatement
    , parseStatementT
    , parseProgram
    , parseProgramT
    , parseExpression
    , parseExpressionT
    , parseFile
    , ParseError
    , SourcePos
    ) where


import Control.Monad

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Text.Parsec ( (<|>) )
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Lexer.Tokens

import Language.Snobol4.Parser.Types
import Language.Snobol4.Parser.Internal

import qualified Language.Snobol4.Lexer as L


-- | Parse an expression
parseExpression = L.lex False >=> parseExpressionFromToks

-- | Parse an expression in a transformer
parseExpressionT 
    = runExceptT 
    . (ExceptT . L.lexT False >=> ExceptT . parseExpressionFromToksT)

-- | Parse a statement
parseStatement = L.lex True >=> parseStatementFromToks

-- | Parse a statement in a transformer
parseStatementT
    = runExceptT 
    . (ExceptT . L.lexT True >=> ExceptT . parseStatementFromToksT)

-- | Parse a program
parseProgram = L.lex True >=> parseProgramFromToks

-- | Parse a program in a transformer
parseProgramT
    = runExceptT 
    . (ExceptT . L.lexT True >=> ExceptT . parseProgramFromToksT)

-- | Parse a source file
parseFile :: MonadIO m => FilePath -> m (Either L.ParseError Program)
parseFile path = liftIO $ do
    code <- readFile path
    return $ parseProgram code
