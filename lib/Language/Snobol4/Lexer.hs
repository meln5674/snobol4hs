{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module          : Language.Snobol4.Lexer
Description     : Lexer for the SNOBOL4 implementation
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

The lexer takes as input a string containing SNOBOL4 source code,
and returns a list of tokens, which can be found at 
Language.Snobol4.Lexer.Tokens
-}

module Language.Snobol4.Lexer
    ( module Language.Snobol4.Lexer.Tokens
    , lex
    , lexT
    , ParseError
    , SourcePos
    ) where

import Prelude hiding (lex)

import Control.Monad

import Text.Parsec ( (<|>) )
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P

import Language.Snobol4.Lexer.Tokens

import Language.Snobol4.Parser.Types


wrapPos :: Located a P.SourcePos -> Located a SourcePos
wrapPos (Located x pos) = Located x $ SourcePos pos

wrapError :: (a -> b) -> Either P.ParseError a -> Either ParseError b
wrapError _ (Left err) = Left $ ParseError err
wrapError f (Right x) = Right $ f x

-- | Create a parser which returns x if p succeeds
p `ifthen` x = p >> return x

-- | Create a parser which returns x if the character c is parsed
c `cifthen` x = P.char c `ifthen` x

-- | Create a parser which returns x if the string s is parsed
s `sifthen` x = P.string s `ifthen` x

-- | Parse an opening parenthesis
lParen = '(' `cifthen` LParen

-- | Parse a closing parenthesis
rParen = ')' `cifthen` RParen

-- | Parse an opening angle bracket
lAngle = '<' `cifthen` LAngle

-- | Parse a close angle bracket
rAngle = '>' `cifthen` RAngle

-- | Parse a comma
comma = ',' `cifthen` Comma

-- | Parse an equals sign
equals = '=' `cifthen` Equals

-- | Parse a semicolon
semicolon = ';' `cifthen` SemiColon

-- | Parse a colon
colon = ':' `cifthen` Colon

-- | Parse an end of line
eol = ((void P.endOfLine)) `ifthen` EOL

-- | Parse a single digit
digit = P.digit

-- | Parse a single uppercase letter
letter = P.upper

-- | Parse an underscore
underscore = P.char '_'

-- | Parse a dot
dot = P.char '.'

-- | Parse a letter or digit
alphanumeric = letter <|> digit

-- | Parse a label, which starts with an alphanumeric character and then 
-- contains any characters other than a semicolon or whitespace
-- This parser fails if not reading from the start of a line
label = do
    pos <- P.getPosition
    let col = P.sourceColumn pos
    if col /= 1
        then P.unexpected "Cannot have a label anywhere but the start of the line"
        else do
            P.notFollowedBy $ P.string "END"
            head <- alphanumeric
            tail <- P.many (P.noneOf [';',' '])
            return $ Label $ head : tail

-- | Parse an identifier, which starts with a letter and then contains any 
-- number of alphanumeric characters
identifier = do
    head <- letter
    tail <- P.many (alphanumeric <|> dot <|> underscore)
    return $ Identifier $ head : tail

-- | Parse 1 or more whitespace characters
blanks = Blanks <$> P.many1 P.space

-- | Parse an integer, which contains 1 or more digits
integer' = P.many1 digit

-- | Parse an integer and wraps it in an int literal token
integer = IntLiteral <$> integer'

-- | Parse a real number, which contains 1 or more digits, a dot, and then 0 or 
-- more digits
real = do
    intPart <- integer'
    dotPart <- P.char '.'
    fractionPart <- P.option "" integer'
    return $ RealLiteral $ intPart ++ dotPart : fractionPart

-- | Parse a string literal in single-quotes
sliteral = do
    start <- P.char '\''
    s <- P.manyTill P.anyChar (P.try $ P.char '\'')
    return $ SLiteral $ start : s ++ "\'"

-- | Parse a string literal in double-quotes
dliteral = do
    start <- P.char '\"'
    s <- P.manyTill P.anyChar (P.try $ P.char '\"')
    return $ DLiteral $ start : s ++ "\""

-- | Parse an operator
operator = do
    opChar <- P.oneOf ['~','?','$','.','!','%','*','/','#','+','-','@','|','&']
    return $ Operator $ opChar : []

-- | Parse an exponentiate operator
exponentiate = "**" `sifthen` Exponentiate

-- | Parse a comment line, which begins with an asterisk, then has any number 
-- of characters until the end of line
-- This parser fails if not reading from the start of a line
comment_line = do
    pos <- P.getPosition
    let col = P.sourceColumn pos
    if col /= 1
        then P.unexpected "Cannot have a label anywhere but the start of the line"
        else do
            head <- P.char '*'
            tail <- P.manyTill P.anyChar (P.try (void eol) <|> P.eof)
            return $ LineComment $ tail

-- | Modifie a parser so that, if it succeeds, it wraps the result in a Located 
-- value with the position the parser started at
locate p = do
    pos <- P.getPosition
    x <- p
    return $ Located x pos

-- | Parse any legal SNOBOL4 token
anyToken 
     =  lParen 
    <|> rParen
    <|> lAngle
    <|> rAngle
    <|> comma
    <|> equals
    <|> semicolon
    <|> colon
    <|> eol
    <|> blanks
    <|> label
    <|> identifier
    <|> P.try real
    <|> integer
    <|> sliteral
    <|> dliteral
    <|> comment_line
    <|> P.try exponentiate
    <|> operator

-- | Parse any number of legal SNOBOL4 tokens
tokens = P.many (locate anyToken)

-- Public functions

-- | Produce a list of tokens tagged with their source locations from an input 
-- stream. First parameter sets if the lexing should assume that the input is
-- from the start of a line or not.
lex :: Bool -> String -> Either ParseError [Located Token SourcePos]
lex x = wrapError (map wrapPos) . P.runParser (fixPosition >> tokens) () ""
  where
    fixPosition = if x
        then return ()
        else (flip P.incSourceColumn 1 <$> P.getPosition) >>= P.setPosition
        

-- | Produce a list of tokens tagged with their source locations from an input 
-- stream, inside of a monad transformer
lexT :: Monad m => Bool -> String -> m (Either ParseError [Located Token SourcePos])
lexT x = liftM (wrapError (map wrapPos)) . P.runParserT (fixPosition >> tokens) () ""
  where
    fixPosition = if x 
        then return ()
        else (flip P.incSourceColumn 1 <$> P.getPosition) >>= P.setPosition
