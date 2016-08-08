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

import Text.Parsec ( (<|>), ParsecT )
import qualified Text.Parsec as P

import Language.Snobol4.Lexer.Tokens

import Language.Snobol4.Parser.Types

type ParserState = Bool

-- | Modify a token located using parsec source position to use the internal
-- source position
wrapPos :: Located a P.SourcePos -> Located a SourcePos
wrapPos (Located x pos) = Located x $ SourcePos pos

-- | Modify a value using the parsec error type to use the internal one
wrapError :: (a -> b) -> Either P.ParseError a -> Either ParseError b
wrapError _ (Left err) = Left $ ParseError err
wrapError f (Right x) = Right $ f x

-- | Create a parser which returns x if p succeeds
ifthen :: Monad m => m a -> b -> m b
p `ifthen` x = p >> return x

-- | Create a parser which returns x if the character c is parsed
cifthen :: Monad m => Char -> a -> ParsecT String ParserState m a
c `cifthen` x = P.char c `ifthen` x

-- | Create a parser which returns x if the string s is parsed
sifthen :: Monad m => String -> a -> ParsecT String ParserState m a
s `sifthen` x = P.string s `ifthen` x

-- | Parse an opening parenthesis
lParen :: Monad m => ParsecT String ParserState m Token
lParen = '(' `cifthen` LParen

-- | Parse a closing parenthesis
rParen :: Monad m => ParsecT String ParserState m Token
rParen = ')' `cifthen` RParen

-- | Parse an opening angle bracket
lAngle :: Monad m => ParsecT String ParserState m Token
lAngle = '<' `cifthen` LAngle

-- | Parse a close angle bracket
rAngle :: Monad m => ParsecT String ParserState m Token
rAngle = '>' `cifthen` RAngle

-- | Parse a comma
comma :: Monad m => ParsecT String ParserState m Token
comma = ',' `cifthen` Comma

-- | Parse an equals sign
equals :: Monad m => ParsecT String ParserState m Token
equals = '=' `cifthen` Equals

-- | Parse a semicolon
semicolon :: Monad m => ParsecT String ParserState m Token
semicolon = ';' `cifthen` SemiColon

-- | Parse a colon
colon :: Monad m => ParsecT String ParserState m Token
colon = ':' `cifthen` Colon

-- | Parse an end of line
eol :: Monad m => ParsecT String ParserState m Token
eol = void P.endOfLine `ifthen` EOL

-- | Parse a single digit
digit :: Monad m => ParsecT String ParserState m Char
digit = P.digit

-- | Parse a single uppercase letter
letter :: Monad m => ParsecT String ParserState m Char
letter = P.upper

-- | Parse an underscore
underscore :: Monad m => ParsecT String ParserState m Char
underscore = P.char '_'

-- | Parse a dot
dot :: Monad m => ParsecT String ParserState m Char
dot = P.char '.'

-- | Parse a letter or digit
alphanumeric :: Monad m => ParsecT String ParserState m Char
alphanumeric = letter <|> digit

-- | Parse a label, which starts with an alphanumeric character and then 
-- contains any characters other than a semicolon or whitespace
-- This parser fails if not reading from the start of a line
label :: Monad m => ParsecT String ParserState m Token
label = do
    pos <- P.getPosition
    st <- P.getState
    let col = P.sourceColumn pos
    if (col /= 1 && st /= True)
        then P.unexpected "Cannot have a label anywhere but the start of the line"
        else do
            --P.notFollowedBy $ P.string "END"
            c <- alphanumeric
            cs <- P.many (P.noneOf [';',' ','\n'])
            let lbl = c:cs
            when (lbl == "END") $ P.putState True
            return $ Label $ c:cs

-- | Parse an identifier, which starts with a letter and then contains any 
-- number of alphanumeric characters
identifier :: Monad m => ParsecT String ParserState m Token
identifier = do
    c <- letter
    cs <- P.many (alphanumeric <|> dot <|> underscore)
    return $ Identifier $ c : cs

-- | Parse 1 or more whitespace characters
blanks :: Monad m => ParsecT String ParserState m Token
blanks = Blanks <$> (P.many1 $ P.oneOf " \t")

-- | Parse an integer, which contains 1 or more digits
integer' :: Monad m => ParsecT String ParserState m String
integer' = P.many1 digit

-- | Parse an integer and wraps it in an int literal token
integer :: Monad m => ParsecT String ParserState m Token
integer = IntLiteral <$> integer'

-- | Parse a real number, which contains 1 or more digits, a dot, and then 0 or 
-- more digits
real :: Monad m => ParsecT String ParserState m Token
real = do
    intPart <- integer'
    dotPart <- P.char '.'
    fractionPart <- P.option "0" integer'
    return $ RealLiteral $ intPart ++ dotPart : fractionPart

-- | Parse a string literal in single-quotes
sliteral :: Monad m => ParsecT String ParserState m Token
sliteral = do
    start <- P.char '\''
    s <- P.manyTill P.anyChar (P.try $ P.char '\'')
    return $ SLiteral $ start : s ++ "\'"

-- | Parse a string literal in double-quotes
dliteral :: Monad m => ParsecT String ParserState m Token
dliteral = do
    start <- P.char '\"'
    s <- P.manyTill P.anyChar (P.try $ P.char '\"')
    return $ DLiteral $ start : s ++ "\""

-- | Parse an operator
operator :: Monad m => ParsecT String ParserState m Token
operator = do
    opChar <- P.oneOf ['~','?','$','.','!','%','*','/','#','+','-','@','|','&']
    return $ Operator [opChar]

-- | Parse an exponentiate operator
exponentiate :: Monad m => ParsecT String ParserState m Token
exponentiate = "**" `sifthen` Exponentiate

-- | Parse a comment line, which begins with an asterisk, then has any number 
-- of characters until the end of line
-- This parser fails if not reading from the start of a line
comment_line :: Monad m => ParsecT String ParserState m Token
comment_line = do
    pos <- P.getPosition
    let col = P.sourceColumn pos
    if col /= 1
        then P.unexpected "Cannot have a label anywhere but the start of the line"
        else do
            _ <- P.char '*'
            comment <- P.manyTill P.anyChar (P.try (void $ P.lookAhead eol) <|> P.eof)
            return $ LineComment comment

-- | Parse an end of line followed by a continuation character
continuation :: Monad m => ParsecT String ParserState m ()
continuation = void $ eol >> P.oneOf ['+','.']

-- | Modifie a parser so that, if it succeeds, it wraps the result in a Located 
-- value with the position the parser started at
locate :: Monad m => ParsecT String ParserState m a -> ParsecT String ParserState m (Located a P.SourcePos)
locate p = do
    pos <- P.getPosition
    x <- p
    return $ Located x pos

-- | Parse any legal SNOBOL4 token
anyToken :: Monad m => ParsecT String ParserState m Token
anyToken 
     =  lParen 
    <|> rParen
    <|> lAngle
    <|> rAngle
    <|> comma
    <|> equals
    <|> semicolon
    <|> colon
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
    <|> P.try (continuation >> anyToken)
    <|> eol

-- | Parse any number of legal SNOBOL4 tokens
tokens :: Monad m => ParsecT String ParserState m [Located Token P.SourcePos]
tokens = P.many (locate anyToken)

-- Public functions

-- | Produce a list of tokens tagged with their source locations from an input 
-- stream. First parameter sets if the lexing should assume that the input is
-- from the start of a line or not.
lex :: Bool -> String -> Either ParseError [Located Token SourcePos]
lex x = wrapError (map wrapPos) . P.runParser parser False ""
  where
    parser = do
        unless x $ do
            pos <- flip P.incSourceColumn 1 <$> P.getPosition
            P.setPosition pos
        tokens

-- | Produce a list of tokens tagged with their source locations from an input 
-- stream, inside of a monad transformer. First parameter sets if the lexing
-- should assume that the input is from the start of a line or not.
lexT :: Monad m => Bool -> String -> m (Either ParseError [Located Token SourcePos])
lexT x = liftM (wrapError (map wrapPos)) . P.runParserT parser False ""
  where
    parser = do
        unless x $ do
            pos <- flip P.incSourceColumn 1 <$> P.getPosition
            P.setPosition pos
        tokens
