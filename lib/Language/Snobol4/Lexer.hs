{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Snobol4.Lexer
    ( module Language.Snobol4.Lexer.Tokens
    , lex
    , lexT
    ) where

import Prelude hiding (lex)

import Control.Monad

import Text.Parsec ( (<|>) )
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P

import Language.Snobol4.Lexer.Tokens

p `ifthen` x = p >> return x

c `cifthen` x = P.char c `ifthen` x

s `sifthen` x = P.string s `ifthen` x

lParen = '(' `cifthen` LParen

rParen = ')' `cifthen` RParen

lAngle = '<' `cifthen` LAngle

rAngle = '>' `cifthen` RAngle

comma = ',' `cifthen` Comma

equals = '=' `cifthen` Equals

semicolon = ';' `cifthen` SemiColon

colon = ':' `cifthen` Colon

eol = ((void P.endOfLine)) `ifthen` EOL

digit = P.digit

letter = P.upper

underscore = P.char '_'

dot = P.char '.'

alphanumeric = letter <|> digit

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

identifier = do
    head <- letter
    tail <- P.many (alphanumeric <|> dot <|> underscore)
    return $ Identifier $ head : tail

blanks = Blanks <$> P.many1 P.space

integer' = P.many1 digit

integer = IntLiteral <$> integer'

real = do
    intPart <- integer'
    dotPart <- P.char '.'
    fractionPart <- P.option "" integer'
    return $ RealLiteral $ intPart ++ dotPart : fractionPart

sliteral = do
    start <- P.char '\''
    s <- P.manyTill P.anyChar (P.try $ P.char '\'')
    return $ SLiteral $ start : s ++ "\'"

dliteral = do
    start <- P.char '\"'
    s <- P.manyTill P.anyChar (P.try $ P.char '\"')
    return $ DLiteral $ start : s ++ "\""

operator = do
    opChar <- P.oneOf ['~','?','$','.','!','%','*','/','#','+','-','@','|','&']
    return $ Operator $ opChar : []

exponentiate = "**" `sifthen` Exponentiate

comment_line = do
    pos <- P.getPosition
    let col = P.sourceColumn pos
    if col /= 1
        then P.unexpected "Cannot have a label anywhere but the start of the line"
        else do
            head <- P.char '*'
            tail <- P.manyTill P.anyChar (P.try eol)
            return $ LineComment $ head : tail

---

locate p = do
    pos <- P.getPosition
    x <- p
    return $ Located x pos

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
    <|> operator
    <|> exponentiate
    <|> comment_line

tokens = P.many (locate anyToken)

lex :: String -> Either P.ParseError [Located Token P.SourcePos]
lex = P.runParser tokens () ""

lexT :: Monad m => String -> m (Either P.ParseError [Located Token P.SourcePos])
lexT = P.runParserT tokens () ""
