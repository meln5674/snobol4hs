{-|
Module          : Language.Snobol4.Lexer.Tokens
Description     : Tokens produced by the Lexer for the SNOBOL4 implementation
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Lexer.Tokens where

-- | Tokens produced by the lexer
data Token
    -- | A variable or function name
    = Identifier String
    -- | A goto label
    | Label String
    -- | A binary or unary operator
    | Operator String
    -- | The exponentiate operator
    | Exponentiate
    -- | A integer literal
    | IntLiteral String
    -- | A real literal
    | RealLiteral String
    -- | A single-quote string literal
    | SLiteral String
    -- | A double-quote string literal
    | DLiteral String
    -- | One or more blanks
    | Blanks String
    -- | An opening parenthesis
    | LParen
    -- | A closing parenthesis
    | RParen
    -- | An opening angle bracket
    | LAngle
    -- | A closing parenthesis
    | RAngle
    -- | A :
    | Colon
    -- | A ;
    | SemiColon
    -- | A ,
    | Comma
    -- | A =
    | Equals
    -- | An end of line
    | EOL
    -- | A comment line
    | LineComment String
  deriving (Show,Eq)

-- | A wrapper which adds a source position to a token
-- Position type is left polymorphic for flexibility reasons
data Located x pos = Located x pos deriving Show

-- | Get the position from a type wrapped in a Located
getPos :: Located t p -> p
getPos (Located _ p) = p

-- | Get the item wrapped in a Located
getItem :: Located t p -> t
getItem (Located t _) = t

-- | Get the token from a located token
getToken :: Located Token p -> Token
getToken = getItem

-- | Test if a token is an operator
isOperator :: Token -> Bool
isOperator (Operator _) = True
isOperator _ = False

-- | Test if a token is an identifier
isIdentifier :: Token -> Bool
isIdentifier (Identifier _) = True
isIdentifier _ = False

-- | Test if a token is a label
isLabel :: Token -> Bool
isLabel (Label _) = True
isLabel _ = False

-- | Test if a token is an integer literal
isIntLiteral :: Token -> Bool
isIntLiteral (IntLiteral _) = True
isIntLiteral _ = False

-- | Test if a token is a real literal
isRealLiteral :: Token -> Bool
isRealLiteral (RealLiteral _) = True
isRealLiteral _ = False

-- | Test if a token is a single-quote string literal
isSLiteral :: Token -> Bool
isSLiteral (SLiteral _) = True
isSLiteral _ = False

-- | Test if a token is a double-quote string literal
isDLiteral :: Token -> Bool
isDLiteral (DLiteral _) = True
isDLiteral _ = False

-- | Test if a token is a blank
isBlanks :: Token -> Bool
isBlanks (Blanks _) = True
isBlanks _ = False

-- | Test if a token is a line comment
isLineComment :: Token -> Bool
isLineComment (LineComment _) = True
isLineComment _ = False

-- | Show a token
showToken :: Located Token pos -> String
showToken = show . getToken

-- | Get the identifier string from an identifier token
-- This is an error if the token is not an identifier
getIdentifier :: Token -> String
getIdentifier (Identifier s) = s
getIdentifier t = error
    $ "Internal error: Tried to get identifier from non-identifier token " 
    ++ show t

