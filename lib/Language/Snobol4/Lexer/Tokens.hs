module Language.Snobol4.Lexer.Tokens where

data Token
    = Identifier String
    | Label String
    | Operator String
    | Exponentiate
    | IntLiteral String
    | RealLiteral String
    | SLiteral String
    | DLiteral String
    | Blanks String
    | LParen
    | RParen
    | LAngle
    | RAngle
    | Colon
    | SemiColon
    | Comma
    | Equals
    | EOL
    | LineComment String
  deriving (Show,Eq)

data Located x pos = Located x pos deriving Show

getPos (Located _ p) = p

getItem (Located t _) = t

getToken = getItem

isOperator (Operator _) = True
isOperator _ = False

isIdentifier (Identifier _) = True
isIdentifier _ = False

isLabel (Label _) = True
isLabel _ = False

isIntLiteral (IntLiteral _) = True
isIntLiteral _ = False

isRealLiteral (RealLiteral _) = True
isRealLiteral _ = False

isSLiteral (SLiteral _) = True
isSLiteral _ = False

isDLiteral (DLiteral _) = True
isDLiteral _ = False

isBlanks (Blanks _) = True
isBlanks _ = False

isLineComment (LineComment _) = True
isLineComment _ = False

showToken :: Located Token pos -> String
showToken = show . getToken

getIdentifier (Identifier s) = s

