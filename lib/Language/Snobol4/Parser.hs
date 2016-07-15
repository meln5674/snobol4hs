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
    ) where

import Control.Monad

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Text.Parsec ( (<|>) )
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Lexer.Tokens

import qualified Language.Snobol4.Lexer as L

-- Parser utilities

-- | Utility function, returns x if the predicate p on x holds, Nothing 
-- otherwise
maybeThing p x | p x = Just x
               | otherwise = Nothing

-- | Utility function for getting the position of the next token
nextPos pos _ [] = pos
nextPos pos _ (t:ts) = getPos t

-- Parser primitives

-- | Create a parser which accepts a token which satisfies the predicate f
token' p = P.tokenPrim showToken nextPos (maybeThing (p . getToken))

-- | Create a parser which accepts the token t
token t = token' (t==)

-- Parser combinators

-- | Create a parser which applies another parser in between a 
-- matching pair of parenthesis tokens
inParens = P.between lParen rParen

-- | Create a parser which applies another parser in between a 
-- matching pair of angle bracket tokens
inAngles = P.between lAngle rAngle

-- Single tokens

-- | Parse a failure goto tag
failureTag = token (Identifier "F")

-- | Parse a success goto tag
successTag = token (Identifier "S")

-- | Parse an opening parenthesis token
lParen = token LParen

-- | Parse a closing parenthesis token
rParen = token RParen

-- | Parse an opening angle bracket token
lAngle = token LAngle

-- | Parse a closing angle bracket token
rAngle = token RAngle

-- | Parse an identifier token
identifier = token' isIdentifier

-- | Parse an operator token
operator = token' isOperator

-- | Parse an int literal token
integer = token' isIntLiteral

-- | Parse a real literal token
real = token' isRealLiteral

-- | Parse a single-quote string literal token
sliteral = token' isSLiteral

-- | Parse a double-quote string literal token
dliteral = token' isDLiteral

-- | Parse a exponentiate token
exponentiate = token Exponentiate

-- | Parse a colon token
colon = token Colon

-- | Parse a semicolon token
semicolon = token SemiColon

-- | Parse a comma token
comma = token Comma

-- | Parse an equals sign token
equals = token Equals

-- | Parse an asterisk token
star = token (Operator "*")

-- | Parse a minus token
minus = token (Operator "-")

-- | Parse a plus token
plus = token (Operator "+")

-- | Parse an END keyword
kwEnd = token (Identifier "END")

-- | Parse a LIST keyword
kwList = token (Identifier "LIST")

-- | Parse a UNLIST keyword
kwUnlist = token (Identifier "UNLIST")

-- | Parse a EJECT keyword
kwEject = token (Identifier "EJECT")

-- | Parse a LEFT keyword
kwLeft = token (Identifier "LEFT")

-- | Parse a RIGHT keyword
kwRight = token (Identifier "RIGHT")

-- | Parse a blanks token
blanks = token' isBlanks

-- | Parse a label token
label = token' isLabel

-- | Parse a line comment token
comment_line = token' isLineComment

-- Compound parsers

-- | Parse any unary operator
unary = do
    (Located (Operator op) _) <- operator
    return $ case op of
        "~" -> Not
        "?" -> Question
        "$" -> Dollar
        "." -> Dot
        "*" -> Star
        "+" -> Plus
        "-" -> Minus
        "@" -> At
        "&" -> And

-- | Parse either just a blank (concat operator) or a blank followed by any 
-- binary operator and then another blank
binary = do
    blanks
    x <- P.optionMaybe $ do
        x <- P.optionMaybe (operator <|> exponentiate)
        blanks
        return x
    return $ case join x of
        Just (Located (Operator op) _) -> case op of
            "$" -> Just Dollar
            "!" -> Just Bang
            "**" -> Just DoubleStar
            "*" -> Just Star
            "/" -> Just Slash
            "+" -> Just Plus
            "-" -> Just Minus
            "|" -> Just Pipe
        Nothing -> Nothing

-- | Parse any literal
literal =  sliteral 
       <|> dliteral 
       <|> integer 
       <|> real

-- | Parse an identifier and wrap it in an expression
identifierE = do
    (Located (Identifier s) _) <- identifier
    return $ IdExpr s

-- | Prase a literal and wrap it in an expression
literalE = do
    (Located t _) <- literal
    return $ LitExpr $ case t of
        IntLiteral s -> Int (read s)
        RealLiteral s -> Real (read s)
        SLiteral s -> String $ init $ tail $ s
        DLiteral s -> String $ init $ tail $ s

-- | Parse an element, which optionally begins with a unary operator, then is 
-- either an identifier, literal, function call, reference, or an expression in
-- parenthesis
element = do
    prefix <- P.optionMaybe unary
    rest <-  identifierE 
         <|> literalE 
         <|> function_call 
         <|> reference 
         <|> inParens expression
    return $ case prefix of
        Just x -> PrefixExpr x rest
        Nothing -> rest

-- | Parse a binary operator expression
operation = do
    a <- element
    op <- binary
    b <- (P.try expression <|> element)
    return $ case op of
        Just op -> BinaryExpr a op b
        Nothing -> ConcatExpr a b

-- | Parse an expression, which is an element or binary operation, optionally 
-- surounded by blanks
expression = do
    P.putState False
    P.optional blanks
    x <- P.optionMaybe (P.try operation <|> element)
    P.optional (blanks >> P.putState True)
    return $ case x of
        Just x -> x
        Nothing -> NullExpr

-- | Parse an argument list, which is a list of expressions separated by commas
arg_list = P.sepBy1 expression comma

-- | Parse a function call, which is an identifier followed by an argument list 
-- in parenthesis
function_call = do
    (Located (Identifier func) _) <- identifier
    args <- inParens arg_list
    return $ CallExpr func args

-- | Parse a reference, which is an identifier followed by an argument list 
-- in angle brackets
reference = do
    (Located (Identifier ref) _) <- identifier
    args <- inAngles arg_list
    return $ RefExpr ref args

    
    
    
-- | Parse a subject field, which is blanks followed by an element
subject_field = blanks >> element

-- | Parse a pattern field, which is blanks followed by an expression
pattern_field = blanks >> expression

-- | Parse an object fieild, which is blanks followed by an expression
object_field = blanks >> expression

-- | Parse an equals field, which is blanks followed by an equals sign
equal = blanks >> equals

-- | Parse an unconditional goto
goto = Goto <$> (inParens expression <|> inAngles expression)

-- | Parse a conditional goto that begins with the success goto
successGoto = do
    successTag
    Goto sGoto <- goto
    P.optional blanks
    fGoto <- P.optionMaybe (failureTag >> goto)
    return $ case fGoto of
        Just (Goto fGoto) -> BothGoto sGoto fGoto
        Nothing -> SuccessGoto sGoto

-- | Parse a conditional goto that begins with the failure goto
failureGoto = do
    failureTag
    Goto fGoto <- goto
    P.optional blanks
    sGoto <- P.optionMaybe (successTag >> goto)
    return $ case sGoto of
        Just (Goto sGoto) -> BothGoto sGoto fGoto
        Nothing -> FailGoto fGoto

-- | Parse a goto field
goto_field = do
    st <- P.getState
    if st
        then P.optional blanks
        else void blanks
    colon
    P.optional blanks
    goto  <|> successGoto <|> failureGoto

-- | Parse the end of line token
eol = token EOL

-- | Parse the end of a statement, which is a semicolon or end of line, 
-- optionally preceeded by blanks
eos = void $ P.optional blanks >> ( semicolon <|> eol )

-- | Parse a label and extract the text
labelStr = do
    (Located (Label s) _) <- label
    return s

-- | Parse an assignment statement
assign_statement = do
    lbl <- P.optionMaybe labelStr
    sub <- subject_field
    equal
    obj <- P.optionMaybe object_field
    go <- P.optionMaybe goto_field
    eos
    return $ Stmt lbl (Just sub) Nothing obj go
    
-- | Parse a match statement
match_statement = do
    lbl <- P.optionMaybe labelStr
    sub <- subject_field
    pat <- pattern_field
    go <- P.optionMaybe goto_field
    eos
    return $ Stmt lbl (Just sub) (Just pat) Nothing go

-- | Parse a replacement statement
repl_statement = do
    lbl <- P.optionMaybe labelStr
    sub <- subject_field
    pat <- pattern_field
    equal
    obj <- P.optionMaybe object_field 
    go <- P.optionMaybe goto_field
    eos
    return $ Stmt lbl (Just sub) (Just pat) obj go

-- | Parse a degenerate statement
degen_statement = do
    lbl <- P.optionMaybe labelStr
    sub <- P.optionMaybe subject_field
    go <- P.optionMaybe goto_field
    eos
    return $ Stmt lbl sub Nothing Nothing go

-- | Parse an end statement
end_statement = do
    kwEnd
    x <- P.optionMaybe $ do
        blanks
        P.optionMaybe $ labelStr <|> (kwEnd >> return "END")
    eos
    return $ EndStmt (join x)

-- | Parse a statement
statement
    =  P.try end_statement
   <|> P.try assign_statement 
   <|> P.try match_statement 
   <|> P.try repl_statement 
   <|> P.try degen_statement 

-- | Parse a control line
control_line = void $ minus >> P.optional blanks >> 
     (  (kwList >> blanks >> P.optional (kwLeft <|> kwRight) >> P.optional blanks >> eol)
    <|> kwUnlist
    <|> kwEject
     )

-- | Parse a continued line INCOMPLETE
continue_line = void $ (plus <|> minus) >> statement -- ???

-- | Parse an item in a prototype
item = void $ P.optional identifier

-- | Parse a list of items in a prototype
item_list = void $ P.sepBy1 comma item

-- | Parse a data prototype
data_prototype = void $ identifier >> inParens item_list

-- | Parse a function prototype
function_prototype = void $ identifier >> inParens item_list >> item_list

-- | Parse an external prototype
external_prototype = void $ identifier >> inParens item_list >> item

-- | Parse a signed integer in a prototype
signed_integer = void $ P.optional ( P.optional (plus <|> minus) >> integer)

-- | Parse a dimension in a prototype
dimension = void $ signed_integer >> P.optional (colon >> signed_integer)

-- | Parse an array prototype
array_prototype = void $ P.sepBy1 comma dimension

-- | ???
string_integer = void $ signed_integer

-- | ???
string_real = void $ P.optional ( P.optional (plus <|> minus) >> real)

-- | ???
string_expression = void $ expression

-- | ???
string_code = void $ P.sepBy1 semicolon statement

-- | Parse a program
program = P.many $ do
    P.skipMany (comment_line >> eol)
    statement


-- Parsing from token lists

-- | Parse an expression from tokens
parseExpressionFromToks = P.runParser expression False ""

-- | Parse an expression from tokens in a transformer
parseExpressionFromToksT = P.runParserT expression False ""

-- | Parse a statement from tokens
parseStatementFromToks = P.runParser statement False ""

-- | Parse a statement from tokens in a transformer
parseStatementFromToksT = P.runParserT statement False ""

-- | Parse a program from tokens
parseProgramFromToks = P.runParser program False ""

-- | Parse a program from tokens in a transformer
parseProgramFromToksT = P.runParserT program False ""

-- Public functions

-- | Parse an expression
parseExpression = L.lex >=> parseExpressionFromToks

-- | Parse an expression in a transformer
parseExpressionT 
    = runExceptT 
    . (ExceptT . L.lexT >=> ExceptT . parseExpressionFromToksT)

-- | Parse a statement
parseStatement = L.lex >=> parseStatementFromToks

-- | Parse a statement in a transformer
parseStatementT
    = runExceptT 
    . (ExceptT . L.lexT >=> ExceptT . parseStatementFromToksT)

-- | Parse a program
parseProgram = L.lex >=> parseProgramFromToks

-- | Parse a program in a transformer
parseProgramT
    = runExceptT 
    . (ExceptT . L.lexT >=> ExceptT . parseProgramFromToksT)

-- | Parse a source file
parseFile :: MonadIO m => FilePath -> m (Either P.ParseError Program)
parseFile path = liftIO $ do
    code <- readFile path
    return $ parseProgram code
    
