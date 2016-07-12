{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Snobol4.Parser
--    ( parseStatement
--    ) where
    where

import Control.Monad

import Text.Parsec ( (<|>) )
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Lexer.Tokens

maybeThing p x | p x = Just x
               | otherwise = Nothing

nextPos pos _ [] = pos
nextPos pos _ (t:ts) = getPos t

token' f = P.tokenPrim showToken nextPos (maybeThing (f . getToken))

token t = token' (t==)

failureTag = token (Identifier "F")

successTag = token (Identifier "S")

lParen = token LParen

rParen = token RParen

lAngle = token LAngle

rAngle = token RAngle

identifier = token' isIdentifier

operator = token' isOperator

integer = token' isIntLiteral

real = token' isRealLiteral

sliteral = token' isSLiteral

dliteral = token' isDLiteral

inParens = P.between lParen rParen

inAngles = P.between lAngle rAngle

exponentiate = token Exponentiate

colon = token Colon

semicolon = token SemiColon

comma = token Comma

equals = token Equals

star = token (Operator "*")

minus = token (Operator "-")

plus = token (Operator "+")

kwEnd = token (Identifier "END")

kwList = token (Identifier "LIST")

kwUnlist = token (Identifier "UNLIST")

kwEject = token (Identifier "EJECT")

kwLeft = token (Identifier "LEFT")

kwRight = token (Identifier "RIGHT")

blanks = token' isBlanks

label = token' isLabel

comment_line = token' isLineComment


unary = do
    (Located (Operator op) _) <- operator
    return $ case op of
        "~" -> Not
        "?" -> Question
        "$" -> Dollar
        "." -> Dot
        "!" -> Bang
        "%" -> Percent
        "*" -> Star
        "/" -> Slash
        "#" -> Hash
        "+" -> Plus
        "-" -> Minus
        "@" -> At
        "|" -> Pipe
        "&" -> And

binary = do
    blanks
    x <- P.optionMaybe $ do
        x <- P.optionMaybe (operator <|> exponentiate)
        blanks
        return x
    return $ case join x of
        Just (Located (Operator op) _) -> Just op
        Nothing -> Nothing

literal = sliteral <|> dliteral <|> integer <|> real


identifierE = do
    (Located (Identifier s) _) <- identifier
    return $ IdExpr s

literalE = do
    (Located t _) <- literal
    return $ LitExpr $ case t of
        IntLiteral s -> Int (read s)
        RealLiteral s -> Real (read s)
        SLiteral s -> String (read s)
        DLiteral s -> String (read s)

element = do
    prefix <- P.optionMaybe unary
    rest <- identifierE <|> literalE <|> function_call <|> reference <|> inParens expression
    return $ case prefix of
        Just x -> PrefixExpr x rest
        Nothing -> rest

operation = do
    a <- element
    op <- binary
    b <- (element <|> expression)
    return $ case op of
        Just op -> BinaryExpr a op b
        Nothing -> ConcatExpr a b

expression = do
    P.putState False
    P.optional blanks
    x <- P.optionMaybe (element <|> operation)
    P.optional (blanks >> P.putState True)
    return $ case x of
        Just x -> x
        Nothing -> NullExpr

arg_list = P.sepBy1 expression comma

function_call = do
    (Located (Identifier func) _) <- identifier
    args <- inParens arg_list
    return $ CallExpr func args

reference = do
    (Located (Identifier ref) _) <- identifier
    args <- inAngles arg_list
    return $ RefExpr ref args

    
    
    

subject_field = blanks >> element

pattern_field = blanks >> expression

object_field = blanks >> expression

equal = blanks >> equals

goto = Goto <$> (inParens expression <|> inAngles expression)

successGoto = do
    successTag
    Goto sGoto <- goto
    P.optional blanks
    fGoto <- P.optionMaybe (failureTag >> goto)
    return $ case fGoto of
        Just (Goto fGoto) -> BothGoto sGoto fGoto
        Nothing -> SuccessGoto sGoto

failureGoto = do
    failureTag
    Goto fGoto <- goto
    P.optional blanks
    sGoto <- P.optionMaybe (successTag >> goto)
    return $ case sGoto of
        Just (Goto sGoto) -> BothGoto sGoto fGoto
        Nothing -> FailGoto fGoto


goto_field = do
    st <- P.getState
    if st
        then P.optional blanks
        else void blanks
    colon
    P.optional blanks
    goto  <|> successGoto <|> failureGoto

eol = token EOL

eos = void $ P.optional blanks >> ( semicolon <|> eol )

labelStr = do
    (Located (Label s) _) <- label
    return s

assign_statement = do
    lbl <- P.optionMaybe labelStr
    sub <- subject_field
    equal
    obj <- P.optionMaybe object_field
    go <- P.optionMaybe goto_field
    eos
    return $ AssignStmt lbl sub obj go
    
match_statement = do
    lbl <- P.optionMaybe labelStr
    sub <- subject_field
    pat <- pattern_field
    go <- P.optionMaybe goto_field
    eos
    return $ MatchStmt lbl sub pat go

repl_statement = do
    lbl <- P.optionMaybe labelStr
    sub <- subject_field
    pat <- pattern_field
    equal
    obj <- P.optionMaybe object_field 
    go <- P.optionMaybe goto_field
    eos
    return $ ReplStmt lbl sub pat obj go

degen_statement = do
    lbl <- P.optionMaybe labelStr
    sub <- P.optionMaybe subject_field
    go <- P.optionMaybe goto_field
    eos
    return $ DegenStmt lbl sub go

end_statement = do
    kwEnd
    x <- P.optionMaybe $ do
        blanks
        P.optionMaybe $ labelStr <|> (kwEnd >> return "END")
    eos
    return $ EndStmt (join x)

statement = assign_statement <|> match_statement <|> repl_statement <|> degen_statement <|> end_statement


control_line = void $ minus >> P.optional blanks >> 
     (  (kwList >> blanks >> P.optional (kwLeft <|> kwRight) >> P.optional blanks >> eol)
    <|> kwUnlist
    <|> kwEject
     )

continue_line = void $ (plus <|> minus) >> statement -- ???

item = void $ P.optional identifier

item_list = void $ P.sepBy1 comma item

data_prototype = void $ identifier >> inParens item_list

function_prototype = void $ identifier >> inParens item_list >> item_list

external_prototype = void $ identifier >> inParens item_list >> item

signed_integer = void $ P.optional ( P.optional (plus <|> minus) >> integer)

dimension = void $ signed_integer >> P.optional (colon >> signed_integer)

array_prototype = void $ P.sepBy1 comma dimension

string_integer = void $ signed_integer

string_real = void $ P.optional ( P.optional (plus <|> minus) >> real)

string_expression = void $ expression

string_code = void $ P.sepBy1 semicolon statement






-----

parseStatement = P.runParser statement False ""

