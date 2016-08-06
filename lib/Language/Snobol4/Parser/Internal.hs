{-|
Module          : Language.Snobol4.Parser.Internal
Description     : Internal functions for the parser
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Snobol4.Parser.Internal where

import Data.Maybe

import Control.Monad

import Text.Parsec ( (<|>), ParsecT )
import qualified Text.Parsec as P

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Lexer.Tokens
import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Types
import Language.Snobol4.Parser.Types
import Language.Snobol4.Interpreter.Primitives.Prototypes

-- | Stream of tokens
type TokStream = [Located Token SourcePos]

type ParserState = Bool
{-
push :: Bool -> ParserState -> ParserState
push b (ParserState bs) = ParserState (b:bs)

pop :: ParserState -> (Bool,ParserState)
pop (ParserState (b:bs)) = (b,ParserState bs)
-}
-- | Class of types which can be parsed from source
class Parsable a where
    parser :: Monad m => ParsecT TokStream ParserState m a
    startOfLine :: a -> Bool

instance Parsable Expr where
    parser = fixedExpression
    startOfLine = const False

instance Parsable Stmt where
    parser = statement
    startOfLine = const True

instance Parsable ArrayPrototype where
    parser = array_prototype
    startOfLine = const False

instance Parsable FunctionPrototype where
    parser = function_prototype
    startOfLine = const False

instance Parsable DataPrototype where
    parser = data_prototype
    startOfLine = const False

instance Parsable Program where
    parser = program
    startOfLine = const True

-- | Take a token located using a parsec source position and wrap it to use
-- the internal source position
wrapPos :: Located a P.SourcePos -> Located a SourcePos
wrapPos (Located x pos) = Located x $ SourcePos pos

-- | Take a value that is either a parsec error or a result, if it is an error,
-- wrap it using the internal error type
wrapError :: (a -> b) -> Either P.ParseError a -> Either ParseError b
wrapError _ (Left err) = Left $ ParseError err
wrapError f (Right x) = Right $ f x

-- Parser utilities

-- | Utility function, returns x if the predicate p on x holds, Nothing 
-- otherwise
maybeThing :: (a -> Bool) -> a -> Maybe a
maybeThing p x | p x = Just x
               | otherwise = Nothing

-- | Utility function for getting the position of the next token
nextPos :: P.SourcePos -> a -> [Located b SourcePos] -> P.SourcePos
nextPos pos _ [] = pos
nextPos _ _ (t:_) = let SourcePos pos' = getPos t in pos'

-- Parser primitives

-- | Create a parser which accepts a token which satisfies the predicate f
token' :: Monad m 
       => (Token -> Bool) 
       -> P.ParsecT [Located Token SourcePos] ParserState m (Located Token SourcePos)
token' p = P.tokenPrim showToken nextPos (maybeThing (p . getToken)) <* P.putState False

-- | Create a parser which accepts the token t
token :: Monad m 
      => Token 
      -> P.ParsecT [Located Token SourcePos] ParserState m (Located Token SourcePos)
token t = token' (t==)

-- Parser combinators

-- | Create a parser which applies another parser in between a 
-- matching pair of parenthesis tokens
inParens :: Monad m => ParsecT TokStream ParserState m a
                    -> ParsecT TokStream ParserState m a
inParens = P.between lParen rParen

-- | Create a parser which applies another parser in between a 
-- matching pair of angle bracket tokens
inAngles :: Monad m => ParsecT TokStream ParserState m a
                    -> ParsecT TokStream ParserState m a
inAngles = P.between lAngle rAngle

-- Single tokens

-- | Parse a failure goto tag
failureTag :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
failureTag = token (Identifier "F")

-- | Parse a success goto tag
successTag :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
successTag = token (Identifier "S")

-- | Parse an opening parenthesis token
lParen :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
lParen = token LParen

-- | Parse a closing parenthesis token
rParen :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
rParen = token RParen

-- | Parse an opening angle bracket token
lAngle :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
lAngle = token LAngle

-- | Parse a closing angle bracket token
rAngle :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
rAngle = token RAngle

-- | Parse an identifier token
identifier :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
identifier = token' isIdentifier

-- | Parse an operator token
operator :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
operator = token' isOperator

-- | Parse an int literal token
integer :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
integer = token' isIntLiteral

-- | Parse a real literal token
real :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
real = token' isRealLiteral

-- | Parse a single-quote string literal token
sliteral :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
sliteral = token' isSLiteral

-- | Parse a double-quote string literal token
dliteral :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
dliteral = token' isDLiteral

-- | Parse a exponentiate token
exponentiate :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
exponentiate = token Exponentiate

-- | Parse a colon token
colon :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
colon = token Colon

-- | Parse a semicolon token
semicolon :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
semicolon = token SemiColon

-- | Parse a comma token
comma :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
comma = token Comma

-- | Parse an equals sign token
equals :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
equals = token Equals

-- | Parse an asterisk token
star :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
star = token (Operator "*")

-- | Parse a minus token
minus :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
minus = token (Operator "-")

-- | Parse a plus token
plus :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
plus = token (Operator "+")

-- | Parse an END keyword
kwEnd :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
kwEnd = token (Identifier "END")

-- | Parse a LIST keyword
kwList :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
kwList = token (Identifier "LIST")

-- | Parse a UNLIST keyword
kwUnlist :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
kwUnlist = token (Identifier "UNLIST")

-- | Parse a EJECT keyword
kwEject :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
kwEject = token (Identifier "EJECT")

-- | Parse a LEFT keyword
kwLeft :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
kwLeft = token (Identifier "LEFT")

-- | Parse a RIGHT keyword
kwRight :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
kwRight = token (Identifier "RIGHT")

-- | Parse a blanks token
blanks :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
blanks = token' isBlanks <* P.putState True

-- | Parse a label token
label :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
label = token' isLabel

-- | Parse a line comment token
comment_line :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
comment_line = token' isLineComment

optionalBlanks :: Monad m => ParsecT TokStream ParserState m ()
optionalBlanks = P.optional blanks

mandatoryBlanks :: Monad m => ParsecT TokStream ParserState m ()
mandatoryBlanks = do
    st <- P.getState
    if st
        then P.optional blanks
        else void blanks
    return ()

-- Compound parsers

-- | Parse any unary operator
unary :: Monad m => ParsecT TokStream ParserState m Operator
unary = do
    (Located (Operator op) _) <- operator
    case op of
        "~" -> return Not
        "?" -> return Question
        "$" -> return Dollar
        "." -> return Dot
        "*" -> return Star
        "+" -> return Plus
        "-" -> return Minus
        "@" -> return At
        "&" -> return And
        _ -> P.unexpected $ show op

-- | Parse either just a blank (concat operator) or a blank followed by any 
-- binary operator and then another blank
binary :: Monad m => ParsecT TokStream ParserState m (Maybe Operator)
binary = do
    mandatoryBlanks
    x <- P.optionMaybe $ do
        x <- P.optionMaybe (operator <|> exponentiate)
        mandatoryBlanks
        return x
    let y = join x
    case y of
        Just (Located (Operator op) _) -> case op of
            "$" -> return $ Just Dollar
            "!" -> return $ Just Bang
            "*" -> return $ Just Star
            "/" -> return $ Just Slash
            "+" -> return $ Just Plus
            "-" -> return $ Just Minus
            "|" -> return $ Just Pipe
            "." -> return $ Just Dot
            _ -> P.unexpected $ show op ++ "(Expecting a binary operator)"
        Just (Located Exponentiate _) -> return $ Just DoubleStar
        Nothing -> return Nothing
        _ -> error 
            $ "Internal error: Something other than an operator was lexed as an operator: " 
            ++ show y

-- | Parse any literal
literal :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
literal =  sliteral 
       <|> dliteral 
       <|> integer 
       <|> real

-- | Parse an identifier and wrap it in an expression
identifierE :: Monad m => ParsecT TokStream ParserState m Expr
identifierE = do
    (Located (Identifier s) _) <- identifier
    return $ IdExpr s

-- | Prase a literal and wrap it in an expression
literalE :: Monad m => ParsecT TokStream ParserState m Expr
literalE = do
    (Located t _) <- literal
    return $ LitExpr $ case t of
        IntLiteral s -> Int (read s)
        RealLiteral s -> Real (read s)
        SLiteral s -> String $ init $ tail s
        DLiteral s -> String $ init $ tail s
        _ -> error 
            $ "Internal error: Something other than a literal was lexed as a literal: " 
            ++ show t

-- | Parse an element, which optionally begins with a unary operator, then is 
-- either an identifier, literal, function call, reference, or an expression in
-- parenthesis
element :: Monad m => ParsecT TokStream ParserState m Expr
element = do
    prefix <- P.optionMaybe unary
    rest <-  P.try function_call 
         <|> P.try reference 
         <|> identifierE 
         <|> literalE 
         <|> (ParenExpr <$> inParens expression)
    return $ case prefix of
        Just x -> PrefixExpr x rest
        Nothing -> rest

-- | Parse a binary operator expression
operation :: Monad m => ParsecT TokStream ParserState m Expr
operation = do
    a <- element
    op <- binary
    b <- P.try expression <|> element
    case op of
        Just op' -> return $ BinaryExpr a op' b
        Nothing -> case b of
            NullExpr -> do
                return a
            _ -> return $ BinaryExpr a Blank b

-- | Parse an expression, which is an element or binary operation, optionally 
-- surounded by blanks
expression :: Monad m => ParsecT TokStream ParserState m Expr
expression = do
    optionalBlanks
    result <- topExpression
    optionalBlanks
    return result

-- | Parse an expression that is not permitted to be surrounded by blanks
topExpression :: Monad m => ParsecT TokStream ParserState m Expr
topExpression = do
    x <- P.optionMaybe $ P.try operation <|> element
    return $ fromMaybe NullExpr x

-- | Parse an argument list, which is a list of expressions separated by commas
arg_list :: Monad m => ParsecT TokStream ParserState m [Expr]
arg_list = P.sepBy1 expression comma

-- | Parse a function call, which is an identifier followed by an argument list 
-- in parenthesis
function_call :: Monad m => ParsecT TokStream ParserState m Expr
function_call = do
    (Located (Identifier func) _) <- identifier
    args <- inParens arg_list
    return $ CallExpr func args

-- | Parse a reference, which is an identifier followed by an argument list 
-- in angle brackets
reference :: Monad m => ParsecT TokStream ParserState m Expr
reference = do
    (Located (Identifier ref) _) <- identifier
    args <- inAngles arg_list
    return $ RefExpr ref args

    
-- | Parse an expression and then fix the associativity and precedence    
fixedExpression :: Monad m => ParsecT TokStream ParserState m Expr
fixedExpression = liftM (fixPrec . fixAssoc) expression

fixedTopExpression :: Monad m => ParsecT TokStream ParserState m Expr
fixedTopExpression = liftM (fixPrec . fixAssoc) topExpression

-- | Parse a subject field, which is blanks followed by an element
subject_field :: Monad m => ParsecT TokStream ParserState m Expr
subject_field = mandatoryBlanks >> element

-- | Parse a pattern field, which is blanks followed by an expression
pattern_field :: Monad m => ParsecT TokStream ParserState m Expr
pattern_field = mandatoryBlanks >> fixedTopExpression

-- | Parse an object fieild, which is blanks followed by an expression
object_field :: Monad m => ParsecT TokStream ParserState m Expr
object_field = mandatoryBlanks >> fixedTopExpression

-- | Parse an equals field, which is blanks followed by an equals sign
equal :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
equal = mandatoryBlanks >> equals

gotoPart :: Monad m => ParsecT TokStream ParserState m GotoPart
gotoPart =  (GotoPart <$> inParens fixedExpression) 
        <|> (DirectGotoPart <$> inAngles fixedExpression)

-- | Parse an unconditional goto
goto :: Monad m => ParsecT TokStream ParserState m Goto
goto = Goto <$> gotoPart

-- | Parse a conditional goto that begins with the success goto
successGoto :: Monad m => ParsecT TokStream ParserState m Goto
successGoto = do
    _ <- successTag
    sGoto <- gotoPart
    optionalBlanks
    fGoto <- P.optionMaybe (failureTag >> gotoPart)
    return $ case fGoto of
        Just fGoto' -> BothGoto sGoto fGoto'
        Nothing -> SuccessGoto sGoto

-- | Parse a conditional goto that begins with the failure goto
failureGoto :: Monad m => ParsecT TokStream ParserState m Goto
failureGoto = do
    _ <- failureTag
    fGoto <- gotoPart
    optionalBlanks
    sGoto <- P.optionMaybe (successTag >> gotoPart)
    return $ case sGoto of
        Just sGoto' -> BothGoto sGoto' fGoto
        Nothing -> FailGoto fGoto

-- | Parse a goto field
goto_field :: Monad m => ParsecT TokStream ParserState m Goto
goto_field = do
    mandatoryBlanks
    _ <- colon
    optionalBlanks
    goto  <|> successGoto <|> failureGoto

-- | Parse the end of line token
eol :: Monad m => ParsecT TokStream ParserState m (Located Token SourcePos)
eol = token EOL

-- | Parse the end of a statement, which is a semicolon or end of line, 
-- optionally preceeded by blanks
eos :: Monad m => ParsecT TokStream ParserState m ()
eos = void $ optionalBlanks >> ( semicolon <|> eol )

-- | Parse a label and extract the text
labelStr :: Monad m => ParsecT TokStream ParserState m String
labelStr = do
    result <- label
    case result of
        (Located (Label s) _) -> return s
        t -> error 
            $ "Internal Error: Something other than a label was parsed as a label: " 
            ++ show t

-- | Parse an identifier and return the string
identifierStr :: Monad m => ParsecT TokStream ParserState m String
identifierStr = do
    result <- identifier
    case result of
        (Located (Identifier s) _) -> return s
        t -> error 
            $ "Internal Error: Something other than an identifier was parsed as an identifier: " 
            ++ show t
{-
assign_statement_null :: Monad m => ParsecT TokStream ParserState m Stmt
assign_statement_null = do
    lbl <- P.optionMaybe labelStr
    _ <- blanks
    sub <- element
    _ <- blanks
    _ <- equals
    <- P.optionMaybe goto_field
    eos
    return $ Stmt lbl (Just sub) Nothing (Just NullExpr) go
-}  

-- | Parse an assignment statement
assign_statement :: Monad m => ParsecT TokStream ParserState m Stmt
assign_statement = do
    lbl <- P.optionMaybe labelStr
    sub <- subject_field
    _ <- equal
    obj <- P.option NullExpr object_field
    go <- P.optionMaybe goto_field
    eos
    return $ Stmt lbl (Just sub) Nothing (Just obj) go
    
-- | Parse a match statement
match_statement :: Monad m => ParsecT TokStream ParserState m Stmt
match_statement = do
    lbl <- P.optionMaybe labelStr
    sub <- subject_field
    pat <- pattern_field
    go <- P.optionMaybe goto_field
    eos
    return $ Stmt lbl (Just sub) (Just pat) Nothing go

-- | Parse a replacement statement
repl_statement :: Monad m => ParsecT TokStream ParserState m Stmt
repl_statement = do
    lbl <- P.optionMaybe labelStr
    sub <- subject_field
    pat <- pattern_field
    _ <- equal
    obj <- P.option NullExpr object_field 
    go <- P.optionMaybe goto_field
    eos
    return $ Stmt lbl (Just sub) (Just pat) (Just obj) go

-- | Parse a degenerate statement
degen_statement :: Monad m => ParsecT TokStream ParserState m Stmt
degen_statement = do
    lbl <- P.optionMaybe labelStr
    sub <- P.optionMaybe subject_field
    go <- P.optionMaybe goto_field
    eos
    return $ Stmt lbl sub Nothing Nothing go

-- | Parse an end statement
end_statement :: Monad m => ParsecT TokStream ParserState m Stmt
end_statement = do
    _ <- kwEnd
    x <- P.optionMaybe $ do
        mandatoryBlanks
        P.optionMaybe $ labelStr <|> (kwEnd >> return "END")
    eos
    return $ EndStmt (join x)

-- | Parse a statement
statement :: Monad m => ParsecT TokStream ParserState m Stmt
statement
    =  P.try end_statement
   <|> P.try degen_statement 
   <|> P.try assign_statement 
   <|> P.try match_statement 
   <|> P.try repl_statement 

{-
label
subject
label subject
subject pattern
label subject pattern
subject object
label subject object
subject pattern object
label subject pattern object
goto
label goto
subject goto
label subject goto
subject pattern goto
label subject pattern goto
subject object goto
label subject object goto
subject pattern object goto
label subject pattern object goto
-}
{-
labelOnly = do
    lbl <- P.optionMaybe labelStr
    eos
    return $ Stmt lbl Nothing Nothing Nothing Nothing

subjectOnly = do
    _ <- blanks
    sub <- element
    eos
    return $ Stmt Nothing (Just sub) Nothing Nothing Nothing

labelAndSubject = do
    lbl <- P.optionMaybe labelStr
    _ <- blanks
    sub <- element
    eos
    return $ Stmt lbl (Just sub) Nothing Nothing Nothing Nothing

subjectAndPattern = do
    _ <- blanks
    sub <- element
    _ <- blanks
    pat <- topExpression
    eos
    return $ Stmt 
-}
-- | Parse a control line
control_line :: Monad m => ParsecT TokStream ParserState m ()
control_line = void $ minus >> P.optional blanks >> 
     (  (kwList >> blanks >> P.optional (kwLeft <|> kwRight) >> P.optional blanks >> eol)
    <|> kwUnlist
    <|> kwEject
     )

-- | Parse a continued line INCOMPLETE
continue_line :: Monad m => ParsecT TokStream ParserState m ()
continue_line = void $ (plus <|> minus) >> statement -- ???

-- | Parse an item in a prototype
item :: Monad m => ParsecT TokStream ParserState m String
item = P.option "" identifierStr

-- | Parse a list of items in a prototype
item_list :: Monad m => ParsecT TokStream ParserState m [String]
item_list = P.sepBy1 item comma

-- | Parse a data prototype
data_prototype :: Monad m => ParsecT TokStream ParserState m DataPrototype
data_prototype = DataPrototype <$> (mkString <$> identifierStr) <*> (map mkString <$> inParens item_list)

-- | Parse a function prototype
function_prototype :: Monad m => ParsecT TokStream ParserState m FunctionPrototype
function_prototype = FunctionPrototype 
    <$> (mkString <$> identifierStr)
    <*> (map mkString <$> inParens item_list)
    <*> (map mkString <$> item_list)

-- | Parse an external prototype
external_prototype :: Monad m => ParsecT TokStream ParserState m ()
external_prototype = void $ identifier >> inParens item_list >> item

-- | Parse a signed integer in a prototype
signed_integer :: Monad m => ParsecT TokStream ParserState m Int
signed_integer = do
    _ <- P.optional plus
    (Located (IntLiteral i) _) <- integer
    return $ read i

-- | Parse a dimension in a prototype
dimension :: Monad m => ParsecT TokStream ParserState m Dimension
dimension = do
    minIx <- mkInteger <$> signed_integer
    P.option (0,minIx) $ do
        _ <- colon
        maxIx <- mkInteger <$> signed_integer
        return (minIx,maxIx)

-- | Parse an array prototype
array_prototype :: Monad m => ParsecT TokStream ParserState m ArrayPrototype
array_prototype = ArrayPrototype <$> P.sepBy1 dimension comma

-- | ???
string_integer :: Monad m => ParsecT TokStream ParserState m ()
string_integer = void signed_integer

-- | ???
string_real :: Monad m => ParsecT TokStream ParserState m ()
string_real = void $ P.optional ( P.optional (plus <|> minus) >> real)

-- | ???
string_expression :: Monad m => ParsecT TokStream ParserState m ()
string_expression = void expression

-- | ???
string_code :: Monad m => ParsecT TokStream ParserState m ()
string_code = void $ P.sepBy1 semicolon statement

-- | Parse a program
program :: Monad m => ParsecT TokStream ParserState m Program
program = Program <$> P.many (P.skipMany (comment_line >> eol) >> statement)

-- Operator Associativity and Precedence

-- | Get the precidence of an operator
prec :: Operator -> Int
prec Not = 12
prec Question = 12
prec Dollar = 11
prec Dot = 11
prec Bang = 10
prec DoubleStar = 10
prec Percent = 9
prec Star = 8
prec Slash = 7
prec Hash = 6
prec Plus = 5
prec Minus = 5
prec At = 4
prec Blank = 3
prec Pipe = 2
prec And = 1

-- | The associativity of an operator
data Assoc = AssocLeft | AssocRight deriving Eq

-- | Get the associativity of an operator
assoc :: Operator -> Assoc
assoc Not = AssocRight
assoc DoubleStar = AssocRight
assoc _ = AssocLeft

-- | Take an expression tree and manipulate it so that a top-down evalution
-- will respect the precedence of the operators
fixPrec :: Expr -> Expr
fixPrec (PrefixExpr op expr) = PrefixExpr op $ fixPrec expr
fixPrec (CallExpr i args) = CallExpr i $ map fixPrec args
fixPrec (RefExpr i args) = RefExpr i $ map fixPrec args
fixPrec (ParenExpr expr) = ParenExpr $ fixPrec expr
fixPrec (BinaryExpr exprA op exprB) = case fixPrec exprB of
    BinaryExpr exprA' op' exprB'
        | prec op > prec op' -> BinaryExpr (BinaryExpr exprA op exprA') op' exprB'
    _ -> BinaryExpr exprA op exprB
fixPrec x = x

-- | Take an expression tree and manipulate it so that a top-down evaluation
-- will respect the associativity of the operators
fixAssoc :: Expr -> Expr
fixAssoc (BinaryExpr exprA op (BinaryExpr exprA' op' exprB))
    | prec op == prec op' 
        && assoc op == AssocLeft 
        && assoc op' == AssocLeft
        = fixAssoc $ BinaryExpr (BinaryExpr exprA op exprA') op' exprB
fixAssoc (PrefixExpr op expr) = PrefixExpr op $ fixAssoc expr
fixAssoc (CallExpr i args) = CallExpr i $ map fixAssoc args
fixAssoc (RefExpr i args) = RefExpr i $ map fixAssoc args
fixAssoc (ParenExpr expr) = ParenExpr $ fixAssoc expr
fixAssoc x = x


-- | Parse something from a stream of tokens
parseFromToksT :: (Parsable a, Monad m) => TokStream -> m (Either ParseError a)
parseFromToksT = liftM (wrapError id) . P.runParserT (parser <* P.eof) False ""


