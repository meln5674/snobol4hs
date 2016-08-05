{-|
Module          : Language.Snobol4.Syntax.AST
Description     : The SNOBOL4 Abstract Syntax Tree
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

A SNOBOL4 program is represented as a list of statements, which each have:

* A label

* A subject

* A pattern

* An object

* A goto



All fields are optional, but a statement with no subject may not have a pattern 
or object


Subjects, patterns, objects, and goto targets are expressions.


An expression is one of the following:

* A null string

* A literal int, string, or real

* An identifier

* A function call

* A table/array reference

* An expression prefixed by a unary operator

* Two expressions with a binary operator or blanks (concat)

* An unevaluated expression

* Another expression in parenthesis
-}

module Language.Snobol4.Syntax.AST where

-- | All operators, unary or binary
data Operator
    = 
    -- | Negation, unary only
      Not
    -- | Interrogation, unary only
    | Question
    -- | Indirect reference, unary only
    | Dollar
    -- | Pattern assignment when binary, Name when unary
    | Dot
    -- | Exponentiation, binary only
    | Bang
    -- | None
    | Percent
    -- | Multiplication when binary, Unevaluated expression when unary
    | Star
    -- | Division, binary only
    | Slash
    -- | None
    | Hash
    -- | Positive when unary, Addtion when binary
    | Plus
    -- | Negative when unary, Minus when binary
    | Minus
    -- | Cursor position, unary only
    | At
    -- | Pattern alternatives, binary only
    | Pipe
    -- | Keyword, unary only
    | And
    -- | Exponentiation, binary only
    | DoubleStar
    -- | Concat, binary only
    | Blank
  deriving (Show, Eq, Ord)

-- | A literal value
data Literal
    = 
    -- | Integer literal
      Int Int
    -- | Real literal
    | Real Float
    -- | String literal
    | String String
  deriving (Show, Eq, Ord)

-- | An expression
data Expr
    = 
    -- | An expression with a unary prefix operator
      PrefixExpr Operator Expr
    -- | An expression marked to be evaluated later
    | IdExpr String
    -- | A literal
    | LitExpr Literal
    -- | A function call
    | CallExpr String [Expr]
    -- | An array/table reference
    | RefExpr String [Expr]
    -- | An expression in parenthesis
    | ParenExpr Expr
    -- | Two expressions with a binary operator
    | BinaryExpr Expr Operator Expr
    -- | The null expression, represents a zero-length string
    | NullExpr
  deriving (Show, Eq, Ord)

data GotoPart
    = GotoPart Expr
    | DirectGotoPart Expr
  deriving (Show, Eq)

-- | A goto field
data Goto
    =
    -- | Unconditional goto
      Goto GotoPart
    -- | Goto only if statement succeeds
    | SuccessGoto GotoPart
    -- | Goto only if statement fails
    | FailGoto GotoPart
    -- | Goto with both success and failure case
    | BothGoto GotoPart GotoPart
  deriving (Show, Eq)

-- | Statement
data Stmt
    = 
    -- | Normal statement with label, subject, pattern, object, and goto
      Stmt (Maybe String) (Maybe Expr) (Maybe Expr) (Maybe Expr) (Maybe Goto)
    -- | The end statement
    | EndStmt (Maybe String)
  deriving (Show, Eq)

assignStmt :: Maybe String -> Expr -> Expr -> Maybe Goto -> Stmt
assignStmt lbl sub repl goto = Stmt lbl (Just sub) Nothing (Just repl) goto

matchStmt :: Maybe String -> Expr -> Expr -> Maybe Goto -> Stmt
matchStmt lbl sub pat goto = Stmt lbl (Just sub) (Just pat) Nothing goto

replStmt :: Maybe String -> Expr -> Expr -> Expr -> Maybe Goto -> Stmt
replStmt lbl sub pat repl goto = Stmt lbl (Just sub) (Just pat) (Just repl) goto

degenStmt :: Maybe String -> Expr -> Maybe Goto -> Stmt
degenStmt lbl sub goto = Stmt lbl (Just sub) Nothing Nothing goto

-- | A list of statements
newtype Program = Program { getProgram :: [Stmt] }
  deriving (Show, Eq)
