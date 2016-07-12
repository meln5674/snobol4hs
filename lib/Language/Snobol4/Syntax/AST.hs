module Language.Snobol4.Syntax.AST where

data Operator
    = Not
    | Question
    | Dollar
    | Dot
    | Bang
    | Percent
    | Star
    | Slash
    | Hash
    | Plus
    | Minus
    | At
    | Pipe
    | And
  deriving Show

data Literal
    = Int Int
    | Real Float
    | String String
  deriving Show

data Expr
    = PrefixExpr Operator Expr
    | IdExpr String
    | LitExpr Literal
    | CallExpr String [Expr]
    | RefExpr String [Expr]
    | ParenExpr Expr
    | BinaryExpr Expr String Expr
    | ConcatExpr Expr Expr
    | NullExpr
  deriving Show
data Goto = Goto Expr | SuccessGoto Expr | FailGoto Expr | BothGoto Expr Expr
  deriving Show
data Stmt
    = AssignStmt (Maybe String) Expr (Maybe Expr) (Maybe Goto)
    | MatchStmt (Maybe String) Expr Expr (Maybe Goto)
    | ReplStmt (Maybe String) Expr Expr (Maybe Expr) (Maybe Goto)
    | DegenStmt (Maybe String) (Maybe Expr) (Maybe Goto)
    | EndStmt (Maybe String)
  deriving Show
