module AST where

type Name = String
type Args = [Name]

data Expr
   = Func Name Args Expr
   | Var  Name
   | App  Expr [Expr]
   | Add  Expr Expr
   | Lit  Integer
   deriving (Eq, Show)
