module Sturn.Ast (Expr(..), Stmt(..)) where

data Expr
  = IntLit Int
  | StrLit String
  | NullLit

data Stmt = ReturnStmt Expr
