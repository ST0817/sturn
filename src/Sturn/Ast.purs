module Sturn.Ast (Expr(..), Stmt(..)) where

data Expr
  = IntLit Int
  | StrLit String

data Stmt = ReturnStmt Expr
