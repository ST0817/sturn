module Sturn.Ast (Expr(..), Stmt(..)) where

import Parsing (Position)

data Expr
  = IntLit Int
  | StrLit String
  | NullLit
  | VarExpr Position String

data Stmt
  = ReturnStmt Expr
  | VarStmt Position String Expr
  | AssignStmt Position String Expr
