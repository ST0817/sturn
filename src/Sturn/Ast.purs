module Sturn.Ast (Expr(..), Stmt(..)) where

import Parsing (Position)

data Expr
  = IntLit Int
  | StrLit String
  | NullLit
  | VarExpr Position String
  | FuncExpr (Array String) (Array Stmt)
  | TupleExpr (Array Expr)
  | AddExpr Expr Position Expr
  | CallExpr Expr Position (Array Expr)

data Stmt
  = ReturnStmt Expr
  | VarStmt Position String Expr
  | AssignStmt Position String Expr
