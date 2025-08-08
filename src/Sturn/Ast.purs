module Sturn.Ast (Expr(..), Stmt(..)) where

data Expr = IntLit Int

data Stmt = ReturnStmt Expr
