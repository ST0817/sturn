module Sturn.Execution (interpret) where

import Sturn.Ast (Expr(..), Stmt(..))
import Sturn.Value (Value(..))

evaluate :: Expr -> Value
evaluate (IntLit int) = IntVal int

interpret :: Stmt -> Value
interpret (ReturnStmt expr) = evaluate expr
