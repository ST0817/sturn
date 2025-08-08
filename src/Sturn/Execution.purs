module Sturn.Execution (interpret) where

import Sturn.Ast (Expr(..), Stmt(..))
import Sturn.Value (Value(..))

evaluate :: Expr -> Value
evaluate (IntLit int) = IntVal int
evaluate (StrLit str) = StrVal str
evaluate NullLit = NullVal

interpret :: Stmt -> Value
interpret (ReturnStmt expr) = evaluate expr
