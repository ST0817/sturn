module Sturn.Evaluation (evaluate) where

import Sturn.Ast (Expr(..))
import Sturn.Value (Value(..))

evaluate :: Expr -> Value
evaluate (IntLit int) = IntVal int
