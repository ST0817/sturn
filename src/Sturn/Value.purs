module Sturn.Value (Value(..)) where

import Prelude

import Data.String (joinWith)
import Sturn.Ast (Stmt)

data Value
  = IntVal Int
  | StrVal String
  | NullVal
  | FuncVal (Array String) (Array Stmt)
  | TupleVal (Array Value)

instance Eq Value where
  eq (IntVal int1) (IntVal int2) = int1 == int2
  eq (StrVal str1) (StrVal str2) = str1 == str2
  eq NullVal NullVal = true
  eq (TupleVal elems1) (TupleVal elems2) = elems1 == elems2
  eq _ _ = false

instance Show Value where
  show (IntVal int) = show int
  show (StrVal str) = str
  show NullVal = "null"
  show (FuncVal _ _) = "function"
  show (TupleVal elems) =
    "(" <> joinWith ", " (show <$> elems) <> ")"
