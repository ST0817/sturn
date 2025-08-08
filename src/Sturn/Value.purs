module Sturn.Value (Value(..)) where

import Prelude

data Value
  = IntVal Int
  | StrVal String
  | NullVal

derive instance Eq Value

instance Show Value where
  show (IntVal int) = show int
  show (StrVal str) = str
  show NullVal = "null"
