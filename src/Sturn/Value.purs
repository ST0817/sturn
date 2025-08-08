module Sturn.Value (Value(..)) where

import Prelude

data Value
  = IntVal Int
  | StrVal String

derive instance Eq Value

instance Show Value where
  show (IntVal int) = show int
  show (StrVal str) = str
