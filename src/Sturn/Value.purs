module Sturn.Value (Value(..)) where

import Prelude

data Value = IntVal Int

derive instance Eq Value

instance Show Value where
  show (IntVal int) = show int
