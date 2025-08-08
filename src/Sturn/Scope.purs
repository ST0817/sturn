module Sturn.Scope
  ( EnvRef
  , Scope(..)
  , extendScope
  , newScope
  ) where

import Prelude

import Data.Map (Map, empty)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref, new)
import Sturn.Value (Value)

type EnvRef = Ref (Map String Value)

newtype Scope = Scope
  { envRef :: EnvRef
  , maybeOuter :: Maybe Scope
  }

newScope :: Effect Scope
newScope = do
  envRef <- new empty
  pure $ Scope { envRef, maybeOuter: Nothing }

extendScope :: Scope -> Effect Scope
extendScope outer = do
  envRef <- new empty
  pure $ Scope { envRef, maybeOuter: Just outer }
