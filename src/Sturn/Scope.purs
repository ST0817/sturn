module Sturn.Scope
  ( extendScope
  , newScope
  ) where

import Prelude

import Data.Map (empty)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (new)
import Sturn.Type (Scope(..))

newScope :: Effect Scope
newScope = do
  envRef <- new empty
  pure $ Scope { envRef, maybeOuter: Nothing }

extendScope :: Scope -> Effect Scope
extendScope outer = do
  envRef <- new empty
  pure $ Scope { envRef, maybeOuter: Just outer }
