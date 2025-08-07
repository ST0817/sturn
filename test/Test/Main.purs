module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Main (parseAndEvaluate)
import Sturn.Value (Value(..))
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  it "integer literal" do
    (parseAndEvaluate "42") `shouldEqual` (Right $ IntVal 42)
