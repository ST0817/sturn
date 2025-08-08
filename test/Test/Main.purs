module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Main (parseAndExecute)
import Sturn.Value (Value(..))
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

test :: String -> Value -> Aff Unit
test code expected =
  let
    result = parseAndExecute code
  in
    shouldEqual result $ Right expected

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  it "integer literal" do
    test "return 42;" $ IntVal 42

  it "string literal" do
    test "return \"foo\";" $ StrVal "foo"

  it "null literal" do
    test "return null;" NullVal
