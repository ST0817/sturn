module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Main (parseAndExecute)
import Sturn.Scope (newScope)
import Sturn.Value (Value(..))
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

test :: String -> Value -> Aff Unit
test code expected = do
  scope <- liftEffect $ newScope
  result <- liftEffect $ parseAndExecute scope code
  shouldEqual result $ Left expected

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  it "integer literal" do
    test "return 42;" $ IntVal 42

  it "string literal" do
    test "return \"foo\";" $ StrVal "foo"

  it "null literal" do
    test "return null;" NullVal

  it "variable" do
    let
      code =
        """
        var foo = 42;
        foo = 53;
        return foo;
        """
    test code $ IntVal 53

  it "addition" do
    test "return 42 + 53;" $ IntVal 95

  it "function" do
    let
      code =
        """
        var incr = \n -> n + 1;
        return incr(21);
        """
    test code $ IntVal 22
