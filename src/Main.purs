module Main (main, parseAndExecute) where

import Prelude

import Control.Monad.Except (except, runExceptT)
import Data.Either (Either(..))
import Data.String (joinWith)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (error, logShow)
import Node.EventEmitter (on_)
import Node.ReadLine (close, createConsoleInterface, lineH, noCompletion, prompt, setPrompt)
import Parsing (runParser)
import Parsing.String (parseErrorHuman)
import Sturn.Execution (interpret)
import Sturn.Parsing (parseProgram)
import Sturn.Scope (newScope)
import Sturn.Type (Scope, Value)

parseAndExecute :: Scope -> String -> Effect (Either Value Unit)
parseAndExecute scope code = do
  interpreted <- runExceptT do
    stmts <- except $ runParser code parseProgram
    runExceptT $ traverse_ (interpret scope) stmts
  case interpreted of
    Right result -> pure result
    Left err -> do
      liftEffect $ error $ joinWith "\n"
        $ parseErrorHuman code 20 err
      pure $ Right unit

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  setPrompt "❯ " interface
  scope <- newScope
  prompt interface
  interface # on_ lineH case _ of
    ":quit" -> close interface
    "" -> prompt interface
    input -> do
      result <- parseAndExecute scope input
      case result of
        Right _ -> pure unit
        Left value -> logShow value
      prompt interface
