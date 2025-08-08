module Main (main, parseAndExecute) where

import Prelude

import Control.Monad.Except (except, runExceptT)
import Data.Either (Either(..))
import Data.Map (empty)
import Data.String (joinWith)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (error, logShow)
import Effect.Ref (Ref, new)
import Node.EventEmitter (on_)
import Node.ReadLine (close, createConsoleInterface, lineH, noCompletion, prompt, setPrompt)
import Parsing (runParser)
import Parsing.String (parseErrorHuman)
import Sturn.Execution (Env, interpret)
import Sturn.Parsing (parseProgram)
import Sturn.Value (Value)

parseAndExecute :: Ref Env -> String -> Effect (Either Value Unit)
parseAndExecute envRef code = do
  interpreted <- runExceptT do
    stmts <- except $ runParser code parseProgram
    runExceptT $ traverse_ (interpret envRef) stmts
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
  envRef <- new empty
  prompt interface
  interface # on_ lineH case _ of
    ":quit" -> close interface
    "" -> prompt interface
    input -> do
      result <- parseAndExecute envRef input
      case result of
        Right _ -> pure unit
        Left value -> logShow value
      prompt interface
