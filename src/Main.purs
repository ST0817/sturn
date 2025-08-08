module Main (main, parseAndExecute) where

import Prelude

import Data.Either (Either(..))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Console (error, logShow)
import Node.EventEmitter (on_)
import Node.ReadLine (close, createConsoleInterface, lineH, noCompletion, prompt, setPrompt)
import Parsing (ParseError, runParser)
import Parsing.String (parseErrorHuman)
import Sturn.Execution (interpret)
import Sturn.Parsing (parseReturnStmt)
import Sturn.Value (Value)

parseAndExecute :: String -> Either ParseError Value
parseAndExecute code = do
  stmt <- runParser code parseReturnStmt
  pure $ interpret stmt

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  setPrompt "❯ " interface
  prompt interface
  interface # on_ lineH case _ of
    ":quit" -> close interface
    "" -> prompt interface
    input -> do
      case parseAndExecute input of
        Right value -> logShow value
        Left err -> error $ joinWith "\n"
          $ parseErrorHuman input 20 err
      prompt interface
