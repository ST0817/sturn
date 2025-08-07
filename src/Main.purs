module Main (main, parseAndEvaluate) where

import Prelude

import Data.Either (Either(..))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Console (error, logShow)
import Node.EventEmitter (on_)
import Node.ReadLine (close, createConsoleInterface, lineH, noCompletion, prompt, setPrompt)
import Parsing (ParseError, runParser)
import Parsing.String (parseErrorHuman)
import Sturn.Evaluation (evaluate)
import Sturn.Parsing (parseIntLit)
import Sturn.Value (Value)

parseAndEvaluate :: String -> Either ParseError Value
parseAndEvaluate code = do
  expr <- runParser code parseIntLit
  pure $ evaluate expr

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  setPrompt "❯ " interface
  prompt interface
  interface # on_ lineH case _ of
    ":quit" -> close interface
    "" -> prompt interface
    input -> do
      case parseAndEvaluate input of
        Right value -> logShow value
        Left err -> error $ joinWith "\n"
          $ parseErrorHuman input 20 err
      prompt interface
