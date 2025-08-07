module Sturn.Parsing (parseIntLit) where

import Prelude

import Parsing (Parser)
import Parsing.Language (emptyDef)
import Parsing.Token (TokenParser, makeTokenParser)
import Sturn.Ast (Expr(..))

type SturnParser = Parser String

tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

-- IntLit = [0-9]+
parseIntLit :: SturnParser Expr
parseIntLit = IntLit <$> tokenParser.integer
