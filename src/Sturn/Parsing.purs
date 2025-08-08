module Sturn.Parsing (parseReturnStmt) where

import Prelude

import Parsing (Parser)
import Parsing.Combinators (choice)
import Parsing.Language (emptyDef)
import Parsing.Token (TokenParser, makeTokenParser)
import Sturn.Ast (Expr(..), Stmt(..))

type SturnParser = Parser String

tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

-- IntLit = [0-9]+
parseIntLit :: SturnParser Expr
parseIntLit = IntLit <$> tokenParser.integer

-- StrLit = "\"" .* "\""
parseStrLit :: SturnParser Expr
parseStrLit = StrLit <$> tokenParser.stringLiteral

-- Expr
--   = IntLit
--   | StrLit
parseExpr :: SturnParser Expr
parseExpr = choice
  [ parseIntLit
  , parseStrLit
  ]

-- ReturnStmt = "return" Expr ";"
parseReturnStmt :: SturnParser Stmt
parseReturnStmt = ReturnStmt
  <$ tokenParser.reserved "return"
  <*> parseExpr
  <* tokenParser.semi
