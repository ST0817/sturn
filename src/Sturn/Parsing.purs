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

reserved :: String -> SturnParser Unit
reserved = tokenParser.reserved

-- IntLit = [0-9]+
parseIntLit :: SturnParser Expr
parseIntLit = IntLit <$> tokenParser.integer

-- StrLit = "\"" .* "\""
parseStrLit :: SturnParser Expr
parseStrLit = StrLit <$> tokenParser.stringLiteral

-- NullLit = "null"
parseNullLit :: SturnParser Expr
parseNullLit = NullLit <$ reserved "null"

-- Expr
--   = IntLit
--   | StrLit
--   | NullLit
parseExpr :: SturnParser Expr
parseExpr = choice
  [ parseIntLit
  , parseStrLit
  , parseNullLit
  ]

-- ReturnStmt = "return" Expr ";"
parseReturnStmt :: SturnParser Stmt
parseReturnStmt = ReturnStmt
  <$ reserved "return"
  <*> parseExpr
  <* tokenParser.semi
