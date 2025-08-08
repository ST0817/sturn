module Sturn.Parsing (parseProgram) where

import Prelude

import Parsing (Parser, position)
import Parsing.Combinators (choice)
import Parsing.Combinators.Array (many)
import Parsing.Language (emptyDef)
import Parsing.String (eof)
import Parsing.Token (GenLanguageDef(..), TokenParser, alphaNum, letter, makeTokenParser)
import Sturn.Ast (Expr(..), Stmt(..))

type SturnParser = Parser String

tokenParser :: TokenParser
tokenParser =
  let
    LanguageDef defaultDef = emptyDef
    sturnDef = LanguageDef defaultDef
      { identStart = letter
      , identLetter = alphaNum
      , reservedNames =
          [ "return"
          , "var"
          ]
      }
  in
    makeTokenParser sturnDef

reserved :: String -> SturnParser Unit
reserved = tokenParser.reserved

reservedOp :: String -> SturnParser Unit
reservedOp = tokenParser.reservedOp

semi :: SturnParser String
semi = tokenParser.semi

-- Ident = [a-zA-Z]+
ident :: SturnParser String
ident = tokenParser.identifier

-- IntLit = [0-9]+
parseIntLit :: SturnParser Expr
parseIntLit = IntLit <$> tokenParser.integer

-- StrLit = "\"" .* "\""
parseStrLit :: SturnParser Expr
parseStrLit = StrLit <$> tokenParser.stringLiteral

-- NullLit = "null"
parseNullLit :: SturnParser Expr
parseNullLit = NullLit <$ reserved "null"

-- VarExpr = Ident
parseVarExpr :: SturnParser Expr
parseVarExpr = VarExpr
  <$> position
  <*> ident

-- Expr
--   = IntLit
--   | StrLit
--   | NullLit
--   | VarExpr
parseExpr :: SturnParser Expr
parseExpr = choice
  [ parseIntLit
  , parseStrLit
  , parseNullLit
  , parseVarExpr
  ]

-- ReturnStmt = "return" Expr ";"
parseReturnStmt :: SturnParser Stmt
parseReturnStmt = ReturnStmt
  <$ reserved "return"
  <*> parseExpr
  <* semi

-- VarStmt = "var" Ident "=" Expr ";"
parseVarStmt :: SturnParser Stmt
parseVarStmt = VarStmt
  <$ reserved "var"
  <*> position
  <*> ident
  <* reservedOp "="
  <*> parseExpr
  <* semi

-- AssignStmt = Ident "=" Expr ";"
parseAssignStmt :: SturnParser Stmt
parseAssignStmt = AssignStmt
  <$> position
  <*> ident
  <* reservedOp "="
  <*> parseExpr
  <* semi

-- Stmt
--   = ReturnStmt
--   | VarStmt
--   | AssignStmt
parseStmt :: SturnParser Stmt
parseStmt = choice
  [ parseReturnStmt
  , parseVarStmt
  , parseAssignStmt
  ]

-- Program = Stmt* EOF
parseProgram :: SturnParser (Array Stmt)
parseProgram = tokenParser.whiteSpace
  *> many parseStmt
  <* eof
