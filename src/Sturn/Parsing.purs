module Sturn.Parsing (parseProgram) where

import Prelude

import Control.Lazy (defer)
import Data.Array (foldl, fromFoldable, singleton)
import Parsing (Parser, position)
import Parsing.Combinators (chainl1, choice)
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
      , reservedOpNames =
          [ "="
          , "+"
          , "\\"
          , "->"
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

-- FuncExpr = "\\" Ident* "->" ( "{" Stmt* "}" ) | Expr
--
-- "\n -> n + 1" is desugared to "\n -> { return n + 1; }"
parseFuncExpr :: SturnParser Expr
parseFuncExpr = FuncExpr
  <$ reservedOp "\\"
  <*> many ident
  <* reservedOp "->"
  <*> body
  where
  body = choice [ block, singleExpr ]
  block = defer \_ -> tokenParser.braces $ many parseStmt
  singleExpr = defer \_ -> singleton <$> ReturnStmt <$> parseExpr

-- TupleExpr = "(" ( Expr ( "," Expr )* )? ")"
--
-- Parenthesized expressions are parsed as tuples with a single element.
-- Its elements are evaluated as a single expression during evaluation.
parseTupleExpr :: SturnParser Expr
parseTupleExpr = defer \_ -> TupleExpr
  <$> fromFoldable
  <$> tokenParser.parens (tokenParser.commaSep parseExpr)

-- Term
--   = IntLit
--   | StrLit
--   | NullLit
--   | VarExpr
--   | FuncExpr
--   | TupleExpr
parseTerm :: SturnParser Expr
parseTerm = defer \_ -> choice
  [ parseIntLit
  , parseStrLit
  , parseNullLit
  , parseVarExpr
  , parseFuncExpr
  , parseTupleExpr
  ]

-- AddExpr = Term ( "+" Term )+
parseAddExpr :: SturnParser Expr
parseAddExpr = defer \_ -> chainl1 parseTerm do
  opPos <- position
  reservedOp "+"
  pure \left -> AddExpr left opPos

-- CallExpr = AddExpr "(" ( Expr ( "," Expr )* )? ")"
parseCallExpr :: SturnParser Expr
parseCallExpr = defer \_ -> do
  firstCallee <- parseAddExpr
  foldl (\callee func -> func callee) firstCallee <$> many do
    parenStartPos <- position
    args <- tokenParser.parens
      $ fromFoldable <$> tokenParser.commaSep parseExpr
    pure \callee -> CallExpr callee parenStartPos args

-- Expr = CallExpr
parseExpr :: SturnParser Expr
parseExpr = defer \_ -> parseCallExpr

-- ReturnStmt = "return" Expr ";"
parseReturnStmt :: SturnParser Stmt
parseReturnStmt = defer \_ -> ReturnStmt
  <$ reserved "return"
  <*> parseExpr
  <* semi

-- VarStmt = "var" Ident "=" Expr ";"
parseVarStmt :: SturnParser Stmt
parseVarStmt = defer \_ -> VarStmt
  <$ reserved "var"
  <*> position
  <*> ident
  <* reservedOp "="
  <*> parseExpr
  <* semi

-- AssignStmt = Ident "=" Expr ";"
parseAssignStmt :: SturnParser Stmt
parseAssignStmt = defer \_ -> AssignStmt
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
parseStmt = defer \_ -> choice
  [ parseReturnStmt
  , parseVarStmt
  , parseAssignStmt
  ]

-- Program = Stmt* EOF
parseProgram :: SturnParser (Array Stmt)
parseProgram = tokenParser.whiteSpace
  *> many parseStmt
  <* eof
