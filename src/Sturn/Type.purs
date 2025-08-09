module Sturn.Type
  ( Expr(..)
  , Scope(..)
  , Stmt(..)
  , Value(..)
  , EnvRef
  , Evaluator
  , Interpreter
  ) where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Ref (Ref)
import Parsing (ParseError, Position)

data Expr
  = IntLit Int
  | StrLit String
  | NullLit
  | VarExpr Position String
  | FuncExpr (Array String) (Array Stmt)
  | TupleExpr (Array Expr)
  | AddExpr Expr Position Expr
  | CallExpr Expr Position (Array Expr)

data Stmt
  = ReturnStmt Expr
  | VarStmt Position String Expr
  | AssignStmt Position String Expr
  | ExprStmt Expr

type EnvRef = Ref (Map String Value)

newtype Scope = Scope
  { envRef :: EnvRef
  , maybeOuter :: Maybe Scope
  }

data Value
  = IntVal Int
  | StrVal String
  | NullVal
  | FuncVal (Array String) (Array Stmt) Scope
  | TupleVal (Array Value)

type Evaluator = ExceptT ParseError Effect

type Interpreter = ExceptT Value Evaluator

instance Eq Value where
  eq (IntVal int1) (IntVal int2) = int1 == int2
  eq (StrVal str1) (StrVal str2) = str1 == str2
  eq NullVal NullVal = true
  eq (TupleVal elems1) (TupleVal elems2) = elems1 == elems2
  eq _ _ = false

instance Show Value where
  show (IntVal int) = show int
  show (StrVal str) = str
  show NullVal = "null"
  show (FuncVal _ _ _) = "function"
  show (TupleVal elems) =
    "(" <> joinWith ", " (show <$> elems) <> ")"
