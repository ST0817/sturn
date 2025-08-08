module Sturn.Execution (Env, interpret) where

import Prelude

import Control.Monad.Except (ExceptT, lift, throwError)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, modify_, read)
import Parsing (ParseError(..), Position)
import Sturn.Ast (Expr(..), Stmt(..))
import Sturn.Value (Value(..))

type Env = Map String Value

type Evaluator = ExceptT ParseError Effect

getVar :: Ref Env -> Position -> String -> Evaluator Value
getVar envRef namePos name = do
  env <- liftEffect $ read envRef
  case lookup name env of
    Just value -> pure value
    Nothing -> throwError
      $ ParseError ("Undefined variable: " <> name) namePos

evaluate :: Ref Env -> Expr -> Evaluator Value
evaluate _ (IntLit int) = pure $ IntVal int
evaluate _ (StrLit str) = pure $ StrVal str
evaluate _ NullLit = pure NullVal
evaluate envRef (VarExpr namePos name) =
  getVar envRef namePos name

type Interpreter = ExceptT Value Evaluator

interpret :: Ref Env -> Stmt -> Interpreter Unit
interpret envRef (ReturnStmt expr) = do
  value <- lift $ evaluate envRef expr
  throwError value
interpret envRef (VarStmt namePos name expr) = do
  env <- liftEffect $ read envRef
  case lookup name env of
    Nothing -> do
      value <- lift $ evaluate envRef expr
      liftEffect $ modify_ (insert name value) envRef
    Just _ -> lift $ throwError
      $ ParseError ("Variable redefinition: " <> name) namePos
interpret envRef (AssignStmt namePos name expr) = do
  _ <- lift $ getVar envRef namePos name
  value <- lift $ evaluate envRef expr
  liftEffect $ modify_ (insert name value) envRef
