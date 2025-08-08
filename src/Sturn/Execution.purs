module Sturn.Execution (interpret) where

import Prelude

import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Data.Array (length, zip)
import Data.Either (Either(..))
import Data.Map (insert, lookup, member)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (modify_, read)
import Parsing (ParseError(..), Position)
import Sturn.Ast (Expr(..), Stmt(..))
import Sturn.Scope (Scope(..), EnvRef, extendScope)
import Sturn.Value (Value(..))

type Evaluator = ExceptT ParseError Effect

findVar :: Scope -> Position -> String -> Evaluator (Value /\ EnvRef)
findVar (Scope { envRef, maybeOuter }) namePos name = do
  env <- liftEffect $ read envRef
  case lookup name env, maybeOuter of
    Just value, _ -> pure $ value /\ envRef
    Nothing, Just outer -> findVar outer namePos name
    Nothing, Nothing -> throwError
      $ ParseError ("Undefined variable: " <> name) namePos

defineVar :: Scope -> String -> Value -> Evaluator (Either Unit Unit)
defineVar (Scope { envRef }) name value = do
  env <- liftEffect $ read envRef
  if member name env then
    pure $ Left unit
  else do
    liftEffect $ modify_ (insert name value) envRef
    pure $ Right unit

evaluate :: Scope -> Expr -> Evaluator Value
evaluate _ (IntLit int) = pure $ IntVal int
evaluate _ (StrLit str) = pure $ StrVal str
evaluate _ NullLit = pure NullVal
evaluate scope (VarExpr namePos name) =
  fst <$> findVar scope namePos name
evaluate _ (FuncExpr params body) = pure $ FuncVal params body
evaluate scope (TupleExpr [ elemExpr ]) = evaluate scope elemExpr
evaluate scope (TupleExpr elemExprs) =
  TupleVal <$> traverse (evaluate scope) elemExprs
evaluate scope (AddExpr leftExpr opPos rightExpr) = do
  leftVal <- evaluate scope leftExpr
  rightVal <- evaluate scope rightExpr
  case leftVal, rightVal of
    IntVal leftInt, IntVal rightInt ->
      pure $ IntVal $ leftInt + rightInt
    _, _ -> throwError $ ParseError "Type missmatch." opPos
evaluate scope (CallExpr calleeExpr parenStartPos argExprs) = do
  calleeVal <- evaluate scope calleeExpr
  case calleeVal of
    FuncVal params body | length argExprs == length params -> do
      scope' <- liftEffect $ extendScope scope
      let
        defineParam (param /\ argExpr) = do
          argVal <- evaluate scope argExpr
          void $ defineVar scope' param argVal
      traverse_ defineParam $ zip params argExprs
      result <- runExceptT $ traverse_ (interpret scope') body
      case result of
        Right _ -> pure $ TupleVal []
        Left returnVal -> pure returnVal
    FuncVal _ _ -> throwError
      $ ParseError "Number of arguments does not match." parenStartPos
    _ -> throwError $ ParseError "Not function." parenStartPos

type Interpreter = ExceptT Value Evaluator

interpret :: Scope -> Stmt -> Interpreter Unit
interpret scope (ReturnStmt expr) = do
  value <- lift $ evaluate scope expr
  throwError value
interpret scope (VarStmt namePos name expr) = do
  value <- lift $ evaluate scope expr
  result <- lift $ defineVar scope name value
  case result of
    Right _ -> pure unit
    Left _ -> lift $ throwError
      $ ParseError ("Variable redefinition: " <> name) namePos
interpret scope (AssignStmt namePos name expr) = lift do
  _ /\ envRef <- findVar scope namePos name
  value <- evaluate scope expr
  liftEffect $ modify_ (insert name value) envRef
