module Sturn.Execution (interpret) where

import Prelude

import Control.Monad.Except (lift, runExceptT, throwError)
import Data.Array (length, zip)
import Data.Either (Either(..))
import Data.Map (insert, lookup, member)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (liftEffect)
import Effect.Ref (modify_, read)
import Parsing (ParseError(..), Position)
import Sturn.Scope (extendScope)
import Sturn.Type (EnvRef, Evaluator, Expr(..), Interpreter, Scope(..), Stmt(..), Value(..))

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

evaluate scope (FuncExpr params body) =
  pure $ FuncVal params body scope

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
    FuncVal params body funcScope
      | length argExprs == length params -> do
          funcScope' <- liftEffect $ extendScope funcScope
          let
            defineParam (param /\ argExpr) = do
              argVal <- evaluate scope argExpr
              void $ defineVar funcScope' param argVal
          traverse_ defineParam $ zip params argExprs
          result <- runExceptT $ traverse_ (interpret funcScope') body
          case result of
            Right _ -> pure $ TupleVal []
            Left returnVal -> pure returnVal
    FuncVal _ _ _ -> throwError
      $ ParseError "Number of arguments does not match." parenStartPos
    _ -> throwError $ ParseError "Not function." parenStartPos

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

interpret scope (ExprStmt expr) =
  lift $ void $ evaluate scope expr
