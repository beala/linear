{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

module Eval where

import qualified Data.Map.Strict as Map
import Control.Monad.Catch
import Control.Applicative ((<|>))

import Types

type ValEnv m = Map.Map String (Val m)

eval :: (MonadThrow m) => ValEnv m -> Expr -> m (Val m)
eval env (Cons name) = lookupVar env name
eval env (Var name) = lookupVar env name
eval env (Lam arg _ body) = return $ LamVal (\x -> eval (Map.insert arg x env) body)
eval env (App lam arg) = do
  lamVal <- eval env lam
  case lamVal of
    LamVal f -> do
      argVal <- eval env arg
      f argVal
    _ -> throwEvalError "Applied to non-value type."
eval env (Case caseExpr branches) = do
  caseVal <- eval env caseExpr
  case matchBranch env caseVal branches of
    Just (branchEnv, branchExpr) -> eval branchEnv branchExpr
    Nothing -> throwEvalError $ "Could not pattern match '" ++ (show caseExpr) ++ "'"

mkValEnv :: Monad m => [TypeDecl] -> ValEnv m
mkValEnv decls = foldMap envForDelc decls
  where envForDelc (TypeDecl _ consDecls) = foldMap consMap consDecls
          where consMap (ConsDecl consName typ) = Map.singleton consName (evalConsDecl consName typ [])

evalConsDecl :: Monad m => String -> Type -> [Val m] -> Val m
evalConsDecl consName (Func _ t) args = LamVal (\arg -> return (evalConsDecl consName t (arg:args)))
evalConsDecl consName (Type _) args = ConsVal consName (reverse args)

matchBranch :: MonadThrow m => ValEnv m -> Val m -> [(ConsElim, Expr)] -> Maybe (ValEnv m, Expr)
matchBranch env val ((b, expr):branches) =
  let newEnv = fmap (\x -> (x, expr)) (elimCons env val b)
      newEnv' = matchBranch env val branches in
      newEnv <|> newEnv'
matchBranch _ _ _ = Nothing

elimCons :: Monad m => ValEnv m -> Val m -> ConsElim -> Maybe (ValEnv m)
elimCons bindings (ConsVal valName (valBinding:valBindings)) (ConsElim elimName (elimBinding:elimBindings)) =
  if elimName == valName
  then do
    m <- elimCons bindings valBinding elimBinding
    elimCons m (ConsVal valName valBindings) (ConsElim elimName elimBindings)
  else Nothing
elimCons bindings (ConsVal valName []) (ConsElim elimName []) =
  if elimName == valName
  then Just bindings
  else Nothing
elimCons bindings val (Binding bindingName) = Just $ Map.insert bindingName val bindings
elimCons _ _ _ = Nothing

lookupVar :: (MonadThrow m) => ValEnv m -> String -> m (Val m)
lookupVar env name = maybe
                  (throwEvalError ("Error Var '" ++ name ++ "' not found."))
                  return
                  (Map.lookup name env)

throwEvalError :: MonadThrow m => String -> m a
throwEvalError = throwM . EvalError

throwDeclError :: MonadThrow m => String -> m a
throwDeclError = throwM . DeclError