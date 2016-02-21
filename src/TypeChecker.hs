{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes #-}

module TypeChecker (typeCheck, mkTypeEnv, prettyPrintType, consElim, checkBranch) where

import qualified Data.Map.Strict as Map
import Control.Monad.Catch
import Control.Monad.State.Strict

import Types
import Utils

typeCheck :: (MonadThrow m, MonadState TypeEnv m) => Expr -> m Type
typeCheck (Var name) = do
  varType <- lookupBinding name
  removeBinding name
  return varType
typeCheck (Lam arg typ body) = do
  addBinding arg typ
  bodyType <- typeCheck body
  assertBindingUsed arg
  return $ Func typ bodyType
typeCheck (App lam arg) = do
  lamType <- typeCheck lam
  argType <- typeCheck arg
  case lamType of
    Func fArgType retType -> 
      if fArgType == argType 
        then return $ retType
        else throwTypeError $ "Function expecting type '" ++ show fArgType ++ "' but got '" ++ show argType ++ "'"
    _ -> throwTypeError "Applied to non-function type."
typeCheck (Cons name) = do
  varType <- lookupBinding name
  return varType
typeCheck (Case caseExpr branches) = do
  caseExprType <- typeCheck caseExpr
  undefined

checkBranch :: (MonadThrow m, MonadState TypeEnv m) => Type -> ConsElim -> Expr -> m Type
checkBranch caseExprType branchElim branchExpr = do
  env <- get
  put $ consElim env caseExprType branchElim
  typeCheck branchExpr

consElim :: TypeEnv -> Type -> ConsElim -> TypeEnv
consElim env (Func t1 t2) (ConsElim consName (b:bindings)) =
  let newEnv = consElim env t1 b in
  consElim newEnv t2 (ConsElim consName bindings)
consElim env (Type t1) (ConsElim consName []) = 
  if Map.lookup consName env == (Just (Type t1))
  then env
  else error "boom 1"
consElim env expectedType (Binding bindingName) = Map.insert bindingName expectedType env
consElim _ _ _ = error "boom"

mkTypeEnv :: [TypeDecl] -> TypeEnv
mkTypeEnv = foldMap mkConsTypeMap

mkConsTypeMap :: TypeDecl -> TypeEnv
mkConsTypeMap (TypeDecl _ consDecls) = foldMap mkConsTypePair consDecls
  where 
    mkConsTypePair (ConsDecl consName consType) = Map.singleton consName (consType)

prettyPrintType :: Type -> String
prettyPrintType (Type t) = t
prettyPrintType (Func t1@(Func _ _) t2) = "(" ++ (prettyPrintType t1) ++ ") -> " ++ (prettyPrintType t2)
prettyPrintType (Func t1 t2) = (prettyPrintType t1) ++ " -> " ++ (prettyPrintType t2)

-- Typechecker helpers

throwTypeError :: MonadThrow m => String -> m a
throwTypeError = throwM . TypeError

lookupBinding :: (MonadThrow m, MonadState TypeEnv m) => String -> m Type
lookupBinding varName =
  maybeM
    (throwTypeError ("Undeclared or already used binding '" ++ varName ++ "'"))
    return
    (gets (Map.lookup varName))

removeBinding :: (MonadThrow m, MonadState TypeEnv m) => String -> m ()
removeBinding name =
  ifM (gets (Map.member name))
      (modify (Map.delete name))
      (throwTypeError $ "Attempted to remove '" ++ name ++ "' from the type environment but it does not exist.")

addBinding :: (MonadThrow m, MonadState TypeEnv m) => String -> Type -> m ()
addBinding name typ =
  ifM (gets (Map.member name))
      (throwTypeError $ "Var already exists '" ++ name ++ "'")
      (modify (Map.insert name typ))

assertBindingUsed :: (MonadThrow m, MonadState TypeEnv m) => String -> m ()
assertBindingUsed name =
  whenM 
    (gets (Map.member name))
    (throwTypeError ("Binding '" ++ name ++ "' not used."))