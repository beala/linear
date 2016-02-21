{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Exception
import qualified Data.Map.Strict as Map

data Type = Type String
          | Func Type Type
          deriving (Show, Eq, Ord)

data TypeDecl = TypeDecl String [ConsDecl]
             deriving (Show, Eq, Ord)

data ConsDecl = ConsDecl String Type
              deriving (Show, Eq, Ord)

data Expr = App Expr Expr
          | Lam String Type Expr
          | Case Expr [(ConsElim, Expr)]
          | Cons String
          | Var String
          deriving (Show, Eq, Ord)

data ConsElim = Binding String
              | ConsElim String [ConsElim]
              deriving (Show, Eq, Ord)

--((Cons (Just x)) Nil)


--(Cons "Just" (Unapply (Binding "x")))

--(Cons "Cons" (Unapply (Binding "x")))

--Func        (Type "Int")      (Type "Maybe")
--ConsUnapply (ConsElim "Just") (Binding "x")

--ConsUnapply (ConsUnapply (ConsElim "Cons") (ConsUnapply (ConsElim "Just") (Binding "x"))) (ConsElim "Nil")

--Func Maybe (Func List List)

--Func Unit Maybe

data Prog = Prog [TypeDecl] Expr
          deriving (Show, Eq, Ord)

data Val m = ConsVal String [Val m]
           | LamVal (Val m -> m (Val m))

type ValThrow m = m (Val m)

instance Show (Val m) where
  show (ConsVal s args) = "ConsVal " ++ show s ++ " " ++ show args
  show (LamVal _) = "LamVal <function>"

data EvalError = EvalError String deriving (Show, Eq)

instance Exception EvalError

data TypeError = TypeError String deriving (Show, Eq)

instance Exception TypeError

type ConsName = String
type TypeName = String
data ConsTypeInfo = ConsTypeInfo TypeName [Type] deriving (Show)

data DeclError = DeclError String deriving (Show)
instance Exception DeclError

type TypeEnv = Map.Map String Type
type ConsTypeMap = Map.Map ConsName ConsTypeInfo

data ParseError = ParseError String deriving (Show)

instance Exception ParseError