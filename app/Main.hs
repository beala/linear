{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

module Main where

import qualified Data.Map.Strict as Map
import Control.Exception
import Control.Monad.Catch
import Control.Monad.State.Strict
import Text.Parsec (parseTest, parse)

import Types
import LinearParser
import TypeChecker
import Eval

main :: IO ()
main = forever $ do
  ln <- getLine
  print ln
  parseTest parseProg ln

exBindingMap :: Monad m => ValEnv m
exBindingMap =
  Map.fromList [ ("Unit", ConsVal "Unit" []) 
               , ("Pair", LamVal (\x -> return (LamVal (\y -> return (ConsVal "Pair" [x, y]) ) ) ))
               , ("True", ConsVal "True" [])
               , ("False",ConsVal "False" [])
               ]

exTypeBindingMap :: Map.Map String Type
exTypeBindingMap =
  Map.fromList [ ("Unit", Type "Unit")
               , ("Pair", Func (Type "Bool") (Func (Type "Bool") (Type "BoolPair")))
               , ("True", Type "Bool")
               , ("False", Type "Bool")
               ]

parseAll :: MonadThrow m => String -> String -> m Prog
parseAll source progStr = case parse parseProg source progStr of
  Right p -> return $ p
  Left e -> throwM $ ParseError (show e)

typeCheckProg :: (MonadThrow m, MonadState TypeEnv m) => Prog -> m Type
typeCheckProg (Prog decls expr) = do
  put $ mkTypeEnv decls
  typeCheck expr

evalProg :: MonadThrow m => Prog -> m (Val m)
evalProg (Prog decls expr) = 
  let valEnv = mkValEnv decls in
  eval valEnv expr

parseCheckEvalFile'' :: String -> IO ()
parseCheckEvalFile'' s = evalStateT (parseCheckEvalFile' s) Map.empty

parseCheckEvalFile' :: String -> StateT TypeEnv IO ()
parseCheckEvalFile' = parseCheckEvalFile

parseCheckEvalFile :: (MonadIO m, MonadState TypeEnv m, MonadThrow m) => String -> m ()
parseCheckEvalFile filePath = do
  contents <- liftIO $ readFile filePath
  prog <- parseAll filePath contents
  --typeCheckProg prog >>= (liftIO . putStrLn . prettyPrintType)
  evalProg prog >>= (liftIO . print)

parseCheckEval :: String -> String -> IO ()
parseCheckEval typeDeclStr progStr = do
  (Prog _ expr) <- case parse (parseProg) "parseEval" progStr of
      Left e -> fail (show e)
      Right p -> return p
  typeDecls <- case parse (parseTypeDecls) "parseEval" typeDeclStr of
      Left e -> fail (show e)
      Right p -> return p
  putStrLn typeDeclStr
  putStrLn progStr
  let typeEnv = mkTypeEnv typeDecls
  print $ ((evalStateT (typeCheck expr) typeEnv) :: Either SomeException (Type))
  let valEnv = mkValEnv typeDecls
  print $ (((eval valEnv expr)) :: Either SomeException (Val (Either SomeException)))

