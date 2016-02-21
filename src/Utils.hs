{-# LANGUAGE RankNTypes #-}
module Utils where

import Lens.Simple
import Control.Monad.State.Strict

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM ifCond thenBranch elseBranch = do
  res <- ifCond
  if res then thenBranch else elseBranch

whenM :: Monad m => m Bool -> m () -> m ()
whenM mBool m = do
  bool <- mBool
  if bool then m else return ()

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mBool m = do
  bool <- mBool
  if bool then return () else m

maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM z f mMaybe = do
  maybe_ <- mMaybe
  case maybe_ of
    Just a -> f a
    Nothing -> z

modifyLens :: MonadState s m => Lens s s b b -> (b -> b) -> m ()
modifyLens l = modify . over l 

getsLens :: MonadState s m => Lens s s b b -> (b -> a) -> m a
getsLens l f = gets (f . view l)

bracketState :: (MonadState s m) => m a -> m a
bracketState m = do
  savedState <- get
  ret <- m
  put savedState
  return ret