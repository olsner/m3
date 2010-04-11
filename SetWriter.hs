{-# LANGUAGE GeneralizedNewtypeDeriving,MultiParamTypeClasses,FlexibleInstances #-}

module SetWriter (SetWriterT(), runSetWriterT, runSetWriter, SetWriter) where

import Control.Monad.Writer.Class
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

newtype SetWriterT w m a =
  SetWriterT { unWriterT :: StateT w m a } deriving (Monad,Functor,MonadTrans)

instance (Ord el, Monad m) => MonadWriter (Set el) (SetWriterT (Set el) m) where
  tell set = SetWriterT $ modify (`S.union` set)
  listen m = SetWriterT $ do
    -- in StateT (Set el) m a
    -- unWriterT m is too
    a <- unWriterT m
    w <- get
    return (a,w)
  pass m = SetWriterT $ do
    (a,f) <- unWriterT m
    modify f
    return a
instance (Ord el, Monad m) => MonadWriter (Map el a) (SetWriterT (Map el a) m) where
  tell w = SetWriterT $ modify (`M.union` w)
  listen m = SetWriterT $ do
    -- in StateT (Set el) m a
    -- unWriterT m is too
    a <- unWriterT m
    w <- get
    return (a,w)
  pass m = SetWriterT $ do
    (a,f) <- unWriterT m
    modify f
    return a

type SetWriter el = SetWriterT el Identity

runSetWriterT :: s -> SetWriterT s m a -> m (a, s)
runSetWriterT s m = runStateT (unWriterT m) s

runSetWriter :: s -> SetWriter s a -> (a,s)
runSetWriter s m = runIdentity (runSetWriterT s m)
