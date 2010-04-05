{-# LANGUAGE GeneralizedNewtypeDeriving,DeriveFunctor,FlexibleInstances,MultiParamTypeClasses,UndecidableInstances #-}

module Counter (CounterT(CounterT), runCounterT, getAndInc) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity

newtype CounterT s m a = CounterT { unCounterT :: StateT s m a } deriving (Monad,Functor,MonadTrans)

type Counter s = CounterT s Identity

instance Monad m => Applicative (StateT s m) where
  pure = return
  (<*>) = ap
instance Monad m => Applicative (CounterT s m) where
  pure = return
  (<*>) = ap

--instance MonadReader r m => MonadReader r (CounterT s m) where
--instance MonadIO m => MonadIO (CounterT s m) where
instance (MonadWriter w m) => MonadWriter w (CounterT s m) where
  tell = lift . tell
  listen m = CounterT (listen (unCounterT m))
  pass m = CounterT (pass (unCounterT m))
instance (MonadState s m) => MonadState s (CounterT c m) where
  get = lift get
  put = lift . put

runCounterT :: Monad m => s -> CounterT s m a -> m a
runCounterT s m = liftM fst (runStateT (unCounterT m) s)

runCounter s m = runIdentity (runCounterT s m)

getAndInc :: Monad m => Enum s => CounterT s m s
getAndInc = CounterT (get <* modify succ)
