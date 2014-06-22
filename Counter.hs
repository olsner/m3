{-# LANGUAGE GeneralizedNewtypeDeriving,FlexibleInstances,MultiParamTypeClasses,UndecidableInstances #-}

module Counter (CounterT(CounterT), runCounterT, getAndInc) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

newtype CounterT s m a = CounterT { unCounterT :: StateT s m a } deriving (Monad,Functor,MonadTrans,Applicative)

instance MonadReader r m => MonadReader r (CounterT s m) where
  ask = lift ask
  local fun m = CounterT (local fun (unCounterT m))
instance MonadIO m => MonadIO (CounterT s m) where
  liftIO = lift . liftIO
instance (MonadWriter w m) => MonadWriter w (CounterT s m) where
  tell = lift . tell
  listen m = CounterT (listen (unCounterT m))
  pass m = CounterT (pass (unCounterT m))
instance (MonadState s m) => MonadState s (CounterT c m) where
  get = lift get
  put = lift . put

runCounterT :: Monad m => s -> CounterT s m a -> m a
runCounterT s m = liftM fst (runStateT (unCounterT m) s)

getAndInc :: Functor m => Monad m => Enum s => CounterT s m s
getAndInc = CounterT (get <* modify succ)
