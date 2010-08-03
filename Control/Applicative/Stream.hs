{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Applicative.Stream where

import Control.Applicative
import Control.Applicative.Trans
import Control.Applicative.State
import Control.Applicative.Error
import Control.Monad.Error.Class

newtype StreamF t f a = StreamF { unStreamF :: StateT [t] f a } deriving (Functor,Applicative,Alternative,AlternativeError,Monad,MonadPlus,MonadError e)
runStreamF f ts = runStateT (unStreamF f) ts
evalStreamF f ts = evalStateT (unStreamF f) ts

next :: Monad f => StreamF t f (Maybe t)
next = StreamF (get >>= \s -> case s of [] -> pure Nothing; (x:xs) -> put xs >> pure (Just x))
look :: Monad f => StreamF t f (Maybe t)
look = StreamF (get >>= \s -> case s of [] -> pure Nothing; (x:_) -> pure (Just x))
eof :: Monad f => StreamF t f Bool
eof = StreamF (get >>= \s -> case s of [] -> pure True; _ -> pure False)

instance ApplicativeTrans (StreamF t) where
  liftAp = StreamF . liftAp
