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

next :: Monad f => AlternativeError f => StreamF t f t
next = StreamF (get >>= \s -> case s of [] -> emptyError "Stream.next: EOF"; (x:xs) -> put xs >> pure x)
lookNext :: Monad f => AlternativeError f => StreamF t f t
lookNext = StreamF (get >>= \s -> case s of [] -> emptyError "Stream.lookNext: EOF"; (x:_) -> pure x)
eof :: Monad f => AlternativeError f => StreamF t f ()
eof = StreamF (get >>= \s -> case s of [] -> pure (); _ -> emptyError "Stream.eof: not EOF")

instance ApplicativeTrans (StreamF t) where
  liftAp = StreamF . liftAp
