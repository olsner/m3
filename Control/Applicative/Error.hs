{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Applicative.Error where

import Control.Applicative
import Control.Applicative.Trans
import Control.Monad.Error

newtype ErrorF f a = ErrorF (ErrorT String f a) deriving (Monad,MonadPlus,ApplicativeTrans,MonadError String)
runErrorF (ErrorF (ErrorT t)) = t

class Alternative f => AlternativeError f where
  emptyError :: String -> f a

instance ApplicativeTrans (ErrorT e) where
  liftAp a = ErrorT (fmap Right a)

instance Functor f => Functor (ErrorF f) where
  fmap f (ErrorF (ErrorT x)) = ErrorF (ErrorT (fmap (either Left (Right. f)) x))

instance (Monad f, Functor f) => Applicative (ErrorF f) where
  pure = ErrorF . ErrorT . return . Right
  ErrorF x <*> ErrorF y = ErrorF (x `ap` y)

instance (Monad f, Functor f) => Alternative (ErrorF f) where
  empty = ErrorF (throwError noMsg)
  -- f a -> f a -> f a
  ErrorF x <|> ErrorF y = ErrorF (x `mplus` y)

instance (Monad f, Functor f) => AlternativeError (ErrorF f) where
  emptyError = ErrorF . throwError . strMsg

{-
newtype ErrorF f a = ErrorF { unErrorF :: Either String (f a) }
instance Functor f => Functor (ErrorF f) where
  fmap f (ErrorF g) = ErrorF (fmap (fmap f) g)
instance Applicative f => Applicative (ErrorF f) where
  pure = ErrorF . Right . pure
  x <*> y = error "TODO: Error.hs:13"
instance Applicative f => AlternativeError (ErrorF f) where
  emptyError = ErrorF . Left
instance Applicative f => Alternative (ErrorF f) where
  empty = ErrorF (Left "Alternative.empty")
  ErrorF x <|> ErrorF y = ErrorF $ either (const y) Right x
instance ApplicativeTrans ErrorF where
  liftAp = ErrorF . Right
instance (Applicative f, Monad f) => Monad (ErrorF f) where
  return = pure
  -- ErrorF f a -> (a -> ErrorF f b) -> ErrorF f b
  x >>= y = joinErrorF (fmap y x)
  {-ErrorF $ case x of
    Left err -> Left err
    Right fx -> Right (foo fx y)-}
joinErrorF :: Monad f => ErrorF f (ErrorF f a) -> ErrorF f a
joinErrorF (ErrorF x) = case x of
  Left err -> ErrorF (Left err)
  Right errf -> {- errf :: f (ErrorF f a) -} ErrorF . Right . fmap f $ errf
    -- f :: 

runErrorF :: Applicative f => ErrorF f a -> f (Either String a)
runErrorF (ErrorF f) = either (pure . Left) (fmap Right) f
foo = undefined
-}
