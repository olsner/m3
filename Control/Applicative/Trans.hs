module Control.Applicative.Trans where

import Control.Applicative

class ApplicativeTrans t where
  liftAp :: Applicative f => f a -> t f a
