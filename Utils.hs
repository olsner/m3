module Utils where

import Control.Applicative

maybeM :: Applicative m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f (Just x) = Just <$> f x
maybeM _ Nothing = pure Nothing

