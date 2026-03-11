{-# LANGUAGE ScopedTypeVariables #-}

module ROC.ID.Utilities where

import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Maybe
    ( listToMaybe )

guard :: x -> Maybe y -> Either x y
guard x = maybe (Left x) Right

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

maybeToEnum :: forall a . Bounded a => Enum a => Int -> Maybe a
maybeToEnum i
  | i < fromEnum (minBound :: a) = Nothing
  | i > fromEnum (maxBound :: a) = Nothing
  | otherwise                    = pure $ toEnum i

randomBoundedEnum :: forall a m . MonadRandom m => Bounded a => Enum a => m a
randomBoundedEnum =
  toEnum <$> getRandomR (fromEnum (minBound :: a), fromEnum (maxBound :: a))
