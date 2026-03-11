-- | Construct monoids with do notation.
-- For more information see <https://github.com/tserduke/do-list#readme>.
module Data.DoMonoid
  ( DoMonoid
  , DoMonoidM (DoM)
  , runDoM
  ) where

import GHC.Exts (IsList, IsString, Item, fromList, toList)


-- | The type shortcut.
type DoMonoid m = DoMonoidM m ()

-- | 'DoMonoidM' is not a real instance of 'Monad', 'Applicative' or 'Functor'.
-- It pretends being them with a phantom result type.
newtype DoMonoidM m r = DoM m
  deriving (Eq, Ord, Read, Show, IsString)

-- | Unwrap the monoid.
{-# INLINE runDoM #-}
runDoM :: DoMonoidM m r -> m
runDoM (DoM x) = x


-- | Functor operations are not supported.
instance Functor (DoMonoidM a) where
  fmap = notSupported "fmap"

-- | Applicative operations are not supported.
instance Applicative (DoMonoidM a) where
  pure = notSupported "pure"
  (<*>) = notSupported "(<*>)"

-- | Monadic operations are not supported.
instance (Monoid a) => Monad (DoMonoidM a) where
  (>>=) = notSupported "(>>=)"
  {-# INLINE (>>) #-}
  (DoM x) >> (DoM y) = DoM $ x `mappend` y

instance (IsList m) => IsList (DoMonoidM m r) where
  type Item (DoMonoidM m r) = Item m
  {-# INLINE fromList #-}
  fromList = DoM . fromList
  {-# INLINE toList #-}
  toList = toList . runDoM


notSupported :: String -> a
notSupported func = error $ "DoMonoid " ++ func ++ " is not supported!"
