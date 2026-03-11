-- | Construct lists with do notation.
-- For more information see <https://github.com/tserduke/do-list#readme>.
module Data.DoList
  ( DoList
  , DoListM (DoList)
  -- * Construction
  , item
  -- * List conversion
  , fromList
  , toList
  ) where

import Data.DoMonoid

import GHC.Exts (IsList, IsString, Item, fromString)
import qualified GHC.Exts as E (fromList, toList)


-- | The type shortcut.
type DoList a = DoListM a ()

-- | 'DoListM' is not a real instance of 'Monad', 'Applicative' or 'Functor'.
-- It pretends being them with a phantom result type.
newtype DoListM a r = DoList (DoMonoidM [a] r)
  deriving (Eq, Ord, Read, Show, Functor, Applicative, Monad)

instance (IsString a) => IsString (DoListM a r) where
  {-# INLINE fromString #-}
  fromString = item . fromString

instance IsList (DoListM a r) where
  type Item (DoListM a r) = a
  {-# INLINE fromList #-}
  fromList = fromList
  {-# INLINE toList #-}
  toList = toList

-- | Create a 'DoList' holding a single item.
{-# INLINE item #-}
item :: a -> DoListM a r
item = fromList . pure


-- | Convert from a list.
{-# INLINE fromList #-}
fromList :: [a] -> DoListM a r
fromList = DoList . DoM

-- | Convert to a list.
{-# INLINE toList #-}
toList :: DoListM a r -> [a]
toList (DoList x) = runDoM x
