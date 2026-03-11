module HaskellWorks.Data.Foldable
  ( foldFirst
  , foldLast
  ) where

foldFirst :: Foldable t => t a -> Maybe a
foldFirst = foldr (const . Just) Nothing
{-# INLINE foldFirst #-}

foldLast :: Foldable t => t a -> Maybe a
foldLast = foldl go Nothing
  where go :: Maybe a -> a -> Maybe a
        go _ = Just
{-# INLINE foldLast #-}
