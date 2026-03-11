module PrimitiveExtras.FoldMs where

import Control.Foldl
import PrimitiveExtras.Prelude hiding (fold, foldM)

-- |
-- Given a size of the array,
-- construct a fold, which produces an array of elements.
primArray ::
  (Prim a) =>
  -- | Array size
  Int ->
  FoldM IO a (PrimArray a)
primArray size = FoldM step init extract
  where
    init = Product2 0 <$> newPrimArray size
    step (Product2 index mutable) a = do
      writePrimArray mutable index a
      return (Product2 (succ index) mutable)
    extract (Product2 _ mutable) = unsafeFreezePrimArray mutable
