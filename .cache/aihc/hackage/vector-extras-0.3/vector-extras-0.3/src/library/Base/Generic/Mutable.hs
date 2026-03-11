module Base.Generic.Mutable where

import Base.Prelude
import Data.Vector.Generic.Mutable hiding (forM_)

{-# INLINE writeListInReverseOrderStartingFrom #-}
writeListInReverseOrderStartingFrom :: (MVector v a) => v s a -> Int -> [a] -> ST s ()
writeListInReverseOrderStartingFrom v =
  let loop !index = \case
        value : tail -> do
          unsafeWrite v index value
          loop (pred index) tail
        _ -> return ()
   in loop

{-# INLINE writeAssocList #-}
writeAssocList :: (MVector v a) => v s a -> [(Int, a)] -> ST s ()
writeAssocList v =
  traverse_ $ \(i, value) -> unsafeWrite v i value
