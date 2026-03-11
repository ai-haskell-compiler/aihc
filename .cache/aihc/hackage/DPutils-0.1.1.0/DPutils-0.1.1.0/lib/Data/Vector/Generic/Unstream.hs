
-- | Helper functions for turnings streams into vectors.
--
-- Mostly very similar to bundle conversion functions from the @vector@
-- package.

module Data.Vector.Generic.Unstream where

import           Control.Monad.ST
import           GHC.Conc (pseq)
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import           System.IO.Unsafe (unsafePerformIO)

-- for testing

import qualified Data.Vector.Unboxed as VU



-- | Turns a stream into a vector.
--
-- TODO insert index checks? Generalized @flag devel@

streamToVectorM :: forall m v a . (Monad m, VG.Vector v a) => SM.Stream m a -> m (v a)
streamToVectorM s = do
  let mv' = unsafePerformIO $ VGM.unsafeNew 1
  let put (v',i) x =
        do let v = unsafePerformIO $ if (i < VGM.length v') then return v' else VGM.unsafeGrow v' (max 1 $ VGM.length v')
           seq (unsafePerformIO $ VGM.unsafeWrite v i x) (return (v,i+1))
      {-# Inline [0] put #-}
  (mv,written) <- SM.foldlM' put (mv',0) s
  mv `pseq` return . unsafePerformIO . VG.freeze $ VGM.unsafeSlice 0 written mv
{-# Inline streamToVectorM #-}

