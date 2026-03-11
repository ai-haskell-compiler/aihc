{-# LANGUAGE Rank2Types #-}
import Control.Monad.ST (RealWorld)
import Data.Complex (Complex)
import Data.Vector.Storable (MVector(MVector))
import Foreign (Ptr, Storable, withForeignPtr)
import Text.Printf (printf)
import Blas.Generic.Unsafe (Numeric)
import qualified Data.Vector.Storable as Vector
import qualified Blas.Primitive.Types as Blas
import qualified Blas.Generic.Unsafe as Blas
import qualified TestUtils as T

main :: IO ()
main = T.runTest $ do
  sequence_ (mapNumericTypes testGemm)

mapNumericTypes :: (forall a. (Eq a, Numeric a, Show a) => a -> b) -> [b]
mapNumericTypes f =
  [ f (dummy :: Float)
  , f (dummy :: Double)
  , f (dummy :: Complex Float)
  , f (dummy :: Complex Double)
  ]

dummy :: a
dummy = error "dummy value that shouldn't be used"

withMVector :: MVector s a -> (Ptr a -> IO b) -> IO b
withMVector (MVector _ foreignPtr) = withForeignPtr foreignPtr

mVectorFromList :: Storable a => [a] -> IO (MVector RealWorld a)
mVectorFromList = Vector.thaw . Vector.fromList

testGemm :: (Eq a, Numeric a, Show a) => a -> T.Test ()
testGemm numType = do
  let order    = Blas.RowMajor
      transa   = Blas.NoTrans
      transb   = Blas.Trans
      n        = 2
      size     = n * n
      alpha    = 1.0 `asTypeOf` numType
      beta     = 0.0
      a        = Vector.fromList [1, 2, 3, 4]
      b        = Vector.fromList [1, 2, 3, 5]
      expected = Vector.fromList [5, 13, 11, 29]

  c' <- T.liftIO $ do
    c <- mVectorFromList $ take size (repeat 0)
    Vector.unsafeWith a $ \ pa ->
      Vector.unsafeWith b $ \ pb ->
        withMVector c $ \ pc ->
          Blas.gemm order transa transb n n n alpha pa n pb n beta pc n
    Vector.freeze c

  if c' == expected
    then T.passTest "testGemm"
    else T.failTest $ printf "testGemm: c' does not match: %s != %s"
                      (show c') (show expected)
