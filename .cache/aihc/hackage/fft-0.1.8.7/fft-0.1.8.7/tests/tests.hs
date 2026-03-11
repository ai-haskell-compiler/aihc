{-# LANGUAGE FlexibleContexts #-}
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary)
import Data.Array.CArray
         (CArray, IArray, Ix, Shapable, amap, bounds,
          liftArray2, listArray, normSup, rangeSize, shape, slice, Abs)
import Data.Complex (Complex((:+)))
import Math.FFT (dft, dftCR, dftCRO, dftN, dftRC, dftRCN, dht, idft, idftN)
import Math.FFT.Base (FFTWReal)
import Foreign.Storable (Storable)
import Text.Printf (printf)
import System.Environment (getArgs)
import Control.Monad (liftM2, liftM3)


class Ix i => ArbitraryIx i where
    lowerIx :: i
    chooseIx :: Int -> QC.Gen i

instance ArbitraryIx Int where
    lowerIx = 0
    chooseIx n = QC.choose (1,n)

instance (ArbitraryIx i, ArbitraryIx j) => ArbitraryIx (i, j) where
    lowerIx = (lowerIx, lowerIx)
    chooseIx n =
        let n2 = round $ sqrt $ (fromIntegral n :: Double)
        in  liftM2 (,) (chooseIx n2) (chooseIx n2)

instance (ArbitraryIx i, ArbitraryIx j, ArbitraryIx k) => ArbitraryIx (i, j, k) where
    lowerIx = (lowerIx, lowerIx, lowerIx)
    chooseIx n =
        let n3 = round $ (fromIntegral n :: Double) ** (1/3)
        in  liftM3 (,,) (chooseIx n3) (chooseIx n3) (chooseIx n3)


{- |
We need a custom array type
since we want to have sizes of arrays
that we can process in a rather short time.
-}
newtype FFTArray array i e = FFTArray (array i e)
    deriving (Show)

instance (IArray array e, Arbitrary e, ArbitraryIx i) => Arbitrary (FFTArray array i e) where
    arbitrary = do
        u <- chooseIx 1000
        let rng = (lowerIx, u)
        fmap (FFTArray . listArray rng) $ QC.vector (rangeSize rng)


about ::
    (Fractional a, Num e, Ord a, Ix i, IArray array e, Abs e a) =>
    a -> array i e -> array i e -> Bool
about tol x y =
    normSup (liftArray2 (-) x y) / (1 + normSup (liftArray2 (+) x y)) < tol

partAbout ::
    (Fractional a, Num e, Ord a, Ix i, IArray array e, Shapable i, Abs e a) =>
    a -> array i e -> array i e -> Bool
partAbout tol a b = about tol a (slice ba ba b)
    where ba = bounds a

aboutIdem ::
    (Fractional a, Num e, Ord a, Ix i, IArray array e, Abs e a) =>
    (array i e -> array i e) -> a -> array i e -> Bool
aboutIdem f tol x = about tol (f x) x


prop_dft ::
    (FFTWReal e, Abs (Complex e) a, RealFrac a) =>
    a -> CArray Int (Complex e) -> Bool
prop_dft     = aboutIdem $ idft . dft

prop_dftRC, prop_dht_idem ::
    (Fractional a, Ord a, Ix i, Shapable i, FFTWReal e, Abs e a) =>
    a -> CArray i e -> Bool
prop_dftRC tol a =
    aboutIdem ((if odd (shape a !! 0) then dftCRO else dftCR) . dftRC) tol a
prop_dht_idem tol a =
    aboutIdem (amap (/ fromIntegral (shape a !! 0)) . dht . dht) tol a


prop_dft2, prop_dft22, prop_dft22' ::
    (FFTWReal e, Abs (Complex e) e) =>
    e -> CArray (Int, Int) (Complex e) -> Bool

prop_dft2     = aboutIdem $ idft . dft
prop_dft22    = aboutIdem $ idftN [0,1] . dftN [0,1]
prop_dft22'   = aboutIdem $ idftN [1,0] . dftN [1,0]

prop_dftRC_dft, prop_dftRC_dft22 ::
    (Fractional a, Ord a, Ix i, Shapable i, FFTWReal r, Abs (Complex r) a) =>
    a -> CArray i r -> Bool
prop_dftRC_dft tol a =
    partAbout tol (dftRC a) (dft . amap (:+0) $ a)
prop_dftRC_dft22 tol a =
    partAbout tol (dftRCN [0,1] a) (dftN [0,1] . amap (:+0) $ a)


prop_dft3, prop_dft32, prop_dft32', prop_dft33, prop_dft33', prop_dft33'' ::
    (FFTWReal e, Abs (Complex e) e) =>
    e -> CArray (Int, Int, Int) (Complex e) -> Bool

prop_dft3     = aboutIdem $ idft . dft
prop_dft32    = aboutIdem $ idftN [0,1] . dftN [0,1]
prop_dft32'   = aboutIdem $ idftN [1,0] . dftN [1,0]
prop_dft33    = aboutIdem $ idftN [0,1,2] . dftN [0,1,2]
prop_dft33'   = aboutIdem $ idftN [0,2,1] . dftN [0,2,1]
prop_dft33''  = aboutIdem $ idftN [2,0,1] . dftN [2,0,1]

c_tests ::
    (FFTWReal e, Abs (Complex e) e) =>
    [(String, e -> CArray Int (Complex e) -> Bool)]
c_tests = [ ("dft idem 1D" , prop_dft)
          ]

c_tests2 ::
    (FFTWReal e, Abs (Complex e) e) =>
    [(String, e -> CArray (Int,Int) (Complex e) -> Bool)]
c_tests2 = [ ("dft idem 2D" , prop_dft2)
           , ("dft idem 2D/2" , prop_dft22)
           , ("dft idem 2D/2'" , prop_dft22')
          ]

c_tests3 ::
    (FFTWReal e, Abs (Complex e) e) =>
    [(String, e -> CArray (Int,Int,Int) (Complex e) -> Bool)]
c_tests3 = [ ("dft idem 3D" , prop_dft3)
           , ("dft idem 3D/2" , prop_dft32)
           , ("dft idem 3D/2'" , prop_dft32')
           , ("dft idem 3D/3" , prop_dft33)
           , ("dft idem 3D/3'" , prop_dft33')
           , ("dft idem 3D/3''" , prop_dft33'')
          ]

r_tests ::
    (FFTWReal e, Abs (Complex e) e, Abs e e) =>
    [(String, e -> CArray Int e -> Bool)]
r_tests = [ ("dftRC/CR idem  1D" , prop_dftRC)
          , ("dftRC dft 1D" , prop_dftRC_dft)
          , ("dht idem 1D" , prop_dht_idem)
          ]

r_tests2 ::
    (FFTWReal e, Abs (Complex e) e, Abs e e) =>
    [(String, e -> CArray (Int,Int) e -> Bool)]
r_tests2 = [ ("dftRC/CR idem  2D" , prop_dftRC)
           , ("dftRC dft 2D" , prop_dftRC_dft)
           , ("dftRC dft 2D/2" , prop_dftRC_dft22)
           , ("dht idem 2D" , prop_dht_idem)
          ]

testSingle ::
    (Show i, ArbitraryIx i, Show e, Storable e, Arbitrary e, QC.Testable t) =>
    QC.Args -> a -> (String, a -> CArray i e -> t) -> IO ()
testSingle conf tol (s, f) =
    printf "%-25s: " s >> QC.quickCheckWith conf (\(FFTArray x) -> f tol x)

tests ::
    (Show a, Arbitrary a, FFTWReal a, Abs a a, Abs (Complex a) a) =>
    QC.Args -> a -> IO ()
tests conf tol = do
    mapM_ (testSingle conf tol) c_tests
    mapM_ (testSingle conf tol) r_tests
    mapM_ (testSingle conf tol) c_tests2
    mapM_ (testSingle conf tol) r_tests2
    mapM_ (testSingle conf tol) c_tests3

main :: IO ()
main = do
    args <- getArgs
    let n = case args of [] -> 1000; nStr:_ -> read nStr
        conf =
            QC.stdArgs
              { QC.maxSuccess = n
              , QC.maxDiscardRatio = 1000
              , QC.maxSize = 3 + (n `div` 2)
              }
    putStrLn "Float" ; tests conf (1e-6::Float)
    putStrLn "Double"; tests conf (1e-15::Double)
