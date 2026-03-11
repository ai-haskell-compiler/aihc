{-# LANGUAGE FlexibleContexts #-}

import Criterion.Main

import Data.Vector.Generic as VG
import Data.Vector.Storable as VS
import Data.Vector.Unboxed as VU

import Data.Vec3


oXYZ :: (Double, Double, Double)
oXYZ = (19.899999999, 22.8, -100500)


oXYZ' :: (Double, Double, Double)
oXYZ' = (2, 12, 85.06)


n :: Int
n = 10000000


-- Access to whole elements
testWhole :: (Vec3 a, VG.Vector v a) => v a -> v a -> Benchmarkable
testWhole v v2 = whnf (uncurry $ VG.zipWith (<+>)) (v, v2)


-- Access to x components of elements
testComp :: (Vec3 a, VG.Vector v a, VG.Vector v Double) => v a -> v a -> Benchmarkable
testComp v v' = whnf (uncurry $ VG.zipWith foo) (v, v')
  where
    foo v1 v2 = x1 + x2
      where
        (x1, _, _) = toXYZ v1
        (x2, _, _) = toXYZ v2


-- zipWith using generic dot product
testDotM :: (Vec3 a, VG.Vector v a, VG.Vector v Double) => v a -> v a -> Benchmarkable
testDotM v v2 = whnf (uncurry $ VG.zipWith dotM') (v, v2)
  where
    o = fromXYZ oXYZ
    m = fromRows (o, o, o)
    dotM' e1 e2 = dotM e1 e2 m


testNormalize :: (Vec3 a, VG.Vector v a) => v a -> Benchmarkable
testNormalize = whnf (VG.map normalize)


-- Note that source arrays are not forced in test functions.

csv :: VS.Vector CVec3
csv = VG.replicate n $ fromXYZ oXYZ


cuv :: VU.Vector CVec3
cuv = VG.replicate n $ fromXYZ oXYZ


tv :: VU.Vector TVec3
tv = VG.replicate n $ fromXYZ oXYZ


csv' :: VS.Vector CVec3
csv' = VG.replicate n $ fromXYZ oXYZ'


cuv' :: VU.Vector CVec3
cuv' = VG.replicate n $ fromXYZ oXYZ'


tv' :: VU.Vector TVec3
tv' = VG.replicate n $ fromXYZ oXYZ'


main :: IO ()
main = defaultMain
       [ bgroup "CVec3/Storable" $ mkGroup csv csv'
       , bgroup "CVec3/Unboxed"  $ mkGroup cuv cuv'
       , bgroup "TVec3/Unboxed"  $ mkGroup tv tv'
       ]
  where
    mkGroup v1 v2 =
      [ bench "zipWith-add"      $ testWhole v1 v2
      , bench "zipWith-by-x-add" $ testComp v1 v2
      , bench "zipWith-dotM"     $ testDotM v1 v2
      , bench "normalize"        $ testNormalize v1
      ]
