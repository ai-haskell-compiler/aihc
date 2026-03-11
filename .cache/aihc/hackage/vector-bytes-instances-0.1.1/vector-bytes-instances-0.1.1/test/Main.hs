{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Bytes.Get as Bytes
import qualified Data.Bytes.Put as Bytes
import qualified Data.Bytes.Serial as Bytes
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Serial ()
import qualified Data.Vector.Storable as VS
import Test.Tasty
import Test.Tasty.QuickCheck

roundTrip
  :: forall v a
  .  (Eq (v a), Bytes.Serial (v a), VG.Vector v a)
  => v a
  -> Property
roundTrip v =
    let ev' = Bytes.runGetS Bytes.deserialize (Bytes.runPutS (Bytes.serialize v))
    in property (ev' == Right v)

main = defaultMain $ testGroup "Vector Serial instances"
    [ testProperty "Unboxed"  $ roundTrip $ VU.enumFromTo z 100
    , testProperty "Storable" $ roundTrip $ VS.enumFromTo z 100
    , testProperty "Boxed"    $ roundTrip $ V.enumFromTo  z 100
    ]
  where
    z = 0 :: Int
