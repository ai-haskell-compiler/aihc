{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Vector.StorableSpec
  ( spec
  ) where

import Control.Monad.ST            (ST)
import Data.Vector.Storable        (Storable)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                     hiding (abs)
import Test.Hspec

import qualified Data.ByteString                   as BS
import qualified Data.List                         as L
import qualified Data.Vector.Storable              as DVS
import qualified Data.Vector.Storable.Mutable      as DVSM
import qualified HaskellWorks.Data.ByteString      as BS
import qualified HaskellWorks.Data.Vector.Storable as DVS
import qualified Hedgehog.Gen                      as G
import qualified Hedgehog.Range                    as R

{- HLINT ignore "Redundant do" -}

spec :: Spec
spec = describe "HaskellWorks.Data.Vector.StorableSpec" $ do
  it "mapAccumL: f a b = (a + 1, b * 2)" $ requireProperty $ do
    as <- forAll $ G.list (R.linear 0 10) (G.word64 (R.linear 0 255))
    let f a b = (a + 1, b * 2)
    (DVS.toList <$> DVS.mapAccumL f (0 :: Int) (DVS.fromList as)) === L.mapAccumL f (0 :: Int) as
  it "mapAccumL: f a b = (a * 2, b + 1)" $ requireProperty $ do
    as <- forAll $ G.list (R.linear 0 10) (G.word64 (R.linear 0 255))
    let f a b = (a * 2, b + 1)
    (DVS.toList <$> DVS.mapAccumL f (0 :: Int) (DVS.fromList as)) === L.mapAccumL f (0 :: Int) as
  it "construct2N" $ requireProperty $ do
    as <- forAll $ G.list (R.linear 0 10) (G.word64 (R.linear 0 255))
    let (bs, cs) = DVS.construct2N (length as) stepb (length as * 2) stepc as
    DVS.fromList as           === bs
    DVS.fromList (dupList as) === cs
  describe "construct64UnzipN" $ do
    it "property" $ requireProperty $ do
      abss  <- forAll $ G.list (R.linear 1 8) $ (,)
        <$> fmap BS.pack (G.list (R.linear 1 8) (G.word8 R.constantBounded))
        <*> fmap BS.pack (G.list (R.linear 1 8) (G.word8 R.constantBounded))
      ass   <- forAll $ pure $ fmap fst abss
      bss   <- forAll $ pure $ fmap snd abss
      as    <- forAll $ pure $ mconcat ass
      bs    <- forAll $ pure $ mconcat bss
      al    <- forAll $ pure $ BS.length (mconcat ass)
      bl    <- forAll $ pure $ BS.length (mconcat bss)
      len   <- forAll $ pure $ al + bl
      let res = DVS.construct64UnzipN len (zip ass bss)
      let ra = BS.toByteString (fst res)
      let rb = BS.toByteString (snd res)
      ra === BS.padded ((BS.length as + 7) `div` 8 * 8) as
      rb === BS.padded ((BS.length bs + 7) `div` 8 * 8) bs
  it "unzipFromListN2" $ requireProperty $ do
    abs       <- forAll $ G.list (R.linear 0 8) $ (,)
      <$> G.word8 R.constantBounded
      <*> G.word8 R.constantBounded
    len       <- forAll $ G.int (R.linear 0 8)
    as        <- forAll $ pure $ fmap fst abs
    bs        <- forAll $ pure $ fmap snd abs
    (va, vb)  <- forAll $ pure $ DVS.unzipFromListN2 len abs
    let eva = DVS.fromList (take len as)
    let evb = DVS.fromList (take len bs)
    va === eva
    vb === evb

dupList :: [a] -> [a]
dupList (a:as) = a:a:dupList as
dupList []     = []

stepb :: Storable a => a -> DVSM.MVector s a -> ST s Int
stepb a v = DVSM.write v 0 a >> return 1

stepc :: Storable a => a -> DVSM.MVector s a -> ST s Int
stepc a v = DVSM.write v 0 a >> DVSM.write v 1 a >> return 2
