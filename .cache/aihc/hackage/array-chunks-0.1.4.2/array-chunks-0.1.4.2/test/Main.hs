{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Chunks (Chunks (ChunksCons, ChunksNil))
import qualified Data.Chunks as C
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Primitive (SmallArray)
import Data.Proxy (Proxy (Proxy))
import GHC.Exts (fromList)
import qualified GHC.Exts as Exts
import Test.QuickCheck (Arbitrary, Gen, (===))
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Classes (eqLaws, foldableLaws, isListLaws, monoidLaws, semigroupLaws)
import qualified Test.QuickCheck.Classes as QCC
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import qualified Test.Tasty.QuickCheck as TQC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Chunks"
    [ lawsToTest (eqLaws (Proxy :: Proxy (Chunks Integer)))
    , lawsToTest (semigroupLaws (Proxy :: Proxy (Chunks Integer)))
    , lawsToTest (monoidLaws (Proxy :: Proxy (Chunks Integer)))
    , lawsToTest (isListLaws (Proxy :: Proxy (Chunks Integer)))
    , lawsToTest (foldableLaws (Proxy :: Proxy Chunks))
    , TQC.testProperty "concat" $ \(cs :: Chunks Integer) ->
        F.toList cs === Exts.toList (C.concat cs)
    , TQC.testProperty "concatReverse" $ \(cs :: Chunks Integer) ->
        Exts.toList (mconcat (L.reverse (Exts.toList cs)))
          === Exts.toList (C.concatReverse cs)
    , testCase "eq-A" $
        fromList [fromList [5 :: Integer], fromList [6 :: Integer]]
          @=? (fromList [fromList [5 :: Integer, 6]] :: Chunks Integer)
    , testCase "eq-B" $
        fromList [fromList [], fromList [5 :: Integer], fromList [6 :: Integer]]
          @=? (fromList [fromList [5 :: Integer, 6]] :: Chunks Integer)
    , testCase "eq-C" $
        fromList [fromList [], fromList [5 :: Integer], fromList [6 :: Integer]]
          @=? (fromList [fromList [5 :: Integer, 6], fromList []] :: Chunks Integer)
    ]

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

instance (Arbitrary a) => Arbitrary (Chunks a) where
  arbitrary =
    QC.choose (0, 3 :: Int) >>= \case
      0 -> pure ChunksNil
      1 -> do
        a <- arbitrarySmallArray
        pure (ChunksCons a ChunksNil)
      2 -> do
        a <- arbitrarySmallArray
        b <- arbitrarySmallArray
        pure (ChunksCons a (ChunksCons b ChunksNil))
      3 -> do
        a <- arbitrarySmallArray
        b <- arbitrarySmallArray
        c <- arbitrarySmallArray
        pure (ChunksCons a (ChunksCons b (ChunksCons c ChunksNil)))
      _ -> error "Chunks.arbitrary: not possible"

instance (Arbitrary a) => Arbitrary (SmallArray a) where
  arbitrary = arbitrarySmallArray

arbitrarySmallArray :: (Arbitrary a) => Gen (SmallArray a)
arbitrarySmallArray = do
  n <- QC.choose (0, 2 :: Int)
  fmap Exts.fromList (QC.vectorOf n QC.arbitrary)
