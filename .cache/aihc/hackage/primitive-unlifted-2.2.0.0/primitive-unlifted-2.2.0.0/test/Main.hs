{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeInType #-}

import Control.Monad.ST
import Data.Primitive
import Data.Primitive.Unlifted.Array
import Data.Primitive.Unlifted.Class
import Data.Proxy (Proxy(..))
import Data.Word
import GHC.Exts (fromList)
import GHC.Int
import Test.QuickCheck (Arbitrary,Arbitrary1,Gen,CoArbitrary,Function)
import Test.Tasty (defaultMain,testGroup,TestTree)

import qualified Data.List as L
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Classes.Base as QCC
import qualified Test.QuickCheck.Classes.Base.IsList as QCCL
import qualified Test.Tasty.QuickCheck as TQC

main :: IO ()
main = defaultMain $ testGroup "properties"
  [ testGroup "UnliftedArray"
    [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (UnliftedArray (PrimArray Int16))))
    , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (UnliftedArray (PrimArray Int16))))
    , lawsToTest (QCC.isListLaws (Proxy :: Proxy (UnliftedArray (PrimArray Int16))))
    , TQC.testProperty "mapUnliftedArray" (QCCL.mapProp arrInt16 arrInt32 mapUnliftedArray)
    , TQC.testProperty "foldrUnliftedArray" (QCCL.foldrProp arrInt16 foldrUnliftedArray)
    , TQC.testProperty "foldrUnliftedArray'" (QCCL.foldrProp arrInt16 foldrUnliftedArray')
    , TQC.testProperty "foldlUnliftedArray" (QCCL.foldlProp arrInt16 foldlUnliftedArray)
    , TQC.testProperty "foldlUnliftedArray'" (QCCL.foldlProp arrInt16 foldlUnliftedArray')
    ]
  ]

arrInt16 :: Proxy (PrimArray Int16)
arrInt16 = Proxy

arrInt32 :: Proxy (PrimArray Int16)
arrInt32 = Proxy

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

instance Arbitrary1 Array where
  liftArbitrary elemGen = fmap fromList (QC.liftArbitrary elemGen)

instance Arbitrary a => Arbitrary (Array a) where
  arbitrary = fmap fromList QC.arbitrary

instance Arbitrary1 SmallArray where
  liftArbitrary elemGen = fmap smallArrayFromList (QC.liftArbitrary elemGen)

instance Arbitrary a => Arbitrary (SmallArray a) where
  arbitrary = fmap smallArrayFromList QC.arbitrary

instance Arbitrary ByteArray where
  arbitrary = do
    xs <- QC.arbitrary :: Gen [Word8]
    return $ runST $ do
      a <- newByteArray (L.length xs)
      iforM_ xs $ \ix x -> do
        writeByteArray a ix x
      unsafeFreezeByteArray a

instance (Arbitrary a, Prim a) => Arbitrary (PrimArray a) where
  arbitrary = do
    xs <- QC.arbitrary :: Gen [a]
    return $ runST $ do
      a <- newPrimArray (L.length xs)
      iforM_ xs $ \ix x -> do
        writePrimArray a ix x
      unsafeFreezePrimArray a

instance (Arbitrary b, PrimUnlifted b, Unlifted b ~ a) => Arbitrary (UnliftedArray_ a b) where
  arbitrary = do
    xs <- QC.vector =<< QC.choose (0,3)
    return (unliftedArrayFromList xs)

instance (Prim a, CoArbitrary a) => CoArbitrary (PrimArray a) where
  coarbitrary x = QC.coarbitrary (primArrayToList x)

instance (Prim a, Function a) => Function (PrimArray a) where
  function = QC.functionMap primArrayToList primArrayFromList

iforM_ :: Monad m => [a] -> (Int -> a -> m b) -> m ()
iforM_ xs0 f = go 0 xs0 where
  go !_ [] = return ()
  go !ix (x : xs) = f ix x >> go (ix + 1) xs
