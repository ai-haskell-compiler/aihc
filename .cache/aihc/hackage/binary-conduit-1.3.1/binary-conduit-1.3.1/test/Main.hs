{-# LANGUAGE DeriveGeneric #-}
import Control.Applicative
import Control.Monad (forM_, when)
import Control.Monad.IO.Class
import Data.Binary
import Data.Binary.Put
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Serialization.Binary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property
import Test.QuickCheck.Monadic
import Test.QuickCheck
import Control.Monad.Trans.Resource
import GHC.Generics
import Prelude

-- | check conduitEncode =$= conduitDecode == id
prop_eq :: (Binary a,Eq a) => [a] -> Property
prop_eq xs = monadicIO $ do
    xs' <- liftIO $ runConduit $ CL.sourceList xs
                     .| enc xs
                     .| dec xs
                     .| CL.consume
    assert (xs == xs')
  where enc :: (Binary a, MonadThrow m) => [a] -> ConduitT a ByteString m ()
        enc _ = conduitEncode
        dec :: (Binary a, MonadThrow m) => [a] -> ConduitT ByteString a m ()
        dec _ = conduitDecode

prop_sink :: (Binary a,Eq a) => (a,a) -> Property
prop_sink (a,b) = monadicIO $ do
    (a',b') <- liftIO $ runConduit $ CL.sourceList [a,b]
                          .| enc a
                          .| do a' <- sinkGet get
                                b' <- CL.consume
                                return (a',b')
    assert $ a == a'
    assert $ runPut (put b) == LBS.fromChunks b'
  where enc :: (Binary a, MonadThrow m) => a -> ConduitT a ByteString m ()
        enc _ = conduitEncode

prop_part2 :: [Int] -> Property
prop_part2 xs = monadicIO $ do
    let m = BS.concat . Prelude.concatMap (LBS.toChunks . runPut . put) $ xs
    when (Prelude.length xs>0) $ do
        forM_ [0..BS.length m] $ \l -> do
            let (l1,l2) = BS.splitAt l m
            a <- liftIO $ runConduit $ CL.sourceList [l1,l2]
                            .| conduitDecode
                            .| CL.consume
            stop (xs ?== a)

prop_part3 :: [Int] -> Property
prop_part3 xs = monadicIO $ do
    let m = BS.concat . Prelude.concatMap (LBS.toChunks . runPut . put) $ xs
    when (Prelude.length xs>0) $ do
      forM_ [1..BS.length m] $ \l -> do
          let (l1,l2) = BS.splitAt l m
          when (BS.length l2 > 0) $ do
            forM_ [1..BS.length l2] $ \l' -> do
                let (l2_1,l2_2) = BS.splitAt l' l2
                a <- liftIO $ runConduit $ CL.sourceList [l1,l2_1,l2_2]
                                .| conduitDecode
                                .| CL.consume
                stop $ xs ?== a


data A = A ByteString ByteString deriving (Eq, Show, Generic)

instance Binary A
instance Arbitrary A where
  arbitrary = A <$> fmap BS.pack arbitrary
                <*> fmap BS.pack arbitrary

prop_eq_plus :: (Binary a, Eq a) => [a] -> Property
prop_eq_plus xs = monadicIO $ do
   x <- runConduit $ CL.sourceList xs .| CL.map encode .| CL.map LBS.toStrict .| CL.consume :: PropertyM IO [BS.ByteString]
   y <- runConduit $ CL.sourceList xs .| conduitMsgEncode .| CL.consume :: PropertyM IO [BS.ByteString]
   stop $ x ?== y :: PropertyM IO ()

main :: IO ()
main = hspec $ do
    describe "QC properties: conduitEncode =$= conduitDecode == id" $ do
        prop "int"               $ (prop_eq :: [Int] -> Property)
        prop "string"            $ (prop_eq :: [String] -> Property)
        prop "maybe int"         $ (prop_eq :: [Maybe Int] -> Property)
        prop "either int string" $ (prop_eq :: [Either Int String] -> Property)
        prop "(Int,Int)"         $ (prop_sink :: (Int,Int) -> Property)
        prop "(String,String)"   $ (prop_sink :: (String,String) -> Property)
        prop "A"                 $ (prop_eq   :: [A] -> Property)
    describe "QC properties partial lists" $ do
        prop "break data in 2 parts" $ (prop_part2)
        prop "break data in 3 parts" $ (prop_part3)
    describe "QC properites: CL.conduitMsgEncode returns a correct chunks" $ do
        prop "int"               $ (prop_eq_plus :: [Int] -> Property)
        prop "string"            $ (prop_eq_plus :: [String] -> Property)
        prop "maybe int"         $ (prop_eq_plus :: [Maybe Int] -> Property)
        prop "either int string" $ (prop_eq_plus :: [Either Int String] -> Property)
        prop "A"                 $ (prop_eq_plus :: [A] -> Property)
    describe "HUnit properties:" $ do
      it "decodes message splitted to chunks" $ do
          let i = -32
              l = runPut (put (i::Int))
              (l1,l2) = LBS.splitAt (LBS.length l `div` 2) l
              t = BS.concat . LBS.toChunks
          x <- runConduit $ CL.sourceList [t l1,t l2] .| conduitDecode .| CL.consume
          x `shouldBe` [i]
      it "decodes message with list of values inside" $ do
          let is = [-32,45::Int]
              ls = BS.concat . Prelude.concatMap (LBS.toChunks .runPut . put) $ is
              (ls1,ls2) = BS.splitAt ((BS.length ls `div` 2) +1) ls
          x <- runConduit $ CL.sourceList [ls,ls] .| conduitDecode .| CL.consume
          x' <- runConduit $ CL.sourceList [ls1,ls2] .| conduitDecode .| CL.consume
          x `shouldBe` is++is
          x' `shouldBe` is

