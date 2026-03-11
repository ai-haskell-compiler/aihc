{-# LANGUAGE ScopedTypeVariables #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8  as BS
import           Data.Int
import           Data.Ix
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog
-------------------------------------------------------------------------------
import           Data.RNG
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain tests


-------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "rng-utils"
  [ randomTokenTests
  , seedRNGTests
  , rngIOTests
  , rngRIOTests
  ]


-------------------------------------------------------------------------------
rngIOTests :: TestTree
rngIOTests = testGroup "rngIO"
  [ testProperty "multiple calls in sequence in a large space are very unlikely to produce the same value" $ property $ do
     rng <- genRNG
     i641 :: Int64 <- liftIO (rngIO rng)
     i642 :: Int64 <- liftIO (rngIO rng)
     assert (i641 /= i642)
  , testProperty "2 new rngs produce different values" $ property$ do
     rng1 <- liftIO mkRNG
     rng2 <- liftIO mkRNG
     i641 :: Int64 <- liftIO (rngIO rng1)
     i642 :: Int64 <- liftIO (rngIO rng2)
     assert (i641 /= i642)
  ]


-------------------------------------------------------------------------------
rngRIOTests :: TestTree
rngRIOTests = testGroup "rngRIO"
  [ testProperty "generates values in range for valid ranges" $ property $ do
      r1 <- forAll (Gen.int64 (Range.linearBounded))
      r2 <- forAll (Gen.int64 (Range.linear r1 maxBound))
      rng <- genRNG
      let valRange = (r1, r2)
      x <- liftIO (rngRIO rng valRange)
      assert (inRange valRange x)
  ]


-------------------------------------------------------------------------------
randomTokenTests :: TestTree
randomTokenTests = testGroup "randomToken"
  [ testProperty "0 length produces empty string" $ property $ do
      rng <- genRNG
      res <- liftIO (randomToken 0 rng)
      res === mempty
  , testProperty "< 0 produces empty string" $ property $ do
      rng <- genRNG
      sz <- forAll (Gen.int (Range.linear minBound (-1)))
      res <- liftIO (randomToken sz rng)
      res === mempty
  , testProperty "> 0 produces a string of the given length" $ property $ do
      rng <- genRNG
      sz <- genReasonableSize
      res <- liftIO (randomToken sz rng)
      BS.length res === sz
  , testProperty "produces lowercase hex string" $ property $ do
      rng <- genRNG
      sz <- genReasonableSize
      res <- liftIO (randomToken sz rng)
      let badChars = BS.filter (not . isHex) res
      badChars === mempty
  ]
  where
    isHex c = inRange ('0', '9') c || inRange ('a', 'f') c


-------------------------------------------------------------------------------
genRNG :: PropertyT IO RNG
genRNG = liftIO =<< Gen.sample genRNG'


-------------------------------------------------------------------------------
seedRNGTests :: TestTree
seedRNGTests = testGroup "seedRNG"
  [ testProperty "fixed seed produces the same values" $ property $ do
      seed <- forAll (Gen.int Range.linearBounded)
      sz <- genReasonableSize
      rng1 <- liftIO (seedRNG seed)
      res1 <- liftIO (randomToken sz rng1)
      rng2 <- liftIO (seedRNG seed)
      res2 <- liftIO (randomToken sz rng2)
      res1 === res2
  ]

-------------------------------------------------------------------------------
genRNG' :: Gen (IO RNG)
genRNG' = seedRNG <$> Gen.int Range.linearBounded


-------------------------------------------------------------------------------
genReasonableSize :: PropertyT IO Int
genReasonableSize = forAll (Gen.int (Range.linear 1 512))
