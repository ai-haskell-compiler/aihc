module HaskellWorks.Data.ByteStringSpec where

import Control.Monad
import Data.Foldable
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Internal as LBSI
import qualified HaskellWorks.Data.ByteString  as BS
import qualified Hedgehog.Gen                  as G
import qualified Hedgehog.Range                as R

{- HLINT ignore "Redundant do" -}

spec :: Spec
spec = describe "HaskellWorks.Data.ByteStringSpec" $ do
  it "resegment does not modify data" $ requireProperty $ do
    bss       <- forAll $ (BS.pack <$>) <$> G.list (R.linear 0 8) (G.list (R.linear 0 24) (G.word8 R.constantBounded))
    chunkSize <- forAll $ G.int (R.linear 1 4)
    mconcat (BS.resegment chunkSize bss) === mconcat bss
  it "resegment creates correctly sized segments" $ requireProperty $ do
    bss       <- forAll $ (BS.pack <$>) <$> G.list (R.linear 0 8) (G.list (R.linear 0 24) (G.word8 R.constantBounded))
    chunkSize <- forAll $ G.int (R.linear 1 4)
    let withoutLast = reverse (drop 1 (reverse (BS.resegment chunkSize bss)))
    let disalignments = (\n -> n - (n `div` chunkSize) * chunkSize) . BS.length <$> withoutLast
    disalignments === replicate (length disalignments) 0
  it "resegmentPadded does not modify data" $ requireProperty $ do
    bss       <- forAll $ (BS.pack <$>) <$> G.list (R.linear 0 8) (G.list (R.linear 0 24) (G.word8 R.constantBounded))
    chunkSize <- forAll $ G.int (R.linear 1 4)
    elbs      <- forAll $ pure $ LBS.fromChunks bss
    albs      <- forAll $ pure $ LBS.take (LBS.length elbs) (LBS.fromChunks (BS.resegmentPadded chunkSize bss))
    albs === elbs
  it "resegmentPadded creates correctly sized segments" $ requireProperty $ do
    bss       <- forAll $ (BS.pack <$>) <$> G.list (R.linear 0 8) (G.list (R.linear 0 24) (G.word8 R.constantBounded))
    chunkSize <- forAll $ G.int (R.linear 1 4)
    forM_ (reverse (BS.resegmentPadded chunkSize bss)) $ \bs -> do
      BS.length bs `mod` chunkSize === 0
  it "rechunk does not modify data" $ requireProperty $ do
    bss       <- forAll $ (BS.pack <$>) <$> G.list (R.linear 0 8) (G.list (R.linear 0 24) (G.word8 R.constantBounded))
    chunkSize <- forAll $ G.int (R.linear 1 4)
    mconcat (BS.rechunk chunkSize bss) === mconcat bss
  it "rechunk creates correctly sized segments" $ requireProperty $ do
    bss       <- forAll $ (BS.pack <$>) <$> G.list (R.linear 0 8) (G.list (R.linear 0 24) (G.word8 R.constantBounded))
    chunkSize <- forAll $ G.int (R.linear 1 4)
    forM_ (drop 1 (reverse (BS.rechunk chunkSize bss))) $ \bs -> do
      BS.length bs `mod` chunkSize === 0
  it "rechunk creates correctly sized segments" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 (LBSI.defaultChunkSize `div` 4)) (G.word64 R.constantBounded)
    fold (BS.toByteStrings ws) === BS.toByteString ws
