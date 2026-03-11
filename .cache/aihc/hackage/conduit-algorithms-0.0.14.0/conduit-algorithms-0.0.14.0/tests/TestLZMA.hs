{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module TestLZMA
    ( tgroup_lzma
    ) where
import Test.Tasty.TH
import Test.Tasty.HUnit (assertFailure, (@?=))
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, pick, pre, assert)
import Control.Monad.Trans.Resource

import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Conduit ((.|))
import Data.Word
import Data.Conduit.Lzma2
import System.IO.Error (tryIOError)

tgroup_lzma = $(testGroupGenerator)

assertLeft (Left _) = return ()
assertLeft _ = run $ assertFailure "Expected Left, got Right"

someString :: Gen B.ByteString
someString = do
  val <- listOf $ elements [0..255::Word8]
  return $ B.pack val

runC :: MonadUnliftIO m => C.ConduitT () C.Void (ResourceT m) a -> m a
runC = runResourceT . C.runConduit

newtype BigString = BigString B.ByteString
    deriving (Eq, Show)


instance Arbitrary BigString where
    arbitrary = BigString <$> resize (1024*16) someString

prop_compressAndDiscard :: BigString -> Property
prop_compressAndDiscard (BigString str) = monadicIO $ do
    run . runC $
        CL.sourceList [str]
            .| compress Nothing
            .| CL.sinkNull

prop_compressAndCheckLength :: BigString -> Property
prop_compressAndCheckLength (BigString str) = monadicIO $ do
  len <- run . runC $
            CL.sourceList [str]
            .| compress Nothing
            .| CL.fold (\ acc el -> acc + B.length el) 0
  -- random strings don't compress very well
  assert $ len > B.length str `div` 2
  assert $ len - 64 < B.length str * 2

prop_chain :: BigString -> Property
prop_chain (BigString str) = monadicIO . run $ do
    str' <- runC $
        CL.sourceList [str]
            .| compress Nothing
            .| decompress Nothing
            .| CL.consume
    str @?= B.concat str'

prop_compressThenDecompress :: BigString -> Property
prop_compressThenDecompress (BigString str) = monadicIO $ do
    blob <- run . runC $ CL.sourceList [str] .| compress Nothing .| CL.consume
    let blob' = B.concat blob
    randIdx <- pick $ elements [0..B.length blob'-1]
    let resplit = let (x,y) = B.splitAt randIdx blob' in [x,y]
    str' <- run . runC $ CL.sourceList resplit .| decompress Nothing .| CL.consume
    run $ str @?= B.concat str'


prop_decompressRandom :: BigString -> Property
prop_decompressRandom (BigString str) = monadicIO $ do
  -- The BigString is not necessarily big. It can even be the empty string
  -- https://github.com/alphaHeavy/lzma-conduit/issues/19
  pre $ B.length str > 64
  header <- run . runC $
            CL.sourceList []
                .| compress Nothing
                .| CL.consume
  let blob = header ++ [str]
  ioErrorE <- run $
    tryIOError (runC $ CL.sourceList blob .| decompress Nothing .| CL.sinkNull)
  assertLeft ioErrorE

prop_decompressCorrupt :: BigString -> Property
prop_decompressCorrupt (BigString str) = monadicIO $ do
  header <- run . runC $ (CL.sourceList [] .| compress Nothing .| CL.consume)
  let header' = B.concat header
  randVal <- pick $ elements [0..255::Word8]
  randIdx <- pick $ elements [0..B.length header'-1]
  let (left, right) = B.splitAt randIdx header'
      updated = left `B.append` (randVal `B.cons` B.tail right)
      blob = [updated, str]
  ioErrorE <- run $
    tryIOError (runC $ CL.sourceList blob .| decompress Nothing .| CL.sinkNull)
  assertLeft ioErrorE


prop_decompressEmpty :: Property
prop_decompressEmpty = monadicIO $ do
  count <- pick $ elements [0..10]
  let blob = replicate count B.empty
  ioErrorE <- run . tryIOError . runC $ CL.sourceList blob .| decompress Nothing .| CL.sinkNull
  assertLeft ioErrorE
