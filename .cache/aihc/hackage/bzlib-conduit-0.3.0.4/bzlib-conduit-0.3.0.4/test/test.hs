{-# LANGUAGE ViewPatterns #-}
import Control.Applicative
import Control.Monad (replicateM, forM_)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Conduit
import Data.Conduit.BZlib
import System.Random

import Test.Hspec
import Test.Hspec.QuickCheck

import Prelude as P

main :: IO ()
main = hspec $ do
  describe "decompress" $ do
    forM_ [1..5] $ \n -> do
      let file = "sample"++show n
      it ("correctly " ++ file ++ ".bz2") $ do
        dec <- runConduitRes
             $ sourceFile ("test/" ++ file ++ ".bz2")
            .| bunzip2
            .| takeCE 1000000000
            .| sinkLazy
        ref <- L.readFile ("test/" ++ file ++ ".ref")
        dec `shouldBe` ref

  describe "compress" $ do
    prop ". decompress == id" $ \((`mod` (2^(16 :: Int))) . abs -> n) -> do
        let bsize = 8192
        ss <- P.takeWhile (not. null)
              . P.map (P.take bsize)
              . P.iterate (P.drop bsize)
              <$> replicateM (abs n) randomIO
        dest <- runConduitRes
              $ yieldMany (P.map S.pack ss)
             .| bzip2
             .| bunzip2
             .| takeCE 1000000000
             .| sinkLazy
        dest `shouldBe` L.pack (P.concat ss)
