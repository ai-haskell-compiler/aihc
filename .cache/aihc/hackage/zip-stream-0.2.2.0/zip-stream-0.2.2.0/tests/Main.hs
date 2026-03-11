{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad (when, void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit as C
import           Data.Conduit.Combinators (sinkNull)
import           Data.Foldable (for_)
import qualified Data.Text as T
import           Data.Time.LocalTime (utc, utcToLocalTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           GHC.Stats (getRTSStats, RTSStats(..), GCDetails(..))
import           System.Mem (performMajorGC)
import           Test.Hspec (hspec, describe, it)

import           Codec.Archive.Zip.Conduit.Zip


main :: IO ()
main = hspec $ do
  describe "zipping" $ do
    it "ZipDataByteString streams in constant memory" $ do
      C.runConduitRes $
        (do
          -- Stream 1000 * 4 MiB = 4 GiB
          for_ [(1::Int)..1024] $ \i -> do
            -- `bs` needs to depend on loop variable `i`, otherwise GHC may hoist
            -- it out of the loop ("floating"), making the memory constant
            -- even for incorrect implementations, thus making the test useless.
            let !bs = BS.replicate (4 * 1024 * 1024) (fromIntegral i) -- 4 MiB

            C.yield
              ( ZipEntry
                  { zipEntryName = Left ("file-" <> T.pack (show i) <> ".bin")
                  , zipEntryTime = utcToLocalTime utc (posixSecondsToUTCTime 0)
                  , zipEntrySize = Nothing
                  , zipEntryExternalAttributes = Nothing
                  }
              , ZipDataByteString (BSL.fromStrict bs) -- `copy` to avoid sharing
              )

            liftIO $ do
              -- GC every 40 MB to make it easy to observe constant memory.
              when (i `mod` 10 == 0) performMajorGC

              RTSStats{ gc = GCDetails{ gcdetails_live_bytes } } <- getRTSStats
              when (gcdetails_live_bytes > 3 * 1024 * 1024 * 1024) $ do -- 3 GiB
                error $ "Memory usage too high (" ++ show gcdetails_live_bytes ++ " B), probably streaming is not constant-memory"

        )
        C..| void (zipStream defaultZipOptions{ zipOptCompressLevel = 0 })
        C..| sinkNull
        :: IO ()

