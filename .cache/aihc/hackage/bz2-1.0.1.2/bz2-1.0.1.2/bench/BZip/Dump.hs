module BZip.Dump ( compressDump
                 , decompressDump
                 , decompressForce
                 ) where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath      ((</>))
import           System.IO.Temp       (withSystemTempDirectory)

decompressForce :: (BSL.ByteString -> BSL.ByteString) -> IO ()
decompressForce go =
    forceBSL . go =<< BSL.readFile "valgrind-3.15.0.tar.bz2"

    where forceBSL = (`seq` pure ()) . last . BSL.toChunks

{-# INLINE decompressDump #-}
decompressDump :: (BSL.ByteString -> BSL.ByteString) -> IO ()
decompressDump go = withSystemTempDirectory "bz2" $
    \fp -> BSL.writeFile (fp </> "valgrind-3.15.0.tar")
        . go =<< BSL.readFile "valgrind-3.15.0.tar.bz2"

{-# INLINE compressDump #-}
compressDump :: (BSL.ByteString -> BSL.ByteString) -> IO ()
compressDump go = withSystemTempDirectory "bz2" $
    \fp -> BSL.writeFile (fp </> "valgrind-3.15.0.tar.bz2")
        . go =<< BSL.readFile "valgrind-3.15.0.tar"
