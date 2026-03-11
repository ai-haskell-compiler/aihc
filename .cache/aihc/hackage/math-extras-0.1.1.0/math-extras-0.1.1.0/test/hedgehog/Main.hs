module Main (main) where

import           Control.Monad (unless)
import           GHC.IO.Encoding (utf16le)
import           System.Exit (exitFailure)
import           System.IO (hSetEncoding, stdout, stderr)

import qualified DoubleSpec
import qualified IntSpec

main :: IO ()
main = do
  hSetEncoding stdout utf16le
  hSetEncoding stderr utf16le
  passed <- sequenceA [IntSpec.tests, DoubleSpec.tests]
  unless (and passed) exitFailure
