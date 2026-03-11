{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

--------------------------------------------------------------------------------
module Codec.Compression.Heatshrink.Tests (tests) where


--------------------------------------------------------------------------------
import           Codec.Compression.Heatshrink
import qualified Data.ByteString.Lazy as BL
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.Golden
import           Text.Regex.PCRE.Heavy


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Codec.Compression.Heatshrink"
        [ testGroup "decoding files" $ unsafePerformIO goldenTests
        ]


--------------------------------------------------------------------------------
decodeFile :: DecodeParams-> FilePath -> FilePath -> IO ()
decodeFile p s t  = do
  c <- BL.readFile s
  u <- decodeWith p c
  BL.writeFile t u


--------------------------------------------------------------------------------
goldenTests :: IO [TestTree]
goldenTests = do
  files <- findByExtension [".golden"] "files"
  let test = map (scan [re|((.*)/(\w+)_w(\d+)l(\d+))\.golden|]) files
  return $ map createTest test


--------------------------------------------------------------------------------
createTest :: [(String, [String])] -> TestTree
createTest [(f, [noext, dir, base, sw, sl])] = goldenVsFile f f dstFile (decodeFile params srcFile dstFile)
    where
      params = DecodeParams 1024 w l
      srcFile = noext ++ ".hex"
      dstFile = noext ++ ".tmp"
      w = read sw :: Int
      l = read sl :: Int
