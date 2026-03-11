module Main ( main ) where

import OpenSSL.Digest
import OpenSesame

import Data.ByteString.Char8 ( unpack )
import System.Exit
import Test.HUnit

main :: IO ()
main = do cnt <- runTestTT (TestList tests)
          if errors cnt == 0 && failures cnt == 0
             then exitSuccess
             else exitFailure

tests :: [Test]
tests = map (uncurry (mkTest "open sesame")) opensesame

mkTest :: String -> String -> String -> Test
mkTest input algoName expect = TestCase $
  case digestByName' algoName of
    Nothing -> assertFailure ("cannot find digest algorithm " ++ show algoName)
    Just algo -> assertEqual algoName expect (unpack (toHex (digestString algo input)))
