{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  ( main
  ) where

import           Control.Monad ( replicateM )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.PEM ( PEM (..), pemParseBS, pemWriteBS )
import           Test.Framework ( Test, defaultMain, testGroup )
import           Test.Framework.Providers.QuickCheck2 ( testProperty )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit ( (@=?) )
import           Test.QuickCheck ( Arbitrary (..), Gen, choose, elements )

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "units" testUnits
  , testDecodingMultiple
  , testUnmatchingNames
  , testProperty "marshall" testMarshall
  ]

testUnits :: [Test]
testUnits = zipWith
  (curry (\(i, (p, bs)) -> testCase (show i) (pemWriteBS p @=? BC.pack bs)))
  [ 0 :: Int .. ]
  [ (p1, bp1), (p2, bp2) ]
 where
  p1  = PEM { pemName = "abc", pemHeader = [], pemContent = B.replicate 64 0 }
  bp1 = unlines
    [ "-----BEGIN abc-----"
    , "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    , "AAAAAAAAAAAAAAAAAAAAAA=="
    , "-----END abc-----"
    ]
  p2 = PEM { pemName = "xxx", pemHeader = [], pemContent = B.replicate 12 3 }
  bp2 = unlines
    [ "-----BEGIN xxx-----"
    , "AwMDAwMDAwMDAwMD"
    , "-----END xxx-----"
    ]

testDecodingMultiple :: Test
testDecodingMultiple =
  testCase "multiple pems" (pemParseBS content @=? Right expected)
 where
  expected =
    [ PEM { pemName = "marker", pemHeader = [], pemContent = B.replicate 12 3 }
    , PEM { pemName = "marker2", pemHeader = [], pemContent = B.replicate 64 0 }
    ]
  content = BC.pack $ unlines
    [ "some text that is not related to PEM"
    , "and is just going to be ignored by the PEM parser."
    , ""
    , "even empty lines should be skip until the rightful marker"
    , "-----BEGIN marker-----"
    , "AwMDAwMDAwMDAwMD"
    , "-----END marker-----"
    , "some middle text"
    , "-----BEGIN marker2-----"
    , "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    , "AAAAAAAAAAAAAAAAAAAAAA=="
    , "-----END marker2-----"
    , "and finally some trailing text."
    ]

testUnmatchingNames :: Test
testUnmatchingNames =
  testCase "unmatching name"
    ( let r = pemParseBS content
      in  case r of
            Left _ -> True @=? True
            _      -> r @=? Left ""
    )
 where
  content = BC.pack $ unlines
    [ "-----BEGIN marker-----"
    , "AAAA"
    , "-----END marker2-----"
    ]

testMarshall :: [PEM] -> Bool
testMarshall pems = readPems == Right pems
 where
  readPems = pemParseBS writtenPems
  writtenPems = B.concat (map pemWriteBS pems)

arbitraryName :: Gen [Char]
arbitraryName = choose (1, 30) >>= \i -> replicateM i arbitraryAscii
 where
  arbitraryAscii = elements ['A'..'Z']

arbitraryContent :: Gen B.ByteString
arbitraryContent = choose (1,100) >>= \i ->
  (B.pack . map fromIntegral) `fmap` replicateM i (choose (0,255) :: Gen Int)

instance Arbitrary PEM where
  arbitrary = PEM <$> arbitraryName <*> pure [] <*> arbitraryContent
