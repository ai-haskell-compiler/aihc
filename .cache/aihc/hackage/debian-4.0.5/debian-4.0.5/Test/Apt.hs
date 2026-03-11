{-# LANGUAGE OverloadedStrings #-}

module Apt (aptTests) where

import Test.HUnit
import Control.Exception
import Data.Text (Text)
import Debian.Apt.Index
import Debian.Control

aptTests :: [Test]
aptTests = releaseIndexTests

releaseIndexTests :: [Test]
releaseIndexTests =
    [ testReleaseIndexes e ps i | (i, (e, ps)) <- zip [1..]
        [ ( Left "Invalid release file: "
          , [] )
        , ( Left "No indexes in release: No SHA256 Field, No SHA1 Field, No MD5Sum Field"
          , [Paragraph []] )
        , ( Left "No indexes in release: No SHA256 Field, No SHA1 Field, No MD5Sum Field"
          , [Paragraph [Field ("x", "foo")]] )
        , ( Left "No indexes in release: Invalid checksum line: \"abc\", Invalid checksum line: \"def\", Invalid checksum line: \"ghi\""
          , [Paragraph [Field ("SHA256", "abc"), Field ("SHA1", "def"), Field ("MD5Sum", "ghi")]] )
        , ( Left "No indexes in release: Invalid size field: \"123x\", No SHA1 Field, No MD5Sum Field"
          , [Paragraph [Field ("SHA256", "abcde 123x file")]] )
        , ( Right [(CheckSums {md5sum = Nothing, sha1 = Nothing, sha256 = Just "abcde"}, 123, "file")]
          , [Paragraph [Field ("SHA256", "abcde 123 file")]] )
        , ( Right [(CheckSums {md5sum = Just "abcde", sha1 = Nothing, sha256 = Nothing}, 123, "file")]
          , [Paragraph [Field ("md5sum", "abcde 123 file")]] )
        ]
    ]

testReleaseIndexes :: Either String [(CheckSums, Integer, FilePath)] -> [Paragraph' Text] -> Int -> Test
testReleaseIndexes expected ps i = TestCase $
    either (assertError pfx) (assertEqual pfx) expected
        $ indexesInRelease (const True) (Control ps)
    where pfx = "indexesInRelease" <> show i

assertError :: (Eq a, Show a) => String -> String -> a -> Assertion
assertError preface errExpected action = do
    r <- try $ evaluate action
    case r of
        Left (ErrorCall err) -> assertEqual preface errExpected err
        Right _ -> assertFailure $ preface <> " did not call error"
