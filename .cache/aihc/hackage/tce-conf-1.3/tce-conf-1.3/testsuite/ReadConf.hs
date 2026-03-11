-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module ReadConf
   ( tests )
   where

import Test.HUnit
   ( Test (..)
   , assertEqual
   )

import TCE.Data.ReadConf ( readConfig )


tests :: Test
tests = TestList
   [ testGood
   , testListWrap
   , testMissingField
   , testExtraField
   , testComment
   ]


data Config = Config
   { foo :: String
   , bar :: [String]
   , baz :: Bool
   , qux :: Int
   }
   deriving (Eq, Read, Show)


testConf :: Config
testConf = Config
   { foo = "foo"
   , bar = [ "one", "two", "three" ]
   , baz = True
   , qux = 42
   }


testGood :: Test
testGood = TestCase $
   assertEqual "show/read round trip, good data"
      (Right testConf)
      (readConfig . show $ testConf)


testListWrap :: Test
testListWrap = TestCase $ do
   let actual = (readConfig . (\s -> "[" ++ s ++ "]")
         . show $ testConf) :: Either String Config
   assertEqual "read bad data, wrapped in a list"
      (Left "ERROR parsing config data")
      actual


testMissingField :: Test
testMissingField = TestCase $ do
   let content = "(Config {foo = \"foo\", bar = [\"one\",\"two\",\"three\"], baz = True })"
   let actual = (readConfig content) :: Either String Config
   assertEqual "read bad data, missing field"
      (Left "ERROR parsing config data")
      actual


testExtraField :: Test
testExtraField = TestCase $ do
   let content = "(Config {foo = \"foo\", bar = [\"one\",\"two\",\"three\"], baz = True, qux = 42, frotz = \"extra\" })"
   let actual = (readConfig content) :: Either String Config
   assertEqual "read bad data, extra field"
      (Left "ERROR parsing config data")
      actual


testComment :: Test
testComment = TestCase $ do
   let content = "(Config {foo = \"foo\", bar = [\"one\",\"two\",\"three\"]\n-- This is a comment\n, baz = True, qux = 42 })"
   let actual = (readConfig content) :: Either String Config
   assertEqual "otherwise good data with a comment"
      (Right testConf)
      actual
