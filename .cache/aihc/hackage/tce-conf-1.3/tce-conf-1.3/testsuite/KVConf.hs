-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KVConf
   ( tests )
   where

import Data.Map ( fromList )
import Test.HUnit
   ( Test (..)
   , assertEqual
   )

import TCE.Data.KVConf ( parseToArgs, parseToMap )


tests :: Test
tests = TestList
   [ testParseToMap
   , testParseToArgs
   ]


configFileContents :: String
configFileContents = init $ unlines
   [ "foo=one"
   , "# a comment"
   , ""
   , "bar"
   , "baz-blorp=2"
   , "qux = false"
   ]


testParseToMap :: Test
testParseToMap = TestCase $
   assertEqual "parseToMap"
      (fromList [("bar",""),("baz-blorp","2"),("foo","one"),("qux","false")])
      (parseToMap configFileContents)


testParseToArgs :: Test
testParseToArgs = TestCase $
   assertEqual "parseToArgs"
      ["--foo=one","--bar","--baz-blorp=2","--qux=false"]
      (parseToArgs configFileContents)
