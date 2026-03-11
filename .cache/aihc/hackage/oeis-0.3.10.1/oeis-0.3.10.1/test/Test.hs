-- base
import Data.List (isPrefixOf)
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, openFile, utf8)

-- HUnit
import Test.HUnit ((@?=), assertBool, assertFailure)

-- test-framework
import Test.Framework (Test, defaultMain, testGroup)

-- test-framework-hunit
import Test.Framework.Providers.HUnit (testCase)

-- oeis
import Math.OEIS
import Math.OEIS.Internal

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

--------------------------------------------------------------------------------

tests :: [Test]
tests =
    [ testGroup "internals"
      [ testCase "parseOEIS" test_parseOEIS
      ]
    ]

--------------------------------------------------------------------------------

test_parseOEIS :: IO ()
test_parseOEIS = do
    r0 <- readFileUtf8 "test/data/id_rsp.txt"
    case parseOEIS r0 of
      [r]  -> do
          check r
          examples r @?= []
      _ -> assertFailure ""

    r1 <- readFileUtf8 "test/data/seq_rsp.txt"
    case parseOEIS r1 of
      [r,_,_,_,_,_,_,s,_,_]  -> do
          check r
          assertBool "" $ not $ null $ examples s
      _ -> assertFailure ""
  where
    check r = do
        catalogNums r @?= ["A000040","M0652","N0241"]
        assertBool "" $ firstFewPrimes `isPrefixOf` sequenceData r
        signedData  r @?= []
        description r @?= "The prime numbers."
        assertBool "" $ not $ null $ references r
        assertBool "" $ not $ null $ links r
        assertBool "" $ not $ null $ formulas r
        assertBool "" $ not $ null $ xrefs r
        author      r @?= "_N. J. A. Sloane_."
        assertBool "" $ not $ null $ programs r
        assertBool "" $ not $ null $ extensions r
        assertBool "" $ [Core,Nonn,Nice,Easy] `elemAll` keywords r
    firstFewPrimes = [2,3,5,7,11,13,17,19,23,29,31,37]
    elemAll xs ys = all (`elem` ys) xs

--------------------------------------------------------------------------------

readFileUtf8 :: FilePath -> IO String
readFileUtf8 fp = do
    h <- openFile fp ReadMode
    hSetEncoding h utf8
    hGetContents h

--------------------------------------------------------------------------------

{-# ANN module "HLint: ignore Use camelCase" #-}
