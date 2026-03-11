{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Data.String.Transform
import qualified Data.Text             as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests
  = [ testGroup "SmallCheck" smallCheckTest
    , testGroup "Unit tests" hunitTest
    ]

smallCheckTest :: [TestTree]
smallCheckTest
  = [ testProperty "s == toString (toByteStringStrict s)"
      (\(s :: String) -> s == toString (toByteStringStrict s))
    , testProperty "s == toString (toByteStringLazy s)"
      (\(s :: String) -> s == toString (toByteStringLazy s))
    , testProperty "s == toString (toShortByteString s)"
      (\(s :: String) -> s == toString (toShortByteString s))
    , testProperty "s == toString (toTextStrict s)"
      (\(s :: String) -> s == toString (toTextStrict s))
    , testProperty "s == toString (toTextLazy s)"
      (\(s :: String) -> s == toString (toTextLazy s))
    , testProperty "a == b ⇔ toByteStringStrict a == toByteStringStrict b"
      (\(a :: String) (b :: String) ->
          let p = a == b
              q = toByteStringStrict a == toByteStringStrict b
          in p == q)
    ]

hunitTest :: [TestTree]
hunitTest
  = [ testCase "toByteStringStrict 日本語" (toString (toByteStringStrict "日本語") @?= "日本語")
    , testCase "toString Text" (toString (T.pack "foo") @?= "foo")
    ]
