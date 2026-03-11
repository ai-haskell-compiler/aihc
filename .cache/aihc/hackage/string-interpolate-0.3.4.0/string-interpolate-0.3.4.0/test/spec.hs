{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE LambdaCase                 #-}

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as LB
import qualified Data.ByteString.Lazy    as LB
import           Data.Char               ( chr, isSpace )
import           Data.Foldable           ( foldMap )
import qualified Data.HashMap.Strict     as HM
import           Data.List               ( sort, intersperse )
import           Data.Semigroup
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import           Data.Word

import Language.Haskell.TH
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )

import Control.Monad.IO.Class ( liftIO )

import "hspec" Test.Hspec
import "hspec" Test.Hspec.QuickCheck
import "hspec-core" Test.Hspec.Core.Runner
import "QuickCheck" Test.QuickCheck
import "quickcheck-instances" Test.QuickCheck.Instances.ByteString ()
import "QuickCheck" Test.QuickCheck.Monadic
import "quickcheck-unicode" Test.QuickCheck.Unicode

import Data.String.Interpolate ( i, iii, __i, __i'E, __i'L, iii'E, iii'L )
import Data.String.Interpolate.Conversion hiding
  ( build, finalize, interpolate, ofString, chompSpaces )
import Data.String.Interpolate.Types ( InterpSegment(..) )
import Data.String.Interpolate.Parse ( parseInterpSegments )

main :: IO ()
main = hspecWith testConfig $ parallel $ do
  describe "parseInterpSegments" $ modifyMaxSuccess (const 10000) $ modifyMaxSize (const 500) $ do
    -- A pretty weaksauce test, but we've had issues with this before.
    prop "terminates" $
      \(UTF8S str) -> parseInterpSegments str `seq` True

  describe "i" $ modifyMaxSuccess (const 10000) $ modifyMaxSize (const 500) $ do
    it "should allow an escaped backslash right before an interp" $ do
      let var :: String = "bar"
          expected :: String = "foo\\bar"
      [i|foo\\#{var}|] `shouldBe` expected

    it "should only escape verbatim segments a single time" $ do
      let expected :: String = "\\\\\\\\"
      [i|\\\\\\\\|] `shouldBe` expected

    it "should error on hanging #" $ do
      runQ (quoteExp i "#") `shouldThrow` anyException

    it "should error on unterminated backslash" $ do
      runQ (quoteExp i "\\") `shouldThrow` anyException

    it "should error on unknown escape sequence" $ do
      runQ (quoteExp i "\\c") `shouldThrow` anyException

    it "should error on unclosed expression" $ do
      runQ (quoteExp i "#{") `shouldThrow` anyException

    it "should parse TypeApplications" $ do
      let expected :: String = "2"
      [i|#{show @Int 2}|] `shouldBe` expected

    -- This test is primarily a regression test against a performance issue
    -- caused by GHC needing to unify all the proxies passed in the generated
    -- code.
    -- See <https://gitlab.com/williamyaoh/string-interpolate/-/merge_requests/72>
    -- and <https://gitlab.haskell.org/ghc/ghc/-/issues/24984>
    -- It would be preferable if we had something that would actually *fail*
    -- if compilation took too long, but compilation of the test suite being
    -- slow should be enough of a smoke signal.
    it "should work with many interpolations" $
      let x = ()
          (expected :: String) = concat $ intersperse " : " (replicate 780 (show x))
      in [iii| #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} :
          #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x} : #{x}
      |] `shouldBe` expected

    context "when using String as a parameter" $ do
      prop "just interpolating should be id" $
        \(UTF8S str) -> [i|#{str}|] == str

      prop "should passthrough a conversion to strict Text and back unchanged" $
        \(UTF8S str) -> iID @String @T.Text str
      prop "should passthrough a conversion to lazy Text and back unchanged" $
        \(UTF8S str) -> iID @String @LT.Text str
      prop "should passthrough a conversion to strict ByteString and back unchanged" $
        \(UTF8S str) -> iID @String @B.ByteString str
      prop "should passthrough a conversion to lazy ByteString and back unchanged" $
        \(UTF8S str) -> iID @String @LB.ByteString str

    context "when using strict Text as a parameter" $ do
      prop "just interpolating should be id" $
        \(t :: T.Text) -> [i|#{t}|] == t

      prop "should passthrough a conversion to String and back unchanged" $ iID @T.Text @String
      prop "should passthrough a conversion to lazy Text and back unchanged" $ iID @T.Text @LT.Text
      prop "should passthrough a conversion to strict ByteString and back unchanged" $ iID @T.Text @B.ByteString
      prop "should passthrough a conversion to lazy ByteString and back unchanged" $ iID @T.Text @LB.ByteString

    context "when using lazy Text as a parameter" $ do
      prop "just interpolating should be id" $
        \(lt :: LT.Text) -> [i|#{lt}|] == lt

      prop "should passthrough a conversion to String and back unchanged" $ iID @LT.Text @String
      prop "should passthrough a conversion to strict Text and back unchanged" $ iID @LT.Text @T.Text
      prop "should passthrough a conversion to strict ByteString and back unchanged" $ iID @LT.Text @B.ByteString
      prop "should passthrough a conversion to lazy ByteString and back unchanged" $ iID @LT.Text @LB.ByteString

    context "when using strict ByteString as a parameter" $ do
      prop "just interpolating should be id" $
        \(b :: B.ByteString) -> [i|#{b}|] == b

      prop "should passthrough a conversion to lazy ByteString and back unchanged" $ iID @B.ByteString @LB.ByteString

      context "and the ByteString is valid UTF8" $ do
        prop "should passthrough a conversion to String and back unchanged" $ do
          \(UTF8BS b) -> iID @B.ByteString @String b
        prop "should passthrough a conversion to strict Text and back unchanged" $ do
          \(UTF8BS b) -> iID @B.ByteString @T.Text b
        prop "should passthrough a conversion to lazy Text and back unchanged" $ do
          \(UTF8BS b) -> iID @B.ByteString @LT.Text b

    context "when using lazy ByteString as a parameter" $ do
      prop "just interpolating should be id" $
        \(lb :: LB.ByteString) -> [i|#{lb}|] == lb

      prop "should passthrough a conversion to strict ByteString and back unchanged" $ iID @LB.ByteString @B.ByteString

      context "and the ByteString is valid UTF8" $ do
        prop "should passthrough a conversion to String and back unchanged" $
          \(UTF8LBS lb) -> iID @LB.ByteString @String lb
        prop "should passthrough a conversion to strict Text and back unchanged" $
          \(UTF8LBS lb) -> iID @LB.ByteString @T.Text lb
        prop "should passthrough a conversion to lazy Text and back unchanged" $
          \(UTF8LBS lb) -> iID @LB.ByteString @LT.Text lb

    context "when using Char as a parameter" $ do
      prop "interpolating into a String shouldn't have quotes" $
        \(UTF8C c) -> [i|#{c}|] == [c]
      prop "interpolating into strict Text shouldn't have quotes" $
        \(UTF8C c) -> [i|#{c}|] == T.singleton c
      prop "interpolating into lazy Text shouldn't have quotes" $
        \(UTF8C c) -> [i|#{c}|] == LT.singleton c
      prop "interpolating into strict ByteString shouldn't have quotes" $
        \(UTF8C c) -> [i|#{c}|] == (LB.toStrict $ LB.toLazyByteString $ LB.charUtf8 c)
      prop "interpolating into lazy ByteString shouldn't have quotes" $
        \(UTF8C c) -> [i|#{c}|] == (LB.toLazyByteString $ LB.charUtf8 c)

    context "when interpolating into strict ByteString" $ do
      it "should handle literal Unicode strings correctly" $ do
        let interpolated :: B.ByteString = [i|λ|]
            expected :: B.ByteString = "\xCE\xBB"
        interpolated `shouldBe` expected

    context "when interpolating into lazy ByteString" $ do
      it "should handle literal Unicode strings correctly" $ do
        let interpolated :: LB.ByteString = [i|λ|]
            expected :: LB.ByteString = "\xCE\xBB"
        interpolated `shouldBe` expected

  describe "__i" $ modifyMaxSuccess (const 250) $ modifyMaxSize (const 500) $ do
    context "when there are newlines" $ do
      it "handles a small code snippet correctly/1" $ do
        let interpolated :: T.Text =
              [__i|
                id :: a -> a
                id x = y
                  where y = x
              |]
            expected :: T.Text = "id :: a -> a\nid x = y\n  where y = x"
        interpolated `shouldBe` expected

      it "handles a small code snippet correctly/2" $ do
        let interpolated :: T.Text =
              [__i|


                This is an example message.

                  Title: Foo
                  Description: Bar
                  Categories:



                This is an example body.

              |]
            expected :: T.Text = "This is an example message.\n\n  Title: Foo\n  Description: Bar\n  Categories:\n\n\n\nThis is an example body."
        interpolated `shouldBe` expected

      it "handles a small code snippet correctly/3" $ do
        let input :: Int = 42
            interpolated :: T.Text =
              [__i|
                add :: Int -> Int -> Int
                add x y =
                  let result = x + y + #{input}
                    in result
              |]
            expected :: T.Text = "add :: Int -> Int -> Int\nadd x y =\n  let result = x + y + 42\n    in result"
        interpolated `shouldBe` expected

      it "handles tabs" $ do
        let interpolated :: T.Text =
              [__i|
		id :: a -> a
		id x = y
			where y = x
              |]
            expected = "id :: a -> a\nid x = y\n\twhere y = x"
        interpolated `shouldBe` expected

    -- prop "produces the same output for different indentation levels" $
    --   \(segs :: [InterpSegment], indent :: Word8, offset :: Word8) -> monadicIO $ do
    --     let interpLines = lines $ interpToString $
    --           filter (\case { Expression _ -> False; _ -> True }) segs
    --         fi = fromIntegral
    --         lessIO = runQ $ quoteExp __i (unlines $ leftPad (fi (indent + 1)) ' ' <$> interpLines)
    --         moreIO = runQ $ quoteExp __i (unlines $ leftPad (fi (indent + offset + 2)) ' ' <$> interpLines)
    --     lessExp <- run lessIO
    --     moreExp <- run moreIO
    --     assert $! lessExp == moreExp

  --   prop "non-whitespace chars in output same as in input" $
  --     \(SpaceyText t) -> charFrequencies [__i|#{t}|] == charFrequencies t

  --   prop "output string length <= input string length" $
  --     \(SpaceyText t) -> T.length [__i|#{t}|] <= T.length t

  --   prop "output words = input words" $
  --     \(SpaceyText t) -> T.words t == T.words [__i|#{t}|]

  describe "__i'E" $ modifyMaxSuccess (const 250) $ modifyMaxSize (const 500) $ do
    context "when there are newlines" $ do
      it "handles a small code snippet correctly/1" $ do
        let interpolated :: T.Text =
              [__i'E|
                id :: a -> a
                id x = y
                  where y = x
              |]
            expected :: T.Text = "\nid :: a -> a\nid x = y\n  where y = x\n              "
        interpolated `shouldBe` expected

      it "handles a small code snippet correctly/2" $ do
        let interpolated :: T.Text =
              [__i'E|


                This is an example message.

                  Title: Foo
                  Description: Bar
                  Categories:



                This is an example body.

              |]
            expected :: T.Text = "\n\n\nThis is an example message.\n\n  Title: Foo\n  Description: Bar\n  Categories:\n\n\n\nThis is an example body.\n\n              "
        interpolated `shouldBe` expected

      it "handles a small code snippet correctly/3" $ do
        let input :: Int = 42
            interpolated :: T.Text =
              [__i'E|
                add :: Int -> Int -> Int
                add x y =
                  let result = x + y + #{input}
                    in result
              |]
            expected :: T.Text = "\nadd :: Int -> Int -> Int\nadd x y =\n  let result = x + y + 42\n    in result\n              "
        interpolated `shouldBe` expected

  describe "__i'L" $ modifyMaxSuccess (const 250) $ modifyMaxSize (const 500) $ do
    context "when there are newlines" $ do
      it "handles a small code snippet correctly/1" $ do
        let interpolated :: T.Text =
              [__i'L|

                id :: a -> a
                id x = y
                  where y = x
              |]
            expected :: T.Text = "\nid :: a -> a\nid x = y\n  where y = x\n              "
        interpolated `shouldBe` expected

      it "handles a small code snippet correctly/2" $ do
        let interpolated :: T.Text =
              [__i'L|


                This is an example message.

                  Title: Foo
                  Description: Bar
                  Categories:



                This is an example body.

              |]
            expected :: T.Text = "\nThis is an example message.\n\n  Title: Foo\n  Description: Bar\n  Categories:\n\n\n\nThis is an example body.\n"
        interpolated `shouldBe` expected

      it "handles a small code snippet correctly/3" $ do
        let input :: Int = 42
            interpolated :: T.Text =
              [__i'L|
                add :: Int -> Int -> Int
                add x y =
                  let result = x + y + #{input}
                    in result
              |]
            expected :: T.Text = "\nadd :: Int -> Int -> Int\nadd x y =\n  let result = x + y + 42\n    in result\n              "
        interpolated `shouldBe` expected

  describe "iii" $ modifyMaxSuccess (const 10000) $ modifyMaxSize (const 500) $ do
    context "when there is whitespace" $ do
      it "collapses a small example of whitespace" $ do
        let interpolated :: T.Text = [iii| foo   bar      baz |]
            expected :: T.Text = "foo bar baz"
        interpolated `shouldBe` expected

      it "collapses a small example of newlines" $ do
        let interpolated :: T.Text =
              [iii|
                Lorem ipsum dolor sit amet,
                consectetur adipiscing elit.
                Aenean congue iaculis dui,
                at iaculis sapien interdum nec.
              |]
            expected :: T.Text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean congue iaculis dui, at iaculis sapien interdum nec."
        interpolated `shouldBe` expected

  describe "iii'E" $ modifyMaxSuccess (const 10000) $ modifyMaxSize (const 500) $ do
    context "when there is whitespace" $ do
      it "collapses a small example of whitespace" $ do
        let interpolated :: T.Text = [iii'E| foo   bar      baz |]
            expected :: T.Text = "foo bar baz"
        interpolated `shouldBe` expected

      it "collapses a small example of newlines" $ do
        let interpolated :: T.Text =
              [iii'E|

                Lorem ipsum dolor sit amet,
                consectetur adipiscing elit.
                Aenean congue iaculis dui,
                at iaculis sapien interdum nec.

              |]
            expected :: T.Text = "\n\nLorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean congue iaculis dui, at iaculis sapien interdum nec.\n\n              "
        interpolated `shouldBe` expected

  describe "iii'L" $ modifyMaxSuccess (const 10000) $ modifyMaxSize (const 500) $ do
    context "when there is whitespace" $ do
      it "collapses a small example of whitespace" $ do
        let interpolated :: T.Text = [iii'L| foo   bar      baz |]
            expected :: T.Text = "foo bar baz"
        interpolated `shouldBe` expected

      it "collapses a small example of newlines" $ do
        let interpolated :: T.Text =
              [iii'L|

                Lorem ipsum dolor sit amet,
                consectetur adipiscing elit.
                Aenean congue iaculis dui,
                at iaculis sapien interdum nec.

              |]
            expected :: T.Text = "\nLorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean congue iaculis dui, at iaculis sapien interdum nec.\n"
        interpolated `shouldBe` expected

testConfig :: Config
testConfig = defaultConfig
  { configDiff = True
  , configFailureReport = Nothing
  }

iID :: forall from to fromflag toflag.
       ( Eq from
       , Interpolatable fromflag to from
       , Interpolatable toflag from to
       )
    => from
    -> Bool
iID from =
  let to :: to = [i|#{from}|]
      from' :: from = [i|#{to}|]
  in from == from'

-- |
-- Add the given number of the specific characters to the left.
leftPad :: Int -> Char -> String -> String
leftPad amt c t = replicate amt c <> t

-- |
-- The default Arbitrary for Char generates U+FFFF and U+FFFE, which aren't
-- valid Unicode. Sigh...
newtype UTF8Char = UTF8C { unUTF8C :: Char }
  deriving newtype (Eq, Show)

newtype UTF8String = UTF8S { unUTF8S :: String }
  deriving newtype (Eq, Show)

newtype UTF8ByteString = UTF8BS B.ByteString
  deriving newtype (Eq, Show)
newtype UTF8LazyByteString = UTF8LBS LB.ByteString
  deriving newtype (Eq, Show)

newtype SpaceyText = SpaceyText T.Text
  deriving newtype (Eq, Show)
newtype NonwhitespaceText = NonwhitespaceText T.Text
  deriving newtype (Eq, Show)

instance Arbitrary UTF8Char where
  arbitrary = UTF8C <$> unicodeChar
  shrink (UTF8C c) = UTF8C <$> shrinkChar c

instance Arbitrary UTF8String where
  arbitrary = do
    chars <- listOf arbitrary
    pure $ UTF8S (unUTF8C <$> chars)
  shrink (UTF8S str) = UTF8S <$> shrink str

instance Arbitrary T.Text where
  arbitrary = T.pack . unUTF8S <$> arbitrary
  shrink t = if T.null t || T.length t == 1
    then []
    else let mid = T.length t `div` 2
         in [T.take mid t, T.drop mid t]

instance Arbitrary LT.Text where
  arbitrary = LT.pack . unUTF8S <$> arbitrary
  shrink lt = if LT.null lt || LT.length lt == 1
    then []
    else let mid = LT.length lt `div` 2
         in [LT.take mid lt, LT.drop mid lt]

instance Arbitrary UTF8ByteString where
  arbitrary = UTF8BS . LB.toStrict . LB.toLazyByteString . foldMap LB.charUtf8 . unUTF8S
    <$> arbitrary

instance Arbitrary UTF8LazyByteString where
  arbitrary = UTF8LBS . LB.toLazyByteString . foldMap LB.charUtf8 . unUTF8S
    <$> arbitrary

-- Basically, we want this to be an 'alternation' of sequences of printable
-- characters and whitespace characters.
instance Arbitrary SpaceyText where
  arbitrary = SpaceyText . foldMap id
    <$> scale
          (round . sqrt @Double . fromIntegral)
          (listOf (oneof [whitespace, nonwhitespace]))

instance Arbitrary NonwhitespaceText where
  arbitrary = NonwhitespaceText <$> nonwhitespace

instance Arbitrary InterpSegment where
  arbitrary = oneof
    [ Verbatim <$> listOf nonwhitespaceChar
    , Expression <$> arbitrary
    , Spaces <$> arbitrary
    , Tabs <$> arbitrary
    ]

  shrink (Verbatim t) = Verbatim <$> shrink t
  shrink (Expression t) = []
  shrink (Spaces n) = [Spaces (n `div` 2), Spaces (n-1)]
  shrink (Tabs n) = [Tabs (n `div` 2), Tabs (n-1)]

charFrequencies :: T.Text -> HM.HashMap Char Int
charFrequencies = T.foldl' (flip $ HM.alter increment) HM.empty . T.filter (not . isSpace)
  where increment :: Maybe Int -> Maybe Int
        increment Nothing  = Just 1
        increment (Just x) = Just (x + 1)

whitespace :: Gen T.Text
whitespace = T.pack
  <$> listOf1 (elements [' ', '\r', '\t', '\n', '\x1680', '\x2000', '\x2006'])

nonwhitespace :: Gen T.Text
nonwhitespace = T.pack
  <$> listOf1 nonwhitespaceChar

nonwhitespaceChar :: Gen Char
nonwhitespaceChar = unicodeChar `suchThat` (not . isSpace)

unicodeChar :: Gen Char
unicodeChar = chr `fmap` points
  where points = flip suchThat (not . reserved) $ oneof
          [ ascii
          , plane0
          , plane1
          , plane2
          , plane14
          ]
