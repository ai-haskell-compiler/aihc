{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , ScopedTypeVariables
  , StandaloneDeriving
  #-}

module Main where

import Path (parseAbsDir, parseAbsFile, Path, Abs, Dir, File)
import Path.Extended (Location (..), QueryParam, locationAbsParser, printLocation, LocationPath (..))

import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Attoparsec.Text (parseOnly)
import Control.Exception (SomeException)
import Control.Monad (replicateM)

import Test.Tasty (defaultMain, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck (Arbitrary (..), suchThat)
import Test.QuickCheck.Gen (Gen, choose, elements, listOf, oneof)
import Test.QuickCheck.Instances ()



newtype ArbitraryLocation = ArbitraryLocation
  { getArbitraryLocation :: Location Abs
  } deriving (Eq, Show)

instance Arbitrary ArbitraryLocation where
  arbitrary = do
    locPath <- oneof [Dir <$> arbitraryAbsDir, File <$> arbitraryAbsFile]
    locQueryParams <- arbitraryQueryParams
    locFragment <- arbitraryFragment
    pure $ ArbitraryLocation Location
      { locPath
      , locQueryParams
      , locFragment
      }
    where
      arbitraryWord :: Gen String
      arbitraryWord = listOf (elements (['A'..'Z']++['a'..'z'])) `suchThat` (not . null)

      arbitraryFragment :: Gen (Maybe String)
      arbitraryFragment = do
        build <- arbitrary
        if not build
          then pure Nothing
          else fmap Just arbitraryWord

      arbitraryQueryParams :: Gen [QueryParam]
      arbitraryQueryParams = do
        build <- arbitrary
        if not build
          then pure []
          else do
            n <- choose (1,4)
            let arbitraryKV :: Gen QueryParam
                arbitraryKV = do
                  k <- arbitraryWord
                  buildVal <- arbitrary
                  mV <-
                    if buildVal
                    then Just <$> arbitraryWord
                    else pure Nothing
                  pure (k,mV)
            replicateM n arbitraryKV

      arbitraryAbsDir :: Gen (Path Abs Dir)
      arbitraryAbsDir = do
        n <- choose (0,5)
        xs <- replicateM n (T.pack <$> arbitraryWord)
        pure $ case parseAbsDir ( case xs of
                                    [] -> "/"
                                    _ -> T.unpack ("/" <> T.intercalate "/" xs <> "/")
                                ) of
                 Left (e :: SomeException) ->
                   error $ "Can't parse abs dir! " <> show e
                 Right path -> path

      arbitraryAbsFile :: Gen (Path Abs File)
      arbitraryAbsFile = do
        n <- choose (1,5)
        xs <- replicateM n (T.pack <$> arbitraryWord)
        pure $ case parseAbsFile (T.unpack ("/" <> T.intercalate "/" xs)) of
                 Left (e :: SomeException) ->
                   error $ "Can't parse abs file! " <> show e
                 Right path -> path



printParseIso :: ArbitraryLocation -> Bool
printParseIso (ArbitraryLocation loc) =
  Right loc == parseOnly locationAbsParser (printLocation loc)



main :: IO ()
main = defaultMain $
  testGroup "Path.Extended"
    [ QC.testProperty "Print / Parse Iso" printParseIso
    ]
