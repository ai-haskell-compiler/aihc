{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ParseSpec where

import qualified Data.Attoparsec.Text
import qualified Data.Text.IO
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO

import SpecHelper

parseSample fname = do
  txt <- Data.Text.IO.readFile $ "./test/samples/" ++ fname
  case Data.Attoparsec.Text.parseOnly parseNarInfo txt of
    Left e -> error e
    Right ni -> pure (txt, ni)

roundTrip fname = do
  (txt, ni) <- parseSample fname
  let built = Data.Text.Lazy.Builder.toLazyText $ buildNarInfo ni
  (Data.Text.Lazy.toStrict built) `shouldBe` txt

referencesEmpty fname = do
  (_, ni) <- parseSample fname
  references ni `shouldSatisfy` Prelude.null

spec :: Spec
spec = do
  it "roundtrips samples" $ do
    mapM_ (roundTrip . show) [0, 1]
  it "parses empty references" $ do
    mapM_ (referencesEmpty . show) [1]

main :: IO ()
main = hspec spec
