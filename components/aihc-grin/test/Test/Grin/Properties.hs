{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Properties
  ( grinPropertyTests,
  )
where

import Aihc.Grin.Parser (parseProgram, renderParseError)
import Aihc.Grin.Pretty (renderProgram)
import Data.Text qualified as T
import Hedgehog (Property, annotate, failure, forAll, property, (===))
import Test.Grin.Arbitrary (genGrinProgram)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

grinPropertyTests :: TestTree
grinPropertyTests =
  testGroup
    "GRIN properties"
    [testProperty "parseProgram . renderProgram = id" prop_programRoundTrip]

prop_programRoundTrip :: Property
prop_programRoundTrip = property $ do
  program <- forAll genGrinProgram
  let rendered = T.pack (renderProgram program)
  annotate (T.unpack rendered)
  case parseProgram rendered of
    Left parseError -> annotate (renderParseError parseError) >> failure
    Right actual -> do
      -- GrinVar equality ignores runtime representations. Compare derived forms to include all fields.
      show actual === show program
