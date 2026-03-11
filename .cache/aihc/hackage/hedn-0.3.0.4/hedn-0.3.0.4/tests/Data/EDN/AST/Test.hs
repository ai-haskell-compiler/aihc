{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.EDN.AST.Test
  ( tests
  ) where

import Hedgehog
  ( Group, Property
  , discover, forAll, property, tripping, withTests
  , (===)
  )

import Data.Text (Text)

import qualified Data.EDN.AST.Types as EDN
import qualified Data.EDN.AST.Gen as EDN
import qualified Data.EDN.AST.Parser as EDN
import qualified Data.EDN.AST.Printer as EDN

tests :: Group
tests = $$(discover)

type ASTParser = Text -> Either String EDN.TaggedValue

prop_generated :: Property
prop_generated = property $ do
  tv <- forAll EDN.genTaggedValue
  tripping tv EDN.renderText (EDN.parseText "<generated>" :: ASTParser)

prop_regr_empty_containers :: Property
prop_regr_empty_containers = withTests 1 . property $ do
  emptyList <- either fail pure $ EDN.parseText "<regr_empty_containers>" "( )"
  emptyList === EDN.NoTag (EDN.List mempty)

  emptyVec <- either fail pure $ EDN.parseText "<regr_empty_containers>" "[ ]"
  emptyVec === EDN.NoTag (EDN.Vec mempty)

  emptySet <- either fail pure $ EDN.parseText "<regr_empty_containers>" "#{ }"
  emptySet === EDN.NoTag (EDN.Set mempty)

  emptyMap <- either fail pure $ EDN.parseText "<regr_empty_containers>" "{ }"
  emptyMap === EDN.NoTag (EDN.Map mempty)
