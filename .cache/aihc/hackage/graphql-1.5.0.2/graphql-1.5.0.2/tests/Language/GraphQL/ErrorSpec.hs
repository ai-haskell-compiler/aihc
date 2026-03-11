{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.ErrorSpec
    ( spec
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Language.GraphQL.Error
import qualified Language.GraphQL.Type as Type
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Text.Megaparsec (PosState(..))
import Text.Megaparsec.Error (ParseError(..), ParseErrorBundle(..))
import Text.Megaparsec.Pos (SourcePos(..), mkPos)

spec :: Spec
spec = describe "parseError" $
    it "generates response with a single error" $ do
        let parseErrors = TrivialError 0 Nothing mempty :| []
            posState = PosState
                { pstateInput = ""
                , pstateOffset = 0
                , pstateSourcePos = SourcePos "" (mkPos 1) (mkPos 1)
                , pstateTabWidth = mkPos 1
                , pstateLinePrefix = ""
                }
        Response Type.Null actual <-
            parseError (ParseErrorBundle parseErrors posState)
        length actual `shouldBe` 1
