{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.Type.OutSpec
    ( spec
    ) where

import Language.GraphQL.Type
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "Value" $
        it "supports overloaded strings" $
            let nietzsche = "Goldstaub abblasen." :: Value
             in nietzsche `shouldBe` String "Goldstaub abblasen."
