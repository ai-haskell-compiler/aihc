{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Test helpers.
module Test.Hspec.GraphQL
    ( shouldResolve
    , shouldResolveTo
    ) where

import Control.Monad.Catch (MonadCatch)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Language.GraphQL.Error
import Language.GraphQL.Execute
import Test.Hspec.Expectations
    ( Expectation
    , expectationFailure
    , shouldBe
    , shouldSatisfy
    )

-- | Asserts that a query resolves to some value.
shouldResolveTo :: (MonadCatch m, Serialize b, Eq b, Show b)
    => Either (ResponseEventStream m b) (Response b)
    -> b
    -> Expectation
shouldResolveTo (Right Response{ errors = Seq.Empty, data' }) expected =
    data' `shouldBe` expected
shouldResolveTo _ _ = expectationFailure
    "the query is expected to resolve to a value, but it resolved to an event stream"

-- | Asserts that the response doesn't contain any errors.
shouldResolve :: (MonadCatch m, Serialize b)
    => (Text -> IO (Either (ResponseEventStream m b) (Response b)))
    -> Text
    -> Expectation
shouldResolve executor query = do
    actual <- executor query
    case actual of
        Right Response{ errors } -> errors `shouldSatisfy` Seq.null
        _ -> expectationFailure
            "the query is expected to resolve to a value, but it resolved to an event stream"
