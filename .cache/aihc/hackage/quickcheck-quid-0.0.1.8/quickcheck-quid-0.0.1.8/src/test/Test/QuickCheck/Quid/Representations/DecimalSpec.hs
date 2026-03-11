{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.QuickCheck.Quid.Representations.DecimalSpec
    where

import Internal.Test.QuickCheck.Quid
    ( Quid, naturalToQuid )
import Internal.Test.QuickCheck.Quid.Representations.Decimal
    ( Decimal (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, parallel )
import Test.Hspec.Unit
    ( UnitTestData (..), unitTests )
import Test.QuickCheck
    ( Arbitrary (..) )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )

import Prelude hiding
    ( (^) )

import qualified Prelude
import qualified Test.QuickCheck.Classes as Laws

spec :: Spec
spec = do

    parallel $ describe "Lawfulness of type class instances" $ do
        testLawsMany @(Decimal Quid)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]

    parallel $ describe "Unit tests" $ do
        unitTests_show_decimal_naturalToQuid

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

unitTests_show_decimal_naturalToQuid :: Spec
unitTests_show_decimal_naturalToQuid = unitTests
    "unitTests_show_decimal_naturalToQuid"
    (show . Decimal . naturalToQuid)
    (mkTest <$> tests)
  where
    mkTest :: (Natural, String) -> UnitTestData Natural String
    mkTest (params, result) = UnitTestData {params, result}

    (^) :: Natural -> Natural -> Natural
    (^) = (Prelude.^)

    tests =
        [ (0, "0")
        , (1, "1")
        , (8, "8")
        , (9, "9")

        , (10^0, "1")
        , (10^1, "10")
        , (10^2, "100")
        , (10^3, "1000")
        , (10^4, "10000")
        , (10^5, "100000")
        , (10^6, "1000000")
        , (10^7, "10000000")

        , (10^0 - 1, "0")
        , (10^1 - 1, "9")
        , (10^2 - 1, "99")
        , (10^3 - 1, "999")
        , (10^4 - 1, "9999")
        , (10^5 - 1, "99999")
        , (10^6 - 1, "999999")
        , (10^7 - 1, "9999999")
        ]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

deriving via Quid instance Arbitrary (Decimal Quid)
