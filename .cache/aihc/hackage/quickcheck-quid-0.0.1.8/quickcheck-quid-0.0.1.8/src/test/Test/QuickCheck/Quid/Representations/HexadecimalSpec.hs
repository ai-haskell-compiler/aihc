{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.QuickCheck.Quid.Representations.HexadecimalSpec
    where

import Internal.Test.QuickCheck.Quid
    ( Quid, naturalToQuid )
import Internal.Test.QuickCheck.Quid.Representations.Hexadecimal
    ( Hexadecimal (..) )
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
        testLawsMany @(Hexadecimal Quid)
            [ Laws.showLaws
            , Laws.showReadLaws
            ]

    parallel $ describe "Unit tests" $ do
        unitTests_show_hexadecimal_naturalToQuid

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

unitTests_show_hexadecimal_naturalToQuid :: Spec
unitTests_show_hexadecimal_naturalToQuid = unitTests
    "unitTests_show_hexadecimal_naturalToQuid"
    (show . Hexadecimal . naturalToQuid)
    (mkTest <$> tests)
  where
    mkTest :: (Natural, String) -> UnitTestData Natural String
    mkTest (params, result) = UnitTestData {params, result}

    (^) :: Natural -> Natural -> Natural
    (^) = (Prelude.^)

    tests =
        [ ( 0, "0x0")
        , ( 1, "0x1")
        , (14, "0xe")
        , (15, "0xf")

        , (16 +  0, "0x10")
        , (16 +  1, "0x11")
        , (16 + 14, "0x1e")
        , (16 + 15, "0x1f")

        , (2 * 16 +  0, "0x20")
        , (2 * 16 +  1, "0x21")
        , (2 * 16 + 14, "0x2e")
        , (2 * 16 + 15, "0x2f")

        , (15 * 16 +  0, "0xf0")
        , (15 * 16 +  1, "0xf1")
        , (15 * 16 + 14, "0xfe")
        , (15 * 16 + 15, "0xff")

        , (16^0, "0x1")
        , (16^1, "0x10")
        , (16^2, "0x100")
        , (16^3, "0x1000")
        , (16^4, "0x10000")
        , (16^5, "0x100000")
        , (16^6, "0x1000000")
        , (16^7, "0x10000000")

        , (16^0 - 1, "0x0")
        , (16^1 - 1, "0xf")
        , (16^2 - 1, "0xff")
        , (16^3 - 1, "0xfff")
        , (16^4 - 1, "0xffff")
        , (16^5 - 1, "0xfffff")
        , (16^6 - 1, "0xffffff")
        , (16^7 - 1, "0xfffffff")
        ]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

deriving via Quid instance Arbitrary (Hexadecimal Quid)
