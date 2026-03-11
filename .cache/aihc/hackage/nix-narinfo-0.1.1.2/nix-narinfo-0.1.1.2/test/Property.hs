{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Text (Text)
import System.FilePath
import Nix.NarInfo (NarInfo(..))
import Test.QuickCheck (Arbitrary(..))

import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text
import qualified Data.Text.Lazy.Builder
import qualified Nix.NarInfo
import qualified Test.QuickCheck

instance Arbitrary Text where
    arbitrary = fmap Data.Text.pack arbitrary

instance Arbitrary (NarInfo FilePath Text Text) where
    arbitrary = do
        storePath   <- arbitrary
        url         <- arbitrary
        compression <- arbitrary
        fileHash    <- arbitrary
        fileSize    <- arbitrary
        narHash     <- arbitrary
        narSize     <- arbitrary
        references  <- arbitrary
        deriver     <- arbitrary
        system      <- arbitrary
        sig         <- arbitrary
        ca          <- arbitrary
        return (NarInfo {..})

property :: NarInfo FilePath Text Text -> Bool
property narinfo0 = eitherRes == Right narinfo0
  where
    builder = Nix.NarInfo.buildNarInfo narinfo0

    text = Data.Text.Lazy.Builder.toLazyText builder

    result =
        Data.Attoparsec.Text.Lazy.parse Nix.NarInfo.parseNarInfo text

    eitherRes =
        Data.Attoparsec.Text.Lazy.eitherResult result

main :: IO ()
main = Test.QuickCheck.quickCheck property
