{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Instances.Text.XHtml
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @xhtml@ library.
-}
module Instances.Text.XHtml () where

import           GHC.Generics (Generic)

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), Gen)

import           Text.XHtml.Frameset (Html, HtmlAttr, HotLink(..), strAttr, toHtml)
import           Text.XHtml.Table (HtmlTable, cell)

instance Arbitrary Html where
    arbitrary = toHtml <$> (arbitrary :: Gen String)

instance Arbitrary HtmlAttr where
    arbitrary = strAttr <$> arbitrary <*> arbitrary

instance Arbitrary HotLink where
    arbitrary = genericArbitrary

instance Arbitrary HtmlTable where
    arbitrary = cell <$> (arbitrary :: Gen Html)

deriving instance Generic HotLink
