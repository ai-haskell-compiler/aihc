{-|
Module:      Spec.Data.ShortTextSpec
Copyright:   (C) 2014-2018 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'ShortText' (from the @text-short@ package).
-}
module Spec.Data.ShortTextSpec (main, spec) where

import           Data.Proxy                (Proxy (..))
import           Data.Text.Short           (ShortText)

import           Spec.Utils                (matchesTextShowSpec)

import           Test.Hspec                (Spec, describe, hspec, parallel)
import           Test.QuickCheck.Instances ()

import           TextShow.Data.ShortText   ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "ShortText" $
        matchesTextShowSpec (Proxy :: Proxy ShortText)
