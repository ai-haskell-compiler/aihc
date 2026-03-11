{-|
Module:      Spec.Data.AesonSpec
Copyright:   (C) 2022 Steve Mao
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Aeson' (from the @aeson@ package).
-}
module Spec.Data.AesonSpec (main, spec) where

import           Data.Proxy                (Proxy (..))
import           Data.Aeson                (Key, Value)
import           Data.Aeson.KeyMap         (KeyMap)

import           Spec.Utils                (matchesTextShowSpec)

import           Test.Hspec                (Spec, describe, hspec, parallel)

import           TextShow.Data.Aeson       ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Aeson Value" $
        matchesTextShowSpec (Proxy :: Proxy Value)
    describe "Aeson Key" $
        matchesTextShowSpec (Proxy :: Proxy Key)
    describe "Aeson KeyMap" $
        matchesTextShowSpec (Proxy :: Proxy (KeyMap Char))
