{-|
Module:      Spec.Control.Monad.TransSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for monad transformers.
-}
module Spec.Control.Monad.TransSpec (main, spec) where

import           Control.Monad.Trans.Except              (ExceptT)
import           Control.Monad.Trans.Identity            (IdentityT)
import           Control.Monad.Trans.Maybe               (MaybeT)
import qualified Control.Monad.Trans.Writer.Lazy   as WL (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as WS (WriterT)

import           Data.Proxy (Proxy(..))

import           Instances.Control.Monad.Trans ()

import           Spec.Utils (matchesTextShowSpec)

import           Test.Hspec (Spec, describe, hspec, parallel)
import           Test.QuickCheck.Instances ()

import           TextShow.Control.Monad.Trans ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "ExceptT Char Maybe Int" $
        matchesTextShowSpec (Proxy :: Proxy (ExceptT Char Maybe Int))
    describe "IdentityT Maybe Int" $
        matchesTextShowSpec (Proxy :: Proxy (IdentityT Maybe Int))
    describe "Maybe [] Int" $
        matchesTextShowSpec (Proxy :: Proxy (MaybeT [] Int))
    describe "lazy WriterT String Maybe Int" $
        matchesTextShowSpec (Proxy :: Proxy (WL.WriterT String Maybe Int))
    describe "strict WriterT String Maybe Int" $
        matchesTextShowSpec (Proxy :: Proxy (WS.WriterT String Maybe Int))
