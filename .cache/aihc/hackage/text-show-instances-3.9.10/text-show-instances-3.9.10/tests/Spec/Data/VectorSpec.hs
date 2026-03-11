{-|
Module:      Spec.Data.VectorSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Vector' types.
-}
module Spec.Data.VectorSpec (main, spec) where

import           Data.Proxy (Proxy(..))
import qualified Data.Vector as B (Vector)
import           Data.Vector.Fusion.Bundle.Size (Size)
import qualified Data.Vector.Primitive as P (Vector)
import qualified Data.Vector.Storable as S (Vector)
import qualified Data.Vector.Unboxed as U (Vector)

import           Instances.Data.Vector ()

import           Spec.Utils (matchesTextShowSpec, matchesTextShow1Spec)

import           Test.Hspec (Spec, describe, hspec, parallel)

import           TextShow.Data.Vector ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "(boxed) Vector Char" $ do
        let p :: Proxy (B.Vector Char)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
    describe "(primitive) Vector Char" $
        matchesTextShowSpec (Proxy :: Proxy (P.Vector Char))
    describe "(storable) Vector Char" $
        matchesTextShowSpec (Proxy :: Proxy (S.Vector Char))
    describe "(unboxed) Vector Char" $
        matchesTextShowSpec (Proxy :: Proxy (U.Vector Char))
    describe "Size" $
        matchesTextShowSpec (Proxy :: Proxy Size)
