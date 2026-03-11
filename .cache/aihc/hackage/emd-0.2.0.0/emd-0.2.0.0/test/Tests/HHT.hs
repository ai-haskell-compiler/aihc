{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeInType                               #-}
{-# LANGUAGE TypeOperators                            #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Tests.HHT (
    hhtTests
  ) where

import           Data.Functor.Identity
import           Data.Proxy
import           GHC.TypeNats
import           Hedgehog
import           Numeric.EMD
import           Numeric.HHT
import           Test.Tasty
import           Tests.Util
import qualified Hedgehog.Range        as Range

hhtTests :: TestTree
hhtTests = groupTree $$(discover)

prop_ihht :: Property
prop_ihht = property $ withSize (Range.linear 1 5) $ \(_ :: Proxy n) -> do
    xs <- forAll $ generateData @n
    tripping (CE xs) (hht @_ @(2^n-1) defaultEO . getCE) (Identity . CE . ihht)


