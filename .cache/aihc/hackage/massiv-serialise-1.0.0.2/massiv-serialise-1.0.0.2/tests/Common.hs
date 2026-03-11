{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Common
  ( module X
  , roundtrip
  , roundtripArray
  , roundtripArraySpec
  , roundtripArraysSpec
  ) where
import Data.Massiv.Array
import Codec.Serialise as X
import Data.Typeable
import Massiv.Serialise as X
import Test.Hspec as X
import Test.Hspec.QuickCheck as X
import Test.Massiv.Core
import Test.QuickCheck as X

roundtrip :: (Eq a, Show a, Serialise a) => a -> Property
roundtrip val = deserialise (serialise val) === val


roundtripArray ::
     forall r ix e.
     ( Load r ix e
     , Serialise (Array r ix e)
     , Eq (Array r ix e)
     , Show (Array r ix e)
     )
  => Array r ix e
  -> Property
roundtripArray arr =
  let arr' = deserialise (serialise arr)
  in (arr' === arr) .&&. (getComp arr' === getComp arr)

roundtripArraySpec ::
     forall r ix e.
     ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Typeable e
     , Arbitrary ix
     , Load r ix e
     , Arbitrary e
     , Serialise (Array r ix e)
     )
  => Spec
roundtripArraySpec =
  prop (showsType @(Array r ix e) "") $ roundtripArray @r @ix @e


roundtripArraysSpec ::
     forall r e.
     ( Eq (Array r Ix1 e)
     , Show (Array r Ix1 e)
     , Serialise (Array r Ix1 e)
     , Load r Ix1 e
     , Eq (Array r Ix2 e)
     , Show (Array r Ix2 e)
     , Serialise (Array r Ix2 e)
     , Load r Ix2 e
     , Eq (Array r Ix3 e)
     , Show (Array r Ix3 e)
     , Serialise (Array r Ix3 e)
     , Load r Ix3 e
     , Eq (Array r Ix4 e)
     , Show (Array r Ix4 e)
     , Load r Ix4 e
     , Serialise (Array r Ix4 e)
     , Eq (Array r Ix5 e)
     , Show (Array r Ix5 e)
     , Serialise (Array r Ix5 e)
     , Load r Ix5 e
     , Typeable e
     , Arbitrary e
     )
  => Spec
roundtripArraysSpec =
  describe (showsType @e "") $ do
    roundtripArraySpec @r @Ix1 @e
    roundtripArraySpec @r @Ix2 @e
    roundtripArraySpec @r @Ix3 @e
    roundtripArraySpec @r @Ix4 @e
    roundtripArraySpec @r @Ix5 @e
