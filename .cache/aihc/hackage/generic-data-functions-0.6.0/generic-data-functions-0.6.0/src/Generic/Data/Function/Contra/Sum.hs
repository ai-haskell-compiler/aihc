{-# LANGUAGE UndecidableInstances #-} -- due to typeclass design
{-# LANGUAGE AllowAmbiguousTypes #-}  -- due to typeclass design

module Generic.Data.Function.Contra.Sum where

import GHC.Generics
import Generic.Data.Function.Common.Generic
import Generic.Data.Function.Contra.Constructor

import Data.Functor.Contravariant.Divisible
import Data.Functor.Contravariant

class GContraSum tag gf where
    gContraSum :: GenericContraF tag String -> GenericContraF tag (gf p)

instance (GContraSumD tag gf, Contravariant (GenericContraF tag))
  => GContraSum tag (D1 cd gf) where
    gContraSum f = contramap unM1 (gContraSumD @tag f)

class GContraSumD tag gf where
    gContraSumD :: GenericContraF tag String -> GenericContraF tag (gf p)

instance GContraCSum tag (l :+: r) => GContraSumD tag (l :+: r) where
    gContraSumD = gContraCSum @tag

instance GContraCSum tag (C1 cc gf) => GContraSumD tag (C1 cc gf) where
    gContraSumD = gContraCSum @tag

instance Divisible (GenericContraF tag) => GContraSumD tag V1 where
    gContraSumD _ = contramap absurdV1 conquer

-- TODO rename (? had this on foldmap sum)
class GContraCSum tag gf where
    gContraCSum :: GenericContraF tag String -> GenericContraF tag (gf p)

instance
  ( Decidable (GenericContraF tag)
  , GContraCSum tag l
  , GContraCSum tag r
  ) => GContraCSum tag (l :+: r) where
    gContraCSum f = choose genericSumToEither (gContraCSum @tag f) (gContraCSum @tag f)
      where genericSumToEither = \case L1 l -> Left l; R1 r -> Right r

instance
  ( Divisible (GenericContraF tag)
  , GContraC tag gf, Constructor c
  ) => GContraCSum tag (C1 c gf) where
    gContraCSum f = divide (\(M1 g) -> (conName' @c, g)) f (gContraC @tag)
