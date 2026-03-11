{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE AllowAmbiguousTypes #-} -- due to type class design
{-# LANGUAGE CPP #-} -- due to TypeErrorMessage hack

{- | 'foldMap' for sum types, where constructors are encoded by index
     (distance from first/leftmost constructor) in a single byte, which is
     prepended to their contents.

TODO. Clumsy and limited. And yet, still handy enough I think.
-}

module Generic.Data.Function.FoldMap.SumConsByte where

import GHC.Generics
import GHC.TypeLits
import Data.Kind ( Type, Constraint )
import Generic.Data.Function.Common.TypeLits ( natVal'' )
import Generic.Data.Function.FoldMap.Constructor
  ( GFoldMapC(gFoldMapC)
  , GenericFoldMap(type GenericFoldMapM) )

import Data.Word ( Word8 )

class GFoldMapSumConsByte tag f where
    gFoldMapSumConsByte
        :: (Word8 -> GenericFoldMapM tag) -> f p -> GenericFoldMapM tag

instance GFoldMapSumConsByte tag f => GFoldMapSumConsByte tag (D1 c f) where
    gFoldMapSumConsByte f (M1 a) = gFoldMapSumConsByte @tag f a

instance
  ( FitsInByte (SumArity (l :+: r))
  , GFoldMapCSumCtrArityByte tag 0 (l :+: r)
  , GFoldMapCSumCtr tag (l :+: r)
  , Semigroup (GenericFoldMapM tag)
  ) => GFoldMapSumConsByte tag (l :+: r) where
    gFoldMapSumConsByte f lr =
        gFoldMapCSumCtrArityByte @tag @0 f lr <> gFoldMapCSumCtr @tag lr

instance GFoldMapSumConsByte m (C1 c f) where
    gFoldMapSumConsByte _ = undefined

instance GFoldMapSumConsByte m V1 where
    gFoldMapSumConsByte _ = undefined

---

-- | Sum type handler handling constructors only. Useful if you handle
--   constructor prefixes elsewhere.
class GFoldMapCSumCtr tag f where gFoldMapCSumCtr :: f p -> GenericFoldMapM tag

instance (GFoldMapCSumCtr tag l, GFoldMapCSumCtr tag r)
  => GFoldMapCSumCtr tag (l :+: r) where
    gFoldMapCSumCtr = \case L1 l -> gFoldMapCSumCtr @tag l
                            R1 r -> gFoldMapCSumCtr @tag r

instance GFoldMapC tag f => GFoldMapCSumCtr tag (C1 c f) where
    gFoldMapCSumCtr (M1 a) = gFoldMapC @tag a

---

class GFoldMapCSumCtrArityByte tag (arity :: Natural) f where
    gFoldMapCSumCtrArityByte
        :: (Word8 -> GenericFoldMapM tag) -> f p -> GenericFoldMapM tag

instance
  ( GFoldMapCSumCtrArityByte tag arity l
  , GFoldMapCSumCtrArityByte tag (arity + SumArity l) r
  ) => GFoldMapCSumCtrArityByte tag arity (l :+: r) where
    gFoldMapCSumCtrArityByte f = \case
      L1 l -> gFoldMapCSumCtrArityByte @tag @arity                f l
      R1 r -> gFoldMapCSumCtrArityByte @tag @(arity + SumArity l) f r

instance KnownNat arity => GFoldMapCSumCtrArityByte tag arity (C1 c f) where
    gFoldMapCSumCtrArityByte f _ = f (fromIntegral (natVal'' @arity))

---

type family SumArity (a :: Type -> Type) :: Natural where
    SumArity (C1 c a) = 1
    SumArity (x :+: y) = SumArity x + SumArity y

type FitsInByte n = FitsInByteResult (n <=? 255)

type family FitsInByteResult (b :: Bool) :: Constraint where
    FitsInByteResult 'True = ()
    FitsInByteResult 'False = TypeErrorMessage
        "TODO ya type had more than 255 constructors"

type family TypeErrorMessage (a :: Symbol) :: Constraint where
#if MIN_VERSION_base(4,9,0)
    TypeErrorMessage a = TypeError ('Text a)
-- GHC < 8.0 does not support empty closed type families
#elif __GLASGOW_HASKELL__ < 800
    TypeErrorMessage a = a ~ ""
#endif
