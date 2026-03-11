{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- thanks to type manipulation

{- TODO
* in some low-level Haskell code (probably bytestring or other GHC lib) I've
  seen a pattern of binding in a certain way in order to hint a value's
  stickiness (whether it will change) to GHC. Probably whether to put it on LHS
  or RHS.
  * yeah, I think see https://wiki.haskell.org/Let_vs._Where
  * and Data.ByteString.Builder.Internal for a purposeful eta expansion
-}

{- | Generics for bytezap's struct serializer.

We can't use my generic-data-functions library, because we're doing more than
just basic monoidal composition. But I still want the same pluggable generics,
where the user provides the class to use for base cases. So I do that. However,
unlike g-d-f, the class info can't be provided via the user-selected monoid,
because you don't select that. Instead, we take a simple "index" type. It's
pretty much the same idea, surprisingly. This way, we can provide a few sensible
"versions" like in g-f-d, while primarily designing for DIY.
-}

module Bytezap.Struct.Generic where

import Bytezap.Struct
import GHC.Generics
import GHC.Exts
import Bytezap.Common.Generic ( type GTFoldMapCAddition )
import Bytezap.Common.TypeNats ( natValInt )
import Data.Kind
import GHC.TypeNats
import DeFun.Core ( type (~>) )

-- | Class for holding info on class to use for poking base cases.
--
-- The type is just used to map to class info. It is never instantiated.
-- By packing @KnownSizeOf@ into here, we don't need to enforce a type-level
-- solution! Now it's up to you how you want to track your constant lengths.
--
-- We stay unboxed here because the internals are unboxed, just for convenience.
-- Maybe this is bad, let me know.
class GPokeBase tag where
    -- | The state token of our poker.
    type GPokeBaseSt tag

    -- | The type class that provides base case poking.
    --
    -- The type class should provide a function that looks like 'gPokeBase'.
    type GPokeBaseC tag a :: Constraint

    gPokeBase :: GPokeBaseC tag a => a -> Poke# (GPokeBaseSt tag)

    type GPokeBaseLenTF tag :: Type ~> Natural

class GPoke tag f where gPoke :: f p -> Poke# (GPokeBaseSt tag)

instance GPoke tag f => GPoke tag (D1 c f) where gPoke = gPoke @tag . unM1
instance GPoke tag f => GPoke tag (C1 c f) where gPoke = gPoke @tag . unM1

instance
  ( GPoke tag l
  , GPoke tag r
  , GPokeBase tag
  , lenL ~ GTFoldMapCAddition (GPokeBaseLenTF tag) l
  , KnownNat lenL
  ) => GPoke tag (l :*: r) where
    -- TODO moved os and s0 to RHS because base is const and those aren't?
    -- will this change anything?? idk!!!!
    gPoke (l :*: r) base# = \os# s0 ->
        case gPoke @tag l base# os# s0 of
          s1 -> gPoke @tag r base# (os# +# lenL#) s1
      where
        !(I# lenL#) = natValInt @lenL

instance (GPokeBase tag, GPokeBaseC tag a) => GPoke tag (S1 c (Rec0 a)) where
    gPoke = gPokeBase @tag . unK1 . unM1

-- | Wow, look! Nothing!
instance GPoke tag U1 where gPoke U1 _base# = \_os# s0 -> s0
