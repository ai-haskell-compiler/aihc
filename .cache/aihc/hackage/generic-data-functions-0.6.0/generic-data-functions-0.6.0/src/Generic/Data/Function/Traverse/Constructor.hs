{-# LANGUAGE UndecidableInstances #-} -- due to type class design
{-# LANGUAGE AllowAmbiguousTypes  #-}  -- due to type class design

module Generic.Data.Function.Traverse.Constructor where

import GHC.Generics
import GHC.TypeLits
import Generic.Data.Function.Common.TypeLits ( natVal'', symbolVal'' )
import Generic.Data.Function.Common.Error ( eNoEmpty )

import Control.Applicative qualified as Applicative
import Control.Applicative ( Alternative(empty) )

import Data.Kind ( type Type, type Constraint )

import Generic.Data.Wrappers ( NoRec0, type ENoRec0, EmptyRec0 )

-- | Implementation enumeration type class for generic 'traverse'.
--
-- The type variable is uninstantiated, used purely as a tag.
-- Good types include the type class used inside (providing you define the
-- type class/it's not an orphan instance), or a custom void data type.
-- See the binrep library on Hackage for an example.
class GenericTraverse tag where
    -- | The target 'Applicative' to 'traverse' to.
    type GenericTraverseF tag :: Type -> Type

    -- | The type class providing the action in 'traverse' for permitted types.
    type GenericTraverseC tag a :: Constraint

    -- | The action in 'traverse' (first argument).
    --
    -- We include data type metadata because this function is useful for monadic
    -- parsers, which can record it in error messages. (We don't do it for
    -- foldMap because it's pure.)
    genericTraverseAction
        :: GenericTraverseC tag a
        => String       {- ^ data type name -}
        -> String       {- ^ constructor name -}
        -> Maybe String {- ^ record name (if present) -}
        -> Natural      {- ^ field index -}
        -> GenericTraverseF tag a

    -- | Action to run when trying to parse a 'V1' (void data type).
    --
    -- Defaults to 'error', but you may wrap it in your functor if it pleases.
    genericTraverseV1 :: GenericTraverseF tag (V1 p)
    -- NOTE: Prior to GHC 9.8, we can't default to a compile-time error here,
    -- due to TypeError limitations. From GHC 9.8, we can use Unsatisfiable.
    -- But I'm still not sure it works how I would like it to. Let's just stick
    -- with 'error' for now. One can always use generic-type-asserts.
    {-
    default genericTraverseV1
        :: Unsatisfiable (ENoEmpty tag) => GenericTraverseF tag (V1 p)
    genericTraverseV1 = unsatisfiable
    -}
    genericTraverseV1 = error eNoEmpty

type ENoEmpty tag =
         'Text "Attempted to derive generic traverse for the void data type"
    :$$: 'Text "To override, implement genericTraverseV1 on:"
    :$$: 'Text "instance GenericTraverse (" :<>: 'ShowType tag :<>: 'Text ")"

-- | 'traverse' over types with no fields in any constructor.
instance GenericTraverse (NoRec0 (f :: Type -> Type)) where
    type GenericTraverseF (NoRec0 f) = f
    type GenericTraverseC (NoRec0 _) _ = TypeError ENoRec0
    genericTraverseAction = undefined

-- | 'traverse' over types where all fields are replaced with the functor's
--   'empty'.
--
-- Note that one may write a valid instance using a 'Monoid' on @a@s instead.
-- I don't think you should. But I can't explain why.
instance GenericTraverse (EmptyRec0 (f :: Type -> Type)) where
    type GenericTraverseF (EmptyRec0 f) = f
    type GenericTraverseC (EmptyRec0 f) _ = Alternative f
    genericTraverseAction _ _ _ _ = empty

class GTraverseC tag (cd :: Symbol) (cc :: Symbol) (si :: Natural) gf where
    gTraverseC :: GenericTraverseF tag (gf p)

instance
  ( Applicative (GenericTraverseF tag)
  , GTraverseC tag cd cc si                 l
  , GTraverseC tag cd cc (si + ProdArity r) r
  ) => GTraverseC tag cd cc si (l :*: r) where
    gTraverseC = Applicative.liftA2 (:*:)
                   (gTraverseC @tag @cd @cc @si)
                   (gTraverseC @tag @cd @cc @(si + ProdArity r))

instance
  ( GenericTraverse tag, GenericTraverseC tag a
  , Functor (GenericTraverseF tag)
  , KnownNat si, ReifyMaybeSymbol mSelName, KnownSymbol cc, KnownSymbol cd
  ) => GTraverseC tag cd cc si (S1 (MetaSel mSelName _ms2 _ms3 _ms4) (Rec0 a)) where
    gTraverseC = (M1 . K1) <$> genericTraverseAction @tag cd cc cs si
      where
        cs = reifyMaybeSymbol @mSelName
        cd = symbolVal'' @cd
        cc = symbolVal'' @cc
        si = natVal'' @si

instance Applicative (GenericTraverseF tag) => GTraverseC tag cd cc 0 U1 where
    gTraverseC = pure U1

type family ProdArity (f :: Type -> Type) :: Natural where
    ProdArity (S1 c f)  = 1
    ProdArity (l :*: r) = ProdArity l + ProdArity r

class ReifyMaybeSymbol (mstr :: Maybe Symbol) where
    reifyMaybeSymbol :: Maybe String
instance ReifyMaybeSymbol Nothing where reifyMaybeSymbol = Nothing
instance KnownSymbol str => ReifyMaybeSymbol (Just str) where
    reifyMaybeSymbol = Just (symbolVal'' @str)
