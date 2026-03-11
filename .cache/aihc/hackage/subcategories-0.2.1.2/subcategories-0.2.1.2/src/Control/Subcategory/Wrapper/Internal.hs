{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Subcategory.Wrapper.Internal (
  module Control.Subcategory.Wrapper.Internal,
) where

import Control.Applicative
import Control.Monad.Fix (MonadFix)
import Control.Monad.Zip (MonadZip)
import Data.Coerce
import Data.Kind (Type)
import Data.MonoTraversable
import Data.Pointed
import Data.Semialign (Align, Unalign)
import Data.Zip (Semialign, Unzip, Zip)
import GHC.Base (MonadPlus)

newtype WrapFunctor f (a :: Type) = WrapFunctor {runFunctor :: f a}
  deriving newtype (Functor, Applicative, Alternative, Monad, Foldable)
  deriving newtype (MonadPlus, MonadFix, MonadFail)
  deriving newtype (Pointed, MonadZip, Unalign, Align, Semialign, Zip, Unzip)

instance (Traversable f) => Traversable (WrapFunctor f) where
  traverse f = fmap WrapFunctor . traverse f . runFunctor

type role WrapMono representational nominal

{- | Similar to 'WrappedMono' from @mono-traversable,
  but uses @newtype@ instaed of GADTs, which is efficient.
  To restrict the construction, we hide genuine constructor
  and expose the constrained pattern synonym 'WrapMono' and
  specifies type roles tightly (note: the role for @mono@
  should NOT be representational honestly; indeed, @WrapMono mono a@
  could be coerced to @WrapMono mono' a@ iff @mono@ and @mono' are
  representationally equivalent __AND__ @Element a ~ Element a@.)
-}
newtype WrapMono mono b = WrapMono' mono
  deriving newtype (MonoFoldable, MonoFunctor, Monoid, Semigroup, MonoPointed)
  deriving newtype (GrowingAppend)

type instance Element (WrapMono mono b) = Element mono

pattern WrapMono :: (b ~ Element mono) => (b ~ Element mono) => mono -> WrapMono mono b
pattern WrapMono {unwrapMono} = WrapMono' unwrapMono

coerceToMono :: WrapMono mono (Element mono) -> mono
{-# INLINE coerceToMono #-}
coerceToMono = coerce

withMonoCoercible ::
  ((Coercible (WrapMono mono (Element mono)) mono) => r) ->
  r
{-# INLINE withMonoCoercible #-}
withMonoCoercible = id
