-- | Generic functors, foldables, and traversables.

module Generic.Functor
  ( -- * Derive functors

    -- ** Derive Functor, Bifunctor, Foldable, Traversable

    -- *** DerivingVia
    -- | The type synonyms 'GenericFunctor' and 'GenericBifunctor' can be
    -- used with the @DerivingVia@ extension to derive 'Functor', 'Bifunctor', and 'Foldable'.
    -- Sadly, 'Traversable' cannot be derived-via.
    GenericFunctor(..)
  , GenericBifunctor(..)

    -- *** Generic method definitions
  , gfmap
  , gfoldMap
  , gtraverse

    -- **** Bifunctors
  , gbimap
  , gfirst
  , gsecond
  , gbifoldMap
  , gbitraverse

    -- * Auxiliary classes

    -- ** Related to standard classes
  , GFunctor()
  , GFoldable()
  , GFoldMap()
  , GTraversable()
  , GTraverse()

    -- *** Bifunctors
  , GBifunctor()
  , GBimap()
  , GFirst()
  , GSecond()
  , GBifoldable()
  , GBifoldMap()
  , GBitraversable()
  , GBitraverse()
  ) where

import Generic.Functor.Internal
