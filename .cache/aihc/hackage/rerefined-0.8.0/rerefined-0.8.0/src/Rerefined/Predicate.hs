{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Base definitions for refinement predicates.

module Rerefined.Predicate
  ( Refine(validate)
  , Refine1(validate1)
  , RefineFailure(..)
  , Predicate(..)
  , KnownPredicateName
  , predicateName
  ) where

import GHC.Exts ( Proxy#, proxy# )
import Data.Text.Builder.Linear qualified as TBL
import GHC.TypeLits ( Natural, Symbol, KnownSymbol, symbolVal' )

-- | Types which define refinements on other types.
class Predicate p where
    -- | Predicate name.
    --
    -- Predicate names should aim to communicate the meaning of the predicate as
    -- clearly and concisely as possible.
    --
    -- Consider using the package @type-level-show@ to build this.
    type PredicateName (d :: Natural) p :: Symbol
        -- ^ TODO d: the operator precedence of the enclosing context (a number
        --   from 0 to 11). Function application has precedence 10.

-- | Constraint for reifying a predicate name.
type KnownPredicateName p = KnownSymbol (PredicateName 0 p)

-- | Reify predicate name to a 'String'.
--
-- Using this regrettably necessitates @UndecidableInstances@, due to the type
-- family in the constraint. It sucks, but that's life.
predicateName :: forall p. KnownPredicateName p => String
predicateName = symbolVal' (proxy# @(PredicateName 0 p))

-- | Refine @a@ with predicate @p@.
--
-- Note that 'validate' implementations decide what failure information is
-- emitted. They don't even have to emit their own predicate name, even though
-- 'Predicate' is in the context. This is useful in certain cases such as
-- "Rerefined.Predicate.Via".
--
-- That does mean that the 'Predicate' context may not be used occasionally. But
-- it's very useful, since otherwise combinator predicates have to constantly
-- state @(Refine p a, Predicate p)@, and all predicates should have a pretty
-- name even if it doesn't get printed under normal circumstances.
class Predicate p => Refine p a where
    -- | Validate predicate @p@ for the given @a@.
    --
    -- 'Nothing' indicates success. 'Just' contains a validation failure.
    validate :: Proxy# p -> a -> Maybe RefineFailure

-- | Refine functor type @f@ with functor predicate @p@.
--
-- By not making the contained type accessible, we ensure refinements apply
-- @forall a. f a@. That is, refinements here apply only to the functor
-- structure, and not the stored elements.
class Predicate p => Refine1 p f where
    -- | Validate predicate @p@ for the given @f a@.
    validate1 :: Proxy# p -> f a -> Maybe RefineFailure

-- | Predicate validation failure.
data RefineFailure = RefineFailure
  { refineFailurePredicate :: TBL.Builder
  -- ^ The name of the predicate that failed.
  --
  -- Obtained via 'predicateName'.

  , refineFailureDetail    :: TBL.Builder
  -- ^ Precise failure detail e.g. which of the inner predicates of an @And@
  --   combinator predicate failed.

  , refineFailureInner     :: [RefineFailure]
  -- ^ Any wrapped errors, for combinator predicates.
  --
  -- What these are, and their order, should be noted in 'refineFailureDetail'.
  } deriving stock Show
