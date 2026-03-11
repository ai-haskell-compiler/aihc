module Data.Generics.Traversable.Core where

import GHC.Exts (Constraint)

class GTraversable (c :: * -> Constraint) a where
  -- | Applicative traversal over (a subset of) immediate subterms. This is
  -- a generic version of 'Data.Traversable.traverse'.
  --
  -- The supplied function is applied only to the «interesting» subterms.
  --
  -- Other subterms are lifted using 'pure', and the whole structure is
  -- folded back using 'Control.Applicative.<*>'.
  --
  -- 'gtraverse' has a default implementation @const pure@, which works for
  -- types without interesting subterms (in particular, atomic types).
  gtraverse
    :: (Applicative f)
    => (forall d . c d => d -> f d)
    -> a -> f a
  gtraverse _ x = pure x
