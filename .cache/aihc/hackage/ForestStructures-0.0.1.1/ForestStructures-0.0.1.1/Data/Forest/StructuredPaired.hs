
-- | A semi-specialized forest structure with the following atomic elements:
-- (i) unstructured regions of type @a@, (ii) binary paired regions of type
-- @(b,b)@ with a recursing tree (or insertion between the two @b@'s), (iii)
-- juxtaposition of two elements, and (iv) an empty structure.

module Data.Forest.StructuredPaired where

import Control.Lens
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Monoid
import GHC.Generics (Generic)

import Data.Forest.Static



-- | A structured forest.

data SPForest r t
  -- | An (unstructured) region with the structured forest. In case @r@ forms a
  -- monoid @SPJ (SPR a) (SPR b) `equiv` SPR (a<>b)@ should hold.
  = SPR r
  -- | A tree within the forest brackets the forest on the left and right side
  -- with elements of type @t@.
  | SPT t (SPForest r t) t
  -- | Juxtaposition of two forests. This allows for simple concatenation of
  -- forests. In particular, there is no particular position, while lists
  -- prefer @x:xs@ vs @xs++[x]@.
  | SPJ [SPForest r t]
  -- | An empty forest. @SPJ SPE SPE `equiv` SPE@ should hold.
  | SPE
  deriving (Read,Show,Eq,Ord,Generic)
makePrisms ''SPForest

instance Functor (SPForest r) where
  fmap f = \case
    SPR r     → SPR r
    SPT l t r → SPT (f l) (fmap f t) (f r)
    SPJ xs    → SPJ (map (fmap f) xs)
    SPE       → SPE
  {-# Inlinable fmap #-}

instance Foldable (SPForest r) where
  foldMap = bifoldMap (const mempty)
  {-# Inlinable foldMap #-}

instance Traversable (SPForest r) where
  traverse = bitraverse pure
  {-# Inlinable traverse #-}

instance Bifunctor SPForest where
  first f = \case
    SPR r     → SPR (f r)
    SPT l t r → SPT l (first f t) r
    SPJ xs    → SPJ (map (first f) xs)
    SPE       → SPE
  {-# Inlinable first #-}
  second = fmap
  {-# Inlinable second #-}
  bimap f g = \case
    SPR r     → SPR (f r)
    SPT l t r → SPT (g l) (bimap f g t) (g r)
    SPJ xs    → SPJ (map (bimap f g) xs)
    SPE       → SPE
  {-# Inlinable bimap #-}

instance Bifoldable SPForest where
  bifoldMap f g = \case
    SPR r     → f r
    SPT l t r → g l <> bifoldMap f g t <> g r
    SPJ xs    → error "Bifoldable" -- mconcatMap (bifoldMap f g) xs
    SPE       → mempty
  {-# Inlinable bifoldMap #-}

instance Bitraversable SPForest where
  bitraverse f g = \case
    SPR r     → SPR <$> f r
    SPT l t r → SPT <$> g l <*> bitraverse f g t <*> g r
    SPJ xs    → error "Bitraversable" -- SPJ <$> bitraverse f g l <*> bitraverse f g r
    SPE       → pure SPE
  {-# Inlinable bitraverse #-}



-- | Structured Forests can be transformed into static forests.
--
-- TODO types involved!

toStaticForest ∷ SPForest r t → Forest p v a
toStaticForest = undefined

