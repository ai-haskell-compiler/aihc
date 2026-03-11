{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances
  , MultiParamTypeClasses, TypeFamilies
  #-}

-- | This module provides the 'HeapItem' type family along with necessary
-- instance declarations used to translate between inserted items and the
-- priority-value pairs needed by the minimum priority heap of the module
-- "Data.Heap.Internal".
module Data.Heap.Item
    ( -- * Type aliases
      Heap, MinHeap, MaxHeap, MinPrioHeap, MaxPrioHeap
      -- * The HeapItem type family
    , HeapItem(..)
    , MinPolicy, MaxPolicy, FstMinPolicy, FstMaxPolicy
      -- * Auxiliary functions
    , splitF
    ) where

import Data.Heap.Internal
import Text.Read

-- | This type alias is an abbreviation for a 'HeapT' which uses the 'HeapItem'
-- instance of @pol item@ to organise its elements.
type Heap pol item = HeapT (Prio pol item) (Val pol item)

-- | A 'Heap' which will always extract the minimum first.
type MinHeap a = Heap MinPolicy a

-- | A 'Heap' which will always extract the maximum first.
type MaxHeap a = Heap MaxPolicy a

-- | A 'Heap' storing priority-value pairs @(prio, val)@. The order of elements
-- is solely determined by the priority @prio@, the value @val@ has no influence.
-- The priority-value pair with minmal priority will always be extracted first.
type MinPrioHeap prio val = Heap FstMinPolicy (prio, val)

-- | A 'Heap' storing priority-value pairs @(prio, val)@. The order of elements
-- is solely determined by the priority @prio@, the value @val@ has no influence.
-- The priority-value pair with maximum priority will always be extracted first.
type MaxPrioHeap prio val = Heap FstMaxPolicy (prio, val)

-- | @'HeapItem' pol item@ is a type class for items that can be stored in a
-- 'HeapT'. A raw @'HeapT' prio val@ only provides a minimum priority heap (i. e.
-- @val@ doesn't influence the ordering of elements and the pair with minimal
-- @prio@ will be extracted first, see 'HeapT' documentation). The job of this
-- class is to translate between arbitrary @item@s and priority-value pairs
-- @('Prio' pol item, 'Val' pol item)@, depending on the policy @pol@ to be used.
-- This way, we are able to use 'HeapT' not only as 'MinPrioHeap', but also as
-- 'MinHeap', 'MaxHeap', 'MaxPrioHeap' or a custom implementation. In short: The
-- job of this class is to deconstruct arbitrary @item@s into a @(prio, val)@
-- pairs that can be handled by a minimum priority 'HeapT'.
--
-- Example: Consider you want to use @'HeapT' prio val@ as a @'MaxHeap' a@. You
-- would have to invert the order of @a@ (e. g. by introducing @newtype InvOrd a
-- = InvOrd a@ along with an apropriate 'Ord' instance for it) and then use a
-- @type 'MaxHeap' a = 'HeapT' (InvOrd a) ()@. You'd also have to translate
-- every @x@ to @(InvOrd x, ())@ before insertion and back after removal in
-- order to retrieve your original type @a@.
--
-- This functionality is provided by the 'HeapItem' class. In the above example,
-- you'd use a 'MaxHeap'. The according instance declaration is of course
-- already provided and looks like this (simplified):
--
-- @data 'MaxPolicy'
--
-- instance ('Ord' a) => 'HeapItem' 'MaxPolicy' a where
--     newtype 'Prio' 'MaxPolicy' a = MaxP a deriving ('Eq')
--     type    'Val'  'MaxPolicy' a = ()
--     'split' x           = (MaxP x, ())
--     'merge' (MaxP x, _) = x
--
-- instance ('Ord' a) => 'Ord' ('Prio' 'MaxPolicy' a) where
--     'compare' (MaxP x) (MaxP y) = 'compare' y x
-- @
--
-- 'MaxPolicy' is a phantom type describing which 'HeapItem' instance is actually
-- meant (e. g. we have to distinguish between 'MinHeap' and 'MaxHeap', which is
-- done via 'MinPolicy' and 'MaxPolicy', respectively) and @MaxP@ inverts the
-- ordering of @a@, so that the maximum will be on top of the 'HeapT'.
--
-- The conversion functions 'split' and 'merge' have to make sure that
--
-- (1) @forall p v. 'split' ('merge' (p, v)) == (p, v)@ ('merge' and 'split'
--     don't remove, add or alter anything)
--
-- (2) @forall p v f. 'fst' ('split' ('merge' (p, f v)) == 'fst' ('split'
--     ('merge' (p, v)))@ (modifying the associated value @v@ doesn't alter the
--      priority @p@)
class (Ord (Prio pol item)) => HeapItem pol item where
    -- | The part of @item@ that determines the order of elements on a 'HeapT'.
    data Prio pol item :: *
    -- | Everything not part of @'Prio' pol item@
    type Val  pol item :: *

    -- | Translate an @item@ into a priority-value pair.
    split :: item -> (Prio pol item, Val pol item)
    -- | Restore the @item@ from a priority-value pair.
    merge :: (Prio pol item, Val pol item) -> item
{-# RULES "split/merge" forall x. split (merge x) = x #-}

-- | Policy type for a 'MinHeap'.
data MinPolicy

instance (Ord a) => HeapItem MinPolicy a where
    newtype Prio MinPolicy a = MinP { unMinP :: a } deriving (Eq, Ord)
    type    Val  MinPolicy a = ()

    split x           = (MinP x, ())
    merge (MinP x, _) = x

instance (Read a) => Read (Prio MinPolicy a) where
    readPrec     = fmap MinP readPrec
    readListPrec = fmap (fmap MinP) readListPrec

instance (Show a) => Show (Prio MinPolicy a) where
    show        = show . unMinP
    showsPrec d = showsPrec d . unMinP
    showList    = showList . (fmap unMinP)

-- | Policy type for a 'MaxHeap'.
data MaxPolicy

instance (Ord a) => HeapItem MaxPolicy a where
    newtype Prio MaxPolicy a = MaxP { unMaxP :: a } deriving (Eq)
    type    Val  MaxPolicy a = ()

    split x           = (MaxP x, ())
    merge (MaxP x, _) = x

instance (Ord a) => Ord (Prio MaxPolicy a) where
    compare (MaxP x) (MaxP y) = compare y x

instance (Read a) => Read (Prio MaxPolicy a) where
    readPrec     = fmap MaxP readPrec
    readListPrec = fmap (fmap MaxP) readListPrec

instance (Show a) => Show (Prio MaxPolicy a) where
    show        = show . unMaxP
    showsPrec d = showsPrec d . unMaxP
    showList    = showList . (fmap unMaxP)

-- | Policy type for a @(prio, val)@ 'MinPrioHeap'.
data FstMinPolicy

instance (Ord prio) => HeapItem FstMinPolicy (prio, val) where
    newtype Prio FstMinPolicy (prio, val) = FMinP { unFMinP :: prio } deriving (Eq, Ord)
    type    Val  FstMinPolicy (prio, val) = val

    split (p,       v) = (FMinP p, v)
    merge (FMinP p, v) = (p,       v)

instance (Read prio) => Read (Prio FstMinPolicy (prio, val)) where
    readPrec     = fmap FMinP readPrec
    readListPrec = fmap (fmap FMinP) readListPrec

instance (Show prio) => Show (Prio FstMinPolicy (prio, val)) where
    show        = show . unFMinP
    showsPrec d = showsPrec d . unFMinP
    showList    = showList . (fmap unFMinP)

-- | Policy type for a @(prio, val)@ 'MaxPrioHeap'.
data FstMaxPolicy

instance (Ord prio) => HeapItem FstMaxPolicy (prio, val) where
    newtype Prio FstMaxPolicy (prio, val) = FMaxP { unFMaxP :: prio } deriving (Eq)
    type    Val  FstMaxPolicy (prio, val) = val

    split (p,       v) = (FMaxP p, v)
    merge (FMaxP p, v) = (p,       v)

instance (Ord prio) => Ord (Prio FstMaxPolicy (prio, val)) where
    compare (FMaxP x) (FMaxP y) = compare y x

instance (Read prio) => Read (Prio FstMaxPolicy (prio, val)) where
    readPrec     = fmap FMaxP readPrec
    readListPrec = fmap (fmap FMaxP) readListPrec

instance (Show prio) => Show (Prio FstMaxPolicy (prio, val)) where
    show        = show . unFMaxP
    showsPrec d = showsPrec d . unFMaxP
    showList    = showList . (fmap unFMaxP)

-- | 'split' a function on @item@s to one on priority-value pairs.
splitF :: (HeapItem pol item) => (item -> a) -> (Prio pol item, Val pol item) -> a
splitF f pv = f (merge pv)
{-# INLINE[1] splitF #-}
{-# RULES "splitF/split" forall f x. splitF f (split x) = f x #-}
