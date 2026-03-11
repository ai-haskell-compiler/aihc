{-# LANGUAGE Rank2Types #-}
-- | This module contains lenses, prisms, grids, grates and traversals for common structures in Haskell.
-- It also contains the combinators for various kinds of optics.
--
-- A Function name with @'@ is a grate variant of a grid, and a function name with @_@ is a traversal variants of a grid or prism.
-- For example, 'both'' is the grate variant of 'both' while 'both_' is the traversal variant.
module Lens.Family2.Stock (
-- * Stock Lenses
    _1, _2
  , chosen
  , ix
  , at, intAt
  , at', intAt'
  , contains, intContains
-- * Stock Prisms
  , left, right
  , just, nothing
-- * Stock Grids
  , both
  , bend, lend
-- * Stock Grates
  , cod
  , both'
  , bend', lend'
-- * Stock Traversals
  , both_
  , bend_, lend_
  , left_, right_
  , just_, nothing_
  , ignored
-- * Stock SECs
  , mapped
-- * Lens Combinators
  , Stock.alongside
  , Stock.backwards
  , Stock.beside, Stock.beside', Stock.beside_
  , Stock.choosing
  , Stock.from
-- * Types
  , Stock.AlongsideLeft, Stock.AlongsideRight
  , Stock.FromF, Stock.FromG
-- * Re-exports
  , Lens, Lens'
  , Grate, Grate'
  , Traversal, Traversal'
  , Setter
  , Stock.AdapterLike, Stock.AdapterLike'
  , Stock.LensLike, Stock.LensLike'
  , Stock.Identical, Stock.Backwards
  , Stock.FiniteBits
-- * Deprecated names
  , lft, rgt
  , some, none
  , lft_, rgt_
  , some_, none_
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Lens.Family.Stock as Stock
import Lens.Family2

-- | Lens on the first element of a pair.
_1 :: Lens (a, r) (b, r) a b
_1 = Stock._1

-- | Lens on the second element of a pair.
_2 :: Lens (r, a) (r, b) a b
_2 = Stock._2

-- | Lens on the Left or Right element of an ('Either' a a).
chosen :: Lens (Either a a) (Either b b) a b
chosen = Stock.chosen

-- | Lens on a given point of a function.
ix :: Eq k => k -> Lens' (k -> v) v
ix k = Stock.ix k

-- | Lens on a given point of a 'Map.Map'.
at :: Ord k => k -> Lens' (Map.Map k v) (Maybe v)
at k = Stock.at k

-- | Lens on a given point of a 'IntMap.IntMap'.
intAt :: Int -> Lens' (IntMap.IntMap v) (Maybe v)
intAt i = Stock.intAt i

-- | Lens providing strict access to a given point of a 'Map.Map'.
at' :: Ord k => k -> Lens' (Map.Map k v) (Maybe v)
at' k = Stock.at' k

-- | Lens providing strict access to a given point of a 'IntMap.IntMap'.
intAt' :: Int -> Lens' (IntMap.IntMap v) (Maybe v)
intAt' i = Stock.intAt' i

-- | Lens on a given point of a 'Set.Set'.
contains :: Ord k => k -> Lens' (Set.Set k) Bool
contains k = Stock.contains k

-- | Lens on a given point of a 'IntSet.IntSet'.
intContains :: Int -> Lens' IntSet.IntSet Bool
intContains i = Stock.intContains i

-- | A grate accessing the codomain of a function.
cod :: Grate (r -> a) (r -> b) a b
cod = Stock.cod

-- | A prism on the 'Left' element of an 'Either'.
left :: Prism (Either a r) (Either b r) a b
left = Stock.left

-- | Traversal on the 'Left' element of an 'Either'.
left_ :: Traversal (Either a r) (Either b r) a b
left_ = Stock.left_

-- | A prism on the 'Right' element of an 'Either'.
right :: Prism (Either r a) (Either r b) a b
right = Stock.right

-- | Traversal on the 'Right' element of an 'Either'.
right_ :: Traversal (Either r a) (Either r b) a b
right_ = Stock.right_

-- | A prism on the 'Just' element of a 'Maybe'.
just :: Prism (Maybe a) (Maybe b) a b
just = Stock.just

-- | Traversal on the 'Just' element of a 'Maybe'.
just_ :: Traversal (Maybe a) (Maybe b) a b
just_ = Stock.just_

-- | A prism on the 'Nothing' element of a 'Maybe'.
nothing :: Prism' (Maybe a) ()
nothing = Stock.nothing

-- | Traversal on the 'Nothing' element of a 'Maybe'.
nothing_ :: Traversal' (Maybe a) ()
nothing_ = Stock.nothing_

-- | A grid on both elements of a pair @(a,a)@.
both :: Grid (a,a) (b,b) a b
both = Stock.both

-- | A grate on both elements of a pair @(a,a)@.
both' :: Grate (a,a) (b,b) a b
both' = Stock.both'

-- | Traversals on both elements of a pair @(a,a)@.
both_ :: Traversal (a,a) (b,b) a b
both_ = Stock.both_

-- | A grid from the least significant bit to the most significant bit of a 'FiniteBits' type.
--
-- Little endian order.
lend :: Stock.FiniteBits b => Grid' b Bool
lend = Stock.lend

-- | A grate from the least significant bit to the most significant bit of a 'FiniteBits' type.
--
-- Little endian order.
lend' :: Stock.FiniteBits b => Grate' b Bool
lend' = Stock.lend'

-- | A traversal from the least significant bit to the most significant bit of a 'FiniteBits' type.
--
-- Little endian order.
lend_ :: Stock.FiniteBits b => Traversal' b Bool
lend_ = Stock.lend_

-- | A grid from the most significant bit to the least significant bit of a 'FiniteBits' type.
--
-- Big endian order.
bend :: Stock.FiniteBits b => Grid' b Bool
bend = Stock.bend

-- | A grate from the most significant bit to the least significant bit of a 'FiniteBits' type.
--
-- Big endian order.
bend' :: Stock.FiniteBits b => Grate' b Bool
bend' = Stock.bend'

-- | A traversal from the most significant bit to the least significant bit of a 'FiniteBits' type.
--
-- Big endian order.
bend_ :: Stock.FiniteBits b => Traversal' b Bool
bend_ = Stock.bend_

-- | The empty traveral on any type.
ignored :: Traversal a a b b'
ignored = Stock.ignored

-- | An SEC referencing the parameter of a functor.
mapped :: Functor f => Setter (f a) (f a') a a'
mapped = Stock.mapped

{-# DEPRECATED lft "Renamed as 'left'." #-}
lft :: Prism (Either a r) (Either b r) a b
lft = left

{-# DEPRECATED lft_ "Renamed as 'left_'." #-}
lft_ :: Traversal (Either a r) (Either b r) a b
lft_ = left_

{-# DEPRECATED rgt "Renamed as 'right'." #-}
rgt :: Prism (Either r a) (Either r b) a b
rgt = right

{-# DEPRECATED rgt_ "Renamed as 'right_'." #-}
rgt_ :: Traversal (Either r a) (Either r b) a b
rgt_ = right_

{-# DEPRECATED some "Renamed as 'just'." #-}
some :: Prism (Maybe a) (Maybe b) a b
some = just

{-# DEPRECATED some_ "Renamed as 'just_'." #-}
some_ :: Traversal (Maybe a) (Maybe b) a b
some_ = just_

{-# DEPRECATED none "Renamed as 'nothing'." #-}
none :: Prism' (Maybe a) ()
none = nothing

{-# DEPRECATED none_ "Renamed as 'nothing_'." #-}
none_ :: Traversal' (Maybe a) ()
none_ = nothing_
