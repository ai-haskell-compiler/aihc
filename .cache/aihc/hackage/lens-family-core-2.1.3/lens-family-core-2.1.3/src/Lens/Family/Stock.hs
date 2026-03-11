-- | This module contains lenses, prisms, grids, grates and traversals for common structures in Haskell.
-- It also contains the combinators for various kinds of optics.
--
-- A Function name with @'@ is a grate variant of a grid, and a function name with @_@ is a traversal variants of a grid or prism.
-- For example, 'both'' is the grate variant of 'both' while 'both_' is the traversal variant.
module Lens.Family.Stock (
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
  , alongside
  , backwards
  , beside, beside', beside_
  , choosing
  , from
-- * Types
  , AlongsideLeft, AlongsideRight
  , FromF, FromG
-- * Re-exports
  , AdapterLike, AdapterLike'
  , LensLike, LensLike'
  , GrateLike, GrateLike'
  , Identical, Backwards
  , FiniteBits
-- * Deprecated names
  , lft, rgt
  , some, none
  , lft_, rgt_
  , some_, none_
  ) where

import Control.Arrow (first, second)
import Control.Applicative.Backwards (Backwards(..))
import Control.Applicative (liftA2)
import Data.Bits (FiniteBits, (.|.), bit, finiteBitSize, testBit, zeroBits)
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.Strict as IntMap'
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map'
import Data.Proxy (asProxyTypeOf)
import qualified Data.Set as Set
import Lens.Family
import Lens.Family.Phantom
import Lens.Family.Unchecked

choosing :: Functor f => LensLike f s0 t0 a b -> LensLike f s1 t1 a b -> LensLike f (Either s0 s1) (Either t0 t1) a b
-- ^ @
-- choosing :: Lens s0 t0 a b -> Lens s1 t1 a b -> Lens (Either s0 s1) (Either t0 t1) a b
-- @
--
-- @
-- choosing :: Traversal s0 t0 a b -> Traversal s1 t1 a b -> Traversal (Either s0 s1) (Either t0 t1) a b
-- @
--
-- @
-- choosing :: Getter s0 t0 a b -> Getter s1 t1 a b -> Getter (Either s0 s1) (Either t0 t1) a b
-- @
--
-- @
-- choosing :: Fold s0 t0 a b -> Fold s1 t1 a b -> Fold (Either s0 s1) (Either t0 t1) a b
-- @
--
-- @
-- choosing :: Setter s0 t0 a b -> Setter s1 t1 a b -> Setter (Either s0 s1) (Either t0 t1) a b
-- @
--
-- Given two lens\/traversal\/getter\/fold\/setter families with the same substructure, make a new lens\/traversal\/getter\/fold\/setter on 'Either'.
choosing la _  f (Left a)  = Left  <$> la f a
choosing _  lb f (Right b) = Right <$> lb f b

_1 :: Functor f => LensLike f (a, r) (b, r) a b
-- ^ @
-- _1 :: Lens (a, r) (b, r) a b
-- @
--
-- Lens on the first element of a pair.
_1 f (a, r) = (\b -> (b, r)) <$> f a

_2 :: Functor f => LensLike f (r, a) (r, b) a b
-- ^ @
-- _2 :: Lens (r, a) (r, b) a b
-- @
--
-- Lens on the second element of a pair.
_2 f (r, a) = (\b -> (r, b)) <$> f a

chosen :: Functor f => LensLike f (Either a a) (Either b b) a b
-- ^ @
-- chosen :: Lens (Either a a) (Either b b) a b
-- @
--
-- Lens on the Left or Right element of an ('Either' a a).
chosen = choosing id id

ix :: (Eq k, Functor f) => k -> LensLike' f (k -> v) v
-- ^ @
-- ix :: Eq k => k -> Lens' (k -> v) v
-- @
--
-- Lens on a given point of a function.
ix k f g = (\v' x -> if (k == x) then v' else g x) <$> f (g k)

at :: (Ord k, Functor f) => k -> LensLike' f (Map.Map k v) (Maybe v)
-- ^ @
-- at :: Ord k => k -> Lens' (Map.Map k v) (Maybe v)
-- @
--
-- Lens on a given point of a 'Map.Map'.
at = flip Map.alterF

intAt :: Functor f => Int -> LensLike' f (IntMap.IntMap v) (Maybe v)
-- ^ @
-- intAt :: Int -> Lens (IntMap.IntMap v) (Maybe v)
-- @
--
-- Lens on a given point of a 'IntMap.IntMap'.
intAt = flip IntMap.alterF

at' :: (Ord k, Functor f) => k -> LensLike' f (Map.Map k v) (Maybe v)
-- ^ @
-- at :: Ord k => k -> Lens' (Map.Map k v) (Maybe v)
-- @
--
-- Lens providing strict access to a given point of a 'Map.Map'.
at' = flip Map'.alterF

intAt' :: Functor f => Int -> LensLike' f (IntMap.IntMap v) (Maybe v)
-- ^ @
-- intAt :: Int -> Lens (IntMap.IntMap v) (Maybe v)
-- @
--
-- Lens providing strict access to a given point of a 'IntMap.IntMap'.
intAt' = flip IntMap'.alterF

contains :: (Ord k, Functor f) => k -> LensLike' f (Set.Set k) Bool
-- ^ @
-- contains :: Ord => k -> Lens' (Set.Set k) Bool
-- @
--
-- Lens on a given point of a 'Set.Set'.
contains k = lens (Set.member k) (\m nv -> if nv then Set.insert k m else Set.delete k m)

intContains :: Functor f => Int -> LensLike' f IntSet.IntSet Bool
-- ^ @
-- intContains :: Int -> Lens' IntSet.IntSet Bool
-- @
--
-- Lens on a given point of a 'IntSet.IntSet'.
intContains k = lens (IntSet.member k) (\m nv -> if nv then IntSet.insert k m else IntSet.delete k m)

cod :: Functor g => GrateLike g (r -> a) (r -> b) a b
-- ^ @
-- cod :: Grate (r -> a) (r -> b) a b
-- @
--
-- A grate accessing the codomain of a function.
cod f h r = f $ ($ r) <$> h

left :: (Applicative f, Traversable g) => AdapterLike f g (Either a r) (Either b r) a b
-- ^ @
-- left :: Prism (Either a r) (Either b r) a b
-- @
--
-- A prism on the 'Left' element of an 'Either'.
left f = either (pure . Right) (fmap Left . f) . traverse switch
 where
  switch = either Right Left

left_ :: Applicative f => LensLike f (Either a r) (Either b r) a b
-- ^ @
-- left_ :: Traversal (Either a r) (Either b r) a b
-- @
--
-- Traversal on the 'Left' element of an 'Either'.
--
-- @
-- left_ = under left
-- @
left_ = under left

right :: (Applicative f, Traversable g) => AdapterLike f g (Either r a) (Either r b) a b
-- ^ @
-- right :: Prism (Either r a) (Either r b) a b
-- @
--
-- A prism on the 'Right' element of an 'Either'.
right f = either (pure . Left) (fmap Right . f) . sequenceA

right_ :: Applicative f => LensLike f (Either r a) (Either r b) a b
-- ^ @
-- right_ :: Traversal (Either r a) (Either r b) a b
-- @
--
-- Traversal on the 'Right' element of an 'Either'.
--
-- @
-- right_ = under right
-- @
right_ = under right

just :: (Applicative f, Traversable g) => AdapterLike f g (Maybe a) (Maybe b) a b
-- ^ @
-- just :: Prism (Maybe a) (Maybe b) a b
-- @
--
-- A prism on the 'Just' element of a 'Maybe'.
just f = maybe (pure Nothing) (fmap Just . f) . sequenceA

just_ :: Applicative f => LensLike f (Maybe a) (Maybe b) a b
-- ^ @
-- just_ :: Traversal (Maybe a) (Maybe b) a b
-- @
--
-- Traversal on the 'Just' element of a 'Maybe'.
just_ = under just

nothing :: (Applicative f, Traversable g) => AdapterLike' f g (Maybe a) ()
-- ^ @
-- nothing :: Prism' (Maybe a) ()
-- @
--
-- A prism on the 'Nothing' element of a 'Maybe'.
nothing = prism (maybe (Right ()) (Left . Just)) (const Nothing)

nothing_ :: Applicative f => LensLike' f (Maybe a) ()
-- ^ @
-- nothing_ :: Traversal' (Maybe a) ()
-- @
--
-- Traversal on the 'Nothing' element of a 'Maybe'.
nothing_ = under nothing

both :: (Applicative f, Functor g) => AdapterLike f g (a,a) (b,b) a b
-- ^ @
-- both :: Grid (a,a) (b,b) a b
-- @
--
-- A grid on both elements of a pair @(a,a)@.
both = beside id id

both' :: Functor g => GrateLike g (a,a) (b,b) a b
-- ^ @
-- both' :: Grate (a,a) (b,b) a b
-- @
--
-- A grate on both elements of a pair @(a,a)@.
--
-- @
-- both' = over both
-- @
both' = beside' id id

both_ :: Applicative f => LensLike f (a,a) (b,b) a b
-- ^ @
-- both_ :: Traversal (a,a) (b,b) a b
-- @
--
-- Traversals on both elements of a pair @(a,a)@.
--
-- @
-- both_ = under both
-- @
both_ = beside_ id id

lend :: (FiniteBits b, Applicative f, Functor g) => AdapterLike' f g b Bool
-- ^ @
-- lend :: FiniteBits b => Grid' b Bool
-- @
--
-- A grid from the least significant bit to the most significant bit of a 'FiniteBits' type.
--
-- Little endian order.
lend f s = foldr (liftA2 (.|.)) (pure zeroBits) [mask i <$> f (flip testBit i <$> s) | i <- [0..finiteBitSize b-1]]
 where
  mask i True = bit i
  mask _ False = zeroBits
  b = b `asProxyTypeOf` s

lend' :: (FiniteBits b, Functor g) => GrateLike' g b Bool
-- ^ @
-- lend' :: FiniteBits b => Grate' b Bool
-- @
--
-- A grate from the least significant bit to the most significant bit of a 'FiniteBits' type.
--
-- Little endian order.
--
-- @
-- lend' = over lend
-- @
lend' = over lend

lend_ :: (FiniteBits b, Applicative f) => LensLike' f b Bool
-- ^ @
-- lend_ :: FiniteBits b => Traversal' b Bool
-- @
--
-- A traversal from the least significant bit to the most significant bit of a 'FiniteBits' type.
--
-- Little endian order.
--
-- @
-- lend_ = under lend
-- @
lend_ = under lend

bend :: (FiniteBits b, Applicative f, Functor g) => AdapterLike' f g b Bool
-- ^ @
-- bend :: FiniteBits b => Grid' b Bool
-- @
--
-- A grid from the most significant bit to the least significant bit of a 'FiniteBits' type.
--
-- Big endian order.
bend = backwards lend

bend' :: (FiniteBits b, Functor g) => GrateLike' g b Bool
-- ^ @
-- bend' :: FiniteBits b => Grate' b Bool
-- @
--
-- A grate from the most significant bit to the least significant bit of a 'FiniteBits' type.
--
-- Big endian order.
--
-- @
-- bend' = over bend
-- @
bend' = over bend

bend_ :: (FiniteBits b, Applicative f) => LensLike' f b Bool
-- ^ @
-- bend_ :: FiniteBits b => Traversal' b Bool
-- @
--
-- A traversal from the most significant bit to the least significant bit of a 'FiniteBits' type.
--
-- Big endian order.
--
-- @
-- bend_ = under bend
-- @
bend_ = under bend

beside :: (Applicative f, Functor g) => AdapterLike f g s0 t0 a b -> AdapterLike f g s1 t1 a b -> AdapterLike f g (s0, s1) (t0, t1) a b
-- ^ @
-- beside :: Grid s1 t1 a b -> Grid s2 t2 a b -> Grid (s1, s2) (t1, t2) a b
-- @
--
-- Given two grids referencing a type 'c', create a grid on the pair referencing 'c'.
beside la lb f s = (,) <$> la f (fst <$> s) <*> lb f (snd <$> s)

beside' :: Functor g => GrateLike g s0 t0 a b -> GrateLike g s1 t1 a b -> GrateLike g (s0, s1) (t0, t1) a b
-- ^ @
-- beside' :: Grate s0 t0 a b -> Grate s1 t1 a b -> Grate (s0, s1) (t0, t1) a b
-- @
--
-- @
-- beside' :: Resetter s0 t0 a b -> Resetter s1 t1 a b -> Resetter (s0, s1) (t0, t1) a b
-- @
--
-- Given two grates\/resetters referencing a type 'c', create a grate\/resetter on the pair referencing 'c'.
beside' la lb = over $ beside (setting la) (setting lb)

beside_ :: Applicative f => LensLike f s0 t0 a b -> LensLike f s1 t1 a b -> LensLike f (s0, s1) (t0, t1) a b
-- ^ @
-- beside_ :: Traversal s0 t0 a b -> Traversal s1 t1 a b -> Traversal (s0, s1) (t0, t1) a b
-- @
--
-- @
-- beside_ :: Fold s0 t0 a b -> Fold s1 t1 a b -> Fold (s0, s1) (t0, t1) a b
-- @
--
-- @
-- beside_ :: Setter s0 t0 a b -> Setter s1 t1 a b -> Setter (s0, s1) (t0, t1) a b
-- @
--
-- Given two traversals\/folds\/setters referencing a type 'c', create a traversal\/fold\/setter on the pair referencing 'c'.
beside_ la lb = under $ beside (resetting la) (resetting lb)

ignored :: Applicative f => null -> s -> f s
-- ^ @
-- ignored :: Traversal s s a b
-- @
--
-- The empty traversal on any type.
ignored _ = pure

mapped :: (Identical f, Functor h) => LensLike f (h a) (h b) a b
-- ^ @
-- mapped :: Functor h => Setter (h a) (h b) a b
-- @
--
-- An SEC referencing the parameter of a functor.
mapped = setting fmap

backwards :: LensLike (Backwards f) s t a b -> LensLike f s t a b
-- ^ @
-- backwards :: Traversal s t a b -> Traversal s t a b
-- backwards :: Fold s t a b -> Fold s t a b
-- @
--
-- Given a traversal or fold, reverse the order that elements are traversed.
--
-- @
-- backwards :: Lens s t a b -> Lens s t a b
-- backwards :: Getter s t a b -> Getter s t a b
-- backwards :: Setter s t a b -> Setter s t a b
-- @
--
-- No effect on lenses, getters or setters.
backwards l f = forwards . l (Backwards . f)

{- Alongside -}

newtype AlongsideLeft f b a = AlongsideLeft (f (a, b))

instance Functor f => Functor (AlongsideLeft f a) where
  fmap f (AlongsideLeft x) = AlongsideLeft (fmap (first f) x)

instance Phantom f => Phantom (AlongsideLeft f a) where
  coerce (AlongsideLeft x) = AlongsideLeft (coerce x)

newtype AlongsideRight f a b = AlongsideRight (f (a, b))

instance Functor f => Functor (AlongsideRight f a) where
  fmap f (AlongsideRight x) = AlongsideRight (fmap (second f) x)

instance Phantom f => Phantom (AlongsideRight f a) where
  coerce (AlongsideRight x) = AlongsideRight (coerce x)

alongside :: Functor f => LensLike (AlongsideLeft f b1) s0 t0 a0 b0
                       -> LensLike (AlongsideRight f t0) s1 t1 a1 b1
                       -> LensLike f (s0, s1) (t0, t1) (a0, a1) (b0, b1)
-- ^ @
-- alongside :: Lens s0 t0 a0 b0 -> Lens s1 t1 a1 b1 -> Lens (s0, s1) (t0, t1) (a0, a1) (b0, b1)
-- @
--
-- @
-- alongside :: Getter s0 t0 a0 b0 -> Getter s1 t1 a1 b1 -> Getter (s0, s1) (t0, t1) (a0, a1) (b0, b1)
-- @
--
-- Given two lens\/getter families, make a new lens\/getter on their product.
alongside l0 l1 f (s0, s1) = ft0t1
 where
  AlongsideRight ft0t1 = l1 f1 s1
  f1 a1 = AlongsideRight ft0a1
   where
    AlongsideLeft ft0a1 = l0 f0 s0
    f0 a0 = AlongsideLeft (f (a0, a1))

{- From -}

newtype FromF i j g x = FromF ((g x -> j) -> i)

instance Functor g => Functor (FromF i j g) where
  fmap f (FromF h) = FromF $ \k -> h (k . fmap f)

instance Phantom g => Phantom (FromF i j g) where
  coerce (FromF h) = FromF $ \k -> h (k . coerce)

newtype FromG e f x = FromG (e -> f x)

instance Functor f => Functor (FromG e f) where
  fmap f (FromG h) = FromG $ fmap f . h

instance Phantom g => Phantom (FromG e g) where
  coerce (FromG h) = FromG $ coerce . h

from :: (Functor f, Functor g)
     => AdapterLike (FromF (g s -> f t) (f b) g) (FromG (f b) f) b a t s
     -> AdapterLike f g s t a b
-- ^ @
-- from :: Adapter b a t s -> Adapter s t a b
-- @
--
-- Reverses the direction of an adapter.
--
-- @
-- from :: Getter b a t s -> Reviewer s t a b
-- from :: Reviewer b a t s -> Getter s t a b
-- @
--
-- Changes a Getter into a Reviewer and vice versa.
from l = l'
 where
  FromF l' = l (\(FromG h1) -> FromF $ (.) h1) (FromG id)

{-# DEPRECATED lft "Renamed as 'left'." #-}
lft :: (Applicative f, Traversable g) => AdapterLike f g (Either a r) (Either b r) a b
lft = left

{-# DEPRECATED lft_ "Renamed as 'left_'." #-}
lft_ :: Applicative f => LensLike f (Either a r) (Either b r) a b
lft_ = left_

{-# DEPRECATED rgt "Renamed as 'right'." #-}
rgt :: (Applicative f, Traversable g) => AdapterLike f g (Either r a) (Either r b) a b
rgt = right

{-# DEPRECATED rgt_ "Renamed as 'right_'." #-}
rgt_ :: Applicative f => LensLike f (Either r a) (Either r b) a b
rgt_ = right_

{-# DEPRECATED some "Renamed as 'just'." #-}
some :: (Applicative f, Traversable g) => AdapterLike f g (Maybe a) (Maybe b) a b
some = just

{-# DEPRECATED some_ "Renamed as 'just_'." #-}
some_ :: Applicative f => LensLike f (Maybe a) (Maybe b) a b
some_ = just_

{-# DEPRECATED none "Renamed as 'nothing'." #-}
none :: (Applicative f, Traversable g) => AdapterLike' f g (Maybe a) ()
none = nothing

{-# DEPRECATED none_ "Renamed as 'nothing_'." #-}
none_ :: Applicative f => LensLike' f (Maybe a) ()
none_ = nothing_
