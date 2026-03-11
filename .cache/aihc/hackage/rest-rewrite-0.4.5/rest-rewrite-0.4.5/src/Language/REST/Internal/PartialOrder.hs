{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.REST.Internal.PartialOrder (
      empty
    , insert
    , replaceUnsafe
    , insertUnsafe
    , gt
    , toList
    , isEmpty
    , elems
    , unionDisjointUnsafe
    , PartialOrder
    , toDescsList
    , descendents
    ) where

import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

import Language.REST.Types () -- Hashable (M.Map a b)
import Language.REST.Internal.Orphans ()
import Text.Printf

-- | Irreflexive (strict) partial orders
newtype PartialOrder a =
  -- | @PartialOrder m@ represents the relation
  --
  -- > (>) = { (a, b) | (a, bs)  <- toList m, b <- bs }
  --
  -- Transitivity implies that @m ! a == { b | a > b}@ if @a@ is in the map.
  --
  -- Asymmetry implies that @member a (m ! b)@ implies
  -- @not (member b (m ! a))@.
  --
  -- Irreflexivity means that @a@ cannot be in @m ! a@.
  --
  PartialOrder (M.Map a (S.Set a))
  deriving (Ord, Eq, Generic, Hashable)

instance (Show a) => Show (PartialOrder a) where
  show (PartialOrder m) = L.intercalate " ∧ " $ map go (M.toList m) where
    go (key, s) = case S.toList s of
      [x] -> printf "%s > %s" (show key) (show x)
      xs  -> printf "%s > { %s }" (show key) (L.intercalate ", " (map show xs))

empty :: PartialOrder a
empty = PartialOrder M.empty

isEmpty :: Eq a => PartialOrder a -> Bool
isEmpty p = p == empty

-- | @canInsert (>) a b@ iff @a /= b && not (a > b) && not (b > a)@
canInsert :: (Eq a, Ord a, Hashable a) => PartialOrder a -> a -> a -> Bool
canInsert o f g = f /= g && not (gt o f g) && not (gt o g f)

-- | @gt (>) a b == (a > b)@
gt :: (Eq a, Ord a, Hashable a) => PartialOrder a -> a -> a -> Bool
gt po t u = S.member u $ descendents t po

unionDisjointUnsafe :: Ord a => PartialOrder a -> PartialOrder a -> PartialOrder a
unionDisjointUnsafe (PartialOrder m) (PartialOrder m') = PartialOrder (M.union m m')

-- | ascendants a (>) = { b | b > a }
ascendants :: Ord k => k -> PartialOrder k -> S.Set k
ascendants k (PartialOrder m)  = M.keysSet $ M.filter (S.member k) m

-- | descendents a (>) = { b | a > b }
descendents :: Ord a => a -> PartialOrder a -> S.Set a
descendents k (PartialOrder m) = M.findWithDefault S.empty k m

-- | @insertUnsafe (>) a b@ is unsafe because it may not respect some
-- of its properties if @canInsert (>) a b@ doesn't hold.
{-# INLINE insertUnsafe #-}
insertUnsafe :: Ord a => PartialOrder a -> a -> a -> PartialOrder a
insertUnsafe o@(PartialOrder m) f g = result
  where
    result = PartialOrder $ M.insertWith S.union f decs $ M.mapWithKey go m

    go k old | S.member k ascs = S.union old decs
    go _ v          = v

    ascs = ascendants f o
    decs = S.insert g $ descendents g o

{-# INLINE insert #-}
insert :: (Eq a, Ord a, Hashable a) => PartialOrder a -> a -> a -> Maybe (PartialOrder a)
insert o f g = if canInsert o f g then Just (insertUnsafe o f g) else Nothing

toDescsList :: PartialOrder k -> [(k, S.Set k)]
toDescsList (PartialOrder m) = M.toList m

toList :: PartialOrder a -> [(a, a)]
toList (PartialOrder m) = do
  (k, vs) <- M.toList m
  v       <- S.toList vs
  return (k, v)

elems :: (Eq a, Ord a, Hashable a) => PartialOrder a -> S.Set a
elems (PartialOrder m) = S.union (M.keysSet m) (S.unions (M.elems m))

-- | @replaceUnsafe olds new (>)@ replaces every element in @olds@ with
-- @new@ in the partial order @(>)@.
--
-- More formally:
--
-- > replaceUnsafe olds new (>) =
-- >   { (a, b) | notElem a olds, notElem b olds }
-- >   U { (new, b) | o <- olds, o > b }
-- >   U { (a, new) | o <- olds, a > o }
--
-- This operation is unsafe because it only yields a partial order
-- if forall @o@ in @olds@:
--  * @o > b@ implies @not (b > new)@, and
--  * @a > o@ implies @not (new > a)@.
--
replaceUnsafe :: (Eq a, Ord a, Hashable a) => [a] -> a -> PartialOrder a -> PartialOrder a
replaceUnsafe froms to po@(PartialOrder m) = result where

  from' = S.fromList froms

  descs = S.unions (map (`descendents` po) froms)

  filtered = M.filterWithKey (\k _ -> k `notElem` froms) m
  m' =
    if S.null descs
    then filtered
    else M.insertWith S.union to descs filtered

  result = PartialOrder $ M.map go m'

  go s | hasFrom s = S.insert to $ S.union descs $ S.difference s from'
  go s  = s

  hasFrom set = any (`S.member` set) froms
