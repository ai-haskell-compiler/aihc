{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
#endif
{-# OPTIONS_HADDOCK not-home #-}

-- | This is an internal module. You probably don't need to import this. Import
-- "Data.CharSet" instead.
--
-- = WARNING
--
-- Definitions in this module allow violating invariants that would otherwise be
-- guaranteed by non-internal modules. Use at your own risk!
--
module Regex.Internal.CharSet
  ( CharSet(..)
  , empty
  , singleton
  , fromRange
  , fromList
  , fromRanges
  , insert
  , insertRange
  , delete
  , deleteRange
  , map
  , not
  , union
  , difference
  , intersection
  , member
  , notMember
  , elems
  , ranges
  , valid
  ) where

import Prelude hiding (not, map)
import qualified Prelude
import Data.Char (ord)
import Data.String (IsString(..))
import qualified Data.Foldable as F
import qualified Data.IntMap.Strict as IM
import Data.Semigroup (Semigroup(..), stimesIdempotentMonoid)
#ifdef __GLASGOW_HASKELL__
import GHC.Exts (Int(..), Char(..), chr#)
#endif

-- TODO: Evaluate other set libraries.
-- Possible candidates: charset, rangeset

-- | A set of @Char@s.
--
-- The members are stored as contiguous ranges of @Char@s. This is efficient
-- when the members form contiguous ranges since many @Char@s can be represented
-- with just one range.
newtype CharSet = CharSet { unCharSet :: IM.IntMap Char } deriving Eq

instance Show CharSet where
  showsPrec p cs = showParen (p > 10) $
    showString "fromRanges " . shows (ranges cs)

-- | @fromString@ = 'fromList'
instance IsString CharSet where
  fromString = fromList

-- | @(<>)@ = 'union'
instance Semigroup CharSet where
  (<>) = union
  sconcat = F.foldl' union empty
  {-# INLINE sconcat #-}
  stimes = stimesIdempotentMonoid

-- | @mempty@ = 'empty'
instance Monoid CharSet where
  mempty = empty
  mconcat = F.foldl' union empty
  {-# INLINE mconcat #-}

-- | The empty set.
empty :: CharSet
empty = CharSet IM.empty

-- | \(O(1)\). A set of one @Char@.
singleton :: Char -> CharSet
singleton c = CharSet (IM.singleton (ord c) c)

-- | \(O(1)\). A @Char@ range (inclusive).
fromRange :: (Char, Char) -> CharSet
fromRange (cl,ch) | cl > ch = empty
fromRange (cl,ch) = CharSet (IM.singleton (ord cl) ch)

-- | \(O(s \min(s,C))\). Create a set from @Char@s in a list.
fromList :: [Char] -> CharSet
fromList = F.foldl' (flip insert) empty
{-# INLINE fromList #-}

-- | \(O(n \min(n,C))\). Create a set from the given @Char@ ranges (inclusive).
fromRanges :: [(Char, Char)] -> CharSet
fromRanges = F.foldl' (flip insertRange) empty
{-# INLINE fromRanges #-}

-- | \(O(\min(n,C))\). Insert a @Char@ into a set.
insert :: Char -> CharSet -> CharSet
insert c = insertRange (c,c)

-- | \(O(\min(n,C))\). Insert all @Char@s in a range (inclusive) into a set.
insertRange :: (Char, Char) -> CharSet -> CharSet
insertRange (cl,ch) cs | cl > ch = cs
insertRange (cl,ch) cs = l `join` fromRange (cl,ch) `join` r
  where
    (l,mr) = split (ord cl) cs
    (_,r) = split (ord ch + 1) mr

-- | \(O(\min(n,C))\). Delete a @Char@ from a set.
delete :: Char -> CharSet -> CharSet
delete c = deleteRange (c,c)

-- | \(O(\min(n,C))\). Delete a @Char@ range (inclusive) from a set.
deleteRange :: (Char, Char) -> CharSet -> CharSet
deleteRange (cl,ch) cs | cl > ch = cs
deleteRange (cl,ch) cs = l `join` r
  where
    (l,mr) = split (ord cl) cs
    (_,r) = split (ord ch + 1) mr

-- | \(O(s \min(s,C))\). Map a function over all @Char@s in a set.
map :: (Char -> Char) -> CharSet -> CharSet
map f = fromList . fmap f . elems

-- | \(O(n)\). The complement of a set.
not :: CharSet -> CharSet
not = CharSet . IM.fromDistinctAscList . complementRanges . ranges
-- TODO: Would be nice to have O(1) complement

-- | \(O(m \min(n+m,C))\). The union of two sets.
--
-- Prefer strict left-associative unions, since this is a strict structure and
-- the runtime is linear in the size of the second argument.
union :: CharSet -> CharSet -> CharSet
union = foldlRanges' (\cs cl ch -> insertRange (cl,ch) cs)

-- | \(O(m \min(n+m,C))\). The difference of two sets.
difference :: CharSet -> CharSet -> CharSet
difference = foldlRanges' (\cs cl ch -> deleteRange (cl,ch) cs)

-- | \(O(n + m \min(n+m,C))\). The intersection of two sets.
intersection :: CharSet -> CharSet -> CharSet
intersection lcs rcs = not (not lcs `union` not rcs)

-- | \(O(\min(n,C))\). Whether a @Char@ is in a set.
member :: Char -> CharSet -> Bool
member c cs = case IM.lookupLE (ord c) (unCharSet cs) of
  Nothing -> False
  Just (_,ch) -> c <= ch

-- | \(O(\min(n,C))\). Whether a @Char@ is not in a set.
notMember :: Char -> CharSet -> Bool
notMember c = Prelude.not . member c

-- | \(O(s)\). The @Char@s in a set.
elems :: CharSet -> [Char]
elems cs = ranges cs >>= \(cl,ch) -> [cl..ch]
{-# INLINE elems #-}

-- | \(O(n)\). The contiguous ranges of @Chars@ in a set.
ranges :: CharSet -> [(Char, Char)]
ranges cs = [(unsafeChr cl, ch) | (cl,ch) <- IM.assocs (unCharSet cs)]
{-# INLINE ranges #-}

--------------------
-- Internal/Unsafe
--------------------

-- | \(O(\min(n,W))\). Split a set into one containing @Char@s smaller than
-- the given char and one greater than or equal to the given char.
split :: Int -> CharSet -> (CharSet, CharSet)
split !c cs = case IM.splitLookup c (unCharSet cs) of
  (l, Just ch, r) -> (CharSet l, CharSet $ IM.insert c ch r)
  (l, Nothing, r) -> case IM.maxViewWithKey l of
    Just ((lgl,lgh),l1)
      | ord lgh >= c -> ( CharSet $ IM.insert lgl (unsafeChr (c - 1)) l1
                        , CharSet $ IM.insert c lgh r )
    _ -> (CharSet l, CharSet r)
-- The bang on c helps because splitLookup was unfortunately not strict in
-- the lookup key until https://github.com/haskell/containers/pull/982.

-- | \(O(\min(n+m,W))\). Join two sets. Every @Char@ in the left set must be
-- smaller than every @Char@ in the right set.
-- /This precondition is not checked./
join :: CharSet -> CharSet -> CharSet
join lcs rcs = case ( IM.maxViewWithKey (unCharSet lcs)
                    , IM.minViewWithKey (unCharSet rcs) ) of
  (Nothing, Nothing) -> empty
  (Nothing, _) -> rcs
  (_, Nothing) -> lcs
  (Just ((lgl,lgh),l1), Just ((rgl,rgh),r1))
    | ord lgh == rgl - 1 -> CharSet $ IM.union l1 (IM.insert lgl rgh r1)
    | otherwise -> CharSet $ IM.union (unCharSet lcs) (unCharSet rcs)
-- Without the Nothing cases above there is a call to union even for those
-- cases. These would ideally be removed after inlining union's wrapper.
-- TODO: maxViewWithKey constructs the map without max but we may end up not
-- needing it. Check if doing lookupMax first is better even if we have to go
-- down the tree twice.

-- | \(O(n)\). Fold over the ranges in a set.
foldlRanges' :: (b -> Char -> Char -> b) -> b -> CharSet -> b
foldlRanges' = \f z cs ->
  IM.foldlWithKey' (\b cl ch -> f b (unsafeChr cl) ch) z (unCharSet cs)
{-# INLINE foldlRanges' #-}

-- | \(O(n)\). The complement of non-overlapping sorted ranges of Chars.
complementRanges :: [(Char, Char)] -> [(Int, Char)]
complementRanges = go
  where
    go [] = [(ord minBound, maxBound)]
    go ((l,h):xs)
      | l == minBound = go1 h xs
      | otherwise     = (ord minBound, unsafePred l) : go1 h xs

    go1 !ph []
      | ph == maxBound = []
      | otherwise      = [(ord ph + 1, maxBound)]
    go1 ph ((l,h):xs) = (ord ph + 1, unsafePred l) : go1 h xs

    unsafePred c = unsafeChr (ord c - 1)

unsafeChr :: Int -> Char
#ifdef __GLASGOW_HASKELL__
unsafeChr (I# i#) = C# (chr# i#)
#else
unsafeChr = toEnum
#endif

------------
-- Testing
------------

-- | Is the internal structure of the set valid?
valid :: CharSet -> Bool
valid cs = noneEmpty && noneAdjacent
  where
    (ls,hs) = unzip (fmap (fmap ord) (IM.assocs (unCharSet cs)))
    noneEmpty = and (zipWith (<=) ls hs)
    noneAdjacent = case ls of
      [] -> True
      _:ls' -> all (>1) (zipWith (-) ls' hs)
