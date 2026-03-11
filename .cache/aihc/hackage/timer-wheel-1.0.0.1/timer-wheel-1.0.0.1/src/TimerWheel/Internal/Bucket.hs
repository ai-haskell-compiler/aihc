{-# LANGUAGE CPP #-}

-- Code from this implementation was cribbed from `psqueues`, whose license is copied below.
--
-- The Glasgow Haskell Compiler License
--
-- Copyright 2004, The University Court of the University of Glasgow.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- - Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- - Redistributions in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.
--
-- - Neither name of the University nor the names of its contributors may be
-- used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
-- GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
-- INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
-- FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
-- DAMAGE.

module TimerWheel.Internal.Bucket
  ( Bucket,
    empty,

    -- * Queries
    isEmpty,
    partition,

    -- * Modifications
    insert,
    Pop (..),
    pop,
    deleteExpectingHit,

    -- * Strict pair
    Pair (..),
  )
where

import Data.Bits
import TimerWheel.Internal.Prelude
import TimerWheel.Internal.Timestamp (Timestamp)

#include "MachDeps.h"

data Bucket a
  = -- Invariants on `Bin k p v m l r`:
    --   1. `l` and `r` can't both be Nil
    --   2. `p` is <= all `p` in `l` and `r`
    --   3. `k` is not an element of `l` nor `r`
    --   4. `m` has one 1-bit, which is (usually) the highest bit position at which any two keys in `k`+`l`+`r` differ
    --      (henceforth referred to as the diffbit). It can get out-of-date by deletions and pops. Thus, `m` represents
    --      a left-bound on the diffbit, that is, the true diffbit can be no left-er than `m`.
    --   5. No key in `l` has the `m` bit set
    --   6. All keys in `r` have the `m` bit set
    Bin {-# UNPACK #-} !AlarmId {-# UNPACK #-} !Timestamp !a {-# UNPACK #-} !Mask !(Bucket a) !(Bucket a)
  | Tip {-# UNPACK #-} !AlarmId {-# UNPACK #-} !Timestamp !a
  | Nil

type Mask = Word64

type AlarmId = Int

-- | An empty bucket.
empty :: Bucket a
empty =
  Nil

isEmpty :: Bucket a -> Bool
isEmpty = \case
  Nil -> True
  _ -> False

-- | Partition a bucket by timestamp (less-than-or-equal-to, greater-than).
partition :: forall a. Timestamp -> Bucket a -> Pair (Bucket a) (Bucket a)
partition q =
  go empty
  where
    go :: Bucket a -> Bucket a -> Pair (Bucket a) (Bucket a)
    go acc t =
      case t of
        Nil -> Pair acc t
        Tip i p x
          | p > q -> Pair acc t
          | otherwise -> Pair (insert i p x acc) Nil
        Bin i p x m l r
          | p > q -> Pair acc t
          | otherwise ->
              case go acc l of
                Pair acc1 l1 ->
                  case go acc1 r of
                    Pair acc2 r1 -> Pair (insert i p x acc2) (merge m l1 r1)

-- | Insert a new timer into a bucket.
--
-- If a timer with the given id is already in the bucket, behavior is undefined.
insert :: forall a. AlarmId -> Timestamp -> a -> Bucket a -> Bucket a
insert i p x bucket =
  case bucket of
    Nil -> Tip i p x
    Tip j q y
      | betteri -> linki j bucket Nil
      | otherwise -> linkj i (Tip i p x) Nil
      where
        betteri = (p, i) < (q, j)
        linkj = link j q y
    Bin j q y m l r
      | betteri ->
          if
            | outsider -> linki j bucket Nil
            | goleft j m -> bini (insertj l) r
            | otherwise -> bini l (insertj r)
      | outsider -> linkj i (Tip i p x) (merge m l r)
      | goleft i m -> binj (inserti l) r
      | otherwise -> binj l (inserti r)
      where
        outsider = prefixNotEqual m i j
        betteri = (p, i) < (q, j)
        bini = Bin i p x m
        binj = Bin j q y m
        inserti = insert i p x
        insertj = insert j q y
        linkj = link j q y
  where
    linki = link i p x

data Pop a
  = PopAlgo {-# UNPACK #-} !AlarmId {-# UNPACK #-} !Timestamp !a !(Bucket a)
  | PopNada

pop :: Bucket a -> Pop a
pop = \case
  Nil -> PopNada
  Tip k p x -> PopAlgo k p x Nil
  Bin k p x m l r -> PopAlgo k p x (merge m l r)
{-# INLINE pop #-}

-- | Delete a timer from a bucket, expecting it to be there.
deleteExpectingHit :: AlarmId -> Bucket v -> Maybe (Bucket v)
deleteExpectingHit i =
  go
  where
    go :: Bucket v -> Maybe (Bucket v)
    go = \case
      Nil -> Nothing
      Tip j _ _
        | i == j -> Just Nil
        | otherwise -> Nothing
      Bin j p x m l r
        -- This commented out short-circuit is what makes this delete variant "expecting a hit"
        --   | prefixNotEqual m i j -> Nothing
        | i == j -> Just $! merge m l r
        | goleft i m -> (\l1 -> bin j p x m l1 r) <$> go l
        | otherwise -> bin j p x m l <$> go r

-- | 'Bin' smart constructor, respecting the invariant that both children can't be 'Nil'.
bin :: AlarmId -> Timestamp -> v -> Mask -> Bucket v -> Bucket v -> Bucket v
bin i p x _ Nil Nil = Tip i p x
bin i p x m l r = Bin i p x m l r
{-# INLINE bin #-}

link :: AlarmId -> Timestamp -> v -> AlarmId -> Bucket v -> Bucket v -> Bucket v
link i p x j t u
  | goleft j m = Bin i p x m t u
  | otherwise = Bin i p x m u t
  where
    m = onlyHighestBit (i2w i `xor` i2w j)
{-# INLINE link #-}

-- Merge two disjoint buckets that have the same mask.
merge :: Mask -> Bucket v -> Bucket v -> Bucket v
merge m l r =
  case (l, r) of
    (Nil, _) -> r
    (_, Nil) -> l
    --
    --    ip      jq
    --
    (Tip i p x, Tip j q y)
      --
      --       ip
      --      /  \
      --    nil  jq
      --
      | (p, i) < (q, j) -> Bin i p x m Nil r
      --
      --       jq
      --      /  \
      --    ip   nil
      --
      | otherwise -> Bin j q y m l Nil
    --
    --    ip      jq
    --           /  \
    --         rl    rr
    --
    (Tip i p x, Bin j q y n rl rr)
      --
      --       ip
      --      /  \
      --    nil  jq
      --        /  \
      --      rl    rr
      --
      | (p, i) < (q, j) -> Bin i p x m Nil r
      --
      --       jq
      --      /  \
      --    ip   rl+rr
      --
      | otherwise -> Bin j q y m l (merge n rl rr)
    --
    --       ip      jq
    --      /  \
    --    ll    lr
    --
    (Bin i p x n ll lr, Tip j q y)
      --
      --         ip
      --        /  \
      --    ll+lr   jq
      --
      | (p, i) < (q, j) -> Bin i p x m (merge n ll lr) r
      --
      --          jq
      --         /  \
      --       ip   nil
      --      /  \
      --    ll    lr
      --
      | otherwise -> Bin j q y m l Nil
    --
    --       ip          jq
    --      /  \        /  \
    --    ll    lr    rl    rr
    --
    (Bin i p x n ll lr, Bin j q y o rl rr)
      --
      --         ip
      --        /  \
      --    ll+lr   jq
      --           /  \
      --         rl    rr
      --
      | (p, i) < (q, j) -> Bin i p x m (merge n ll lr) r
      --
      --          jq
      --         /  \
      --       ip   rl+rr
      --      /  \
      --    ll    lr
      --
      | otherwise -> Bin j q y m l (merge o rl rr)

------------------------------------------------------------------------------------------------------------------------
-- Bit fiddling

-- | Is (or should) this timer be stored on the left of this bin, given its mask?
goleft :: AlarmId -> Mask -> Bool
goleft i m =
  i2w i .&. m == 0
{-# INLINE goleft #-}

-- m = 00001000000000000000000
-- i = IIII???????????????????
-- j = JJJJ???????????????????
--
-- prefixNotEqual m i j answers, is IIII not equal to JJJJ?
prefixNotEqual :: Mask -> AlarmId -> AlarmId -> Bool
prefixNotEqual (prefixMask -> e) i j =
  i2w i .&. e /= i2w j .&. e
{-# INLINE prefixNotEqual #-}

--            m = 0000000000100000
-- prefixMask m = 1111111111000000
prefixMask :: Word64 -> Word64
prefixMask m = -m `xor` m
{-# INLINE prefixMask #-}

onlyHighestBit :: Word64 -> Mask
onlyHighestBit w = unsafeShiftL 1 (WORD_SIZE_IN_BITS - 1 - countLeadingZeros w)
{-# INLINE onlyHighestBit #-}

i2w :: AlarmId -> Word64
i2w = fromIntegral
{-# INLINE i2w #-}

------------------------------------------------------------------------------------------------------------------------
-- Strict pair

data Pair a b
  = Pair !a !b
