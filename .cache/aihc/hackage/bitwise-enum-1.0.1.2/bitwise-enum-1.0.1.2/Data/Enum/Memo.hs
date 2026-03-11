{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE TypeApplications    #-}

-- | Immutable lazy tables of functions over bounded enumerations.
-- Function calls are stored as thunks and not evaluated until accessed.
--
-- The underlying representation is an 'Data.Array.Array' rather than a search
-- tree. This provides /O(1)/ lookup but means that the range of keys should not
-- be very large, as in the case of an 'Int'-like type.
module Data.Enum.Memo
  ( -- * Memoization
    memoize
    -- * Higher-order
  , memoize2
  , memoize3
  , memoize4
  , memoize5
  ) where

import Prelude hiding (lookup)

import Data.Array ((!), array)

-- | Memoize a function with a single argument.
memoize :: ∀ k v. (Bounded k, Enum k) => (k -> v) -> k -> v
memoize f = case array bounds vals of
    memo -> \k -> memo ! fromEnum k
  where
    bounds = (fromEnum @k minBound, fromEnum @k maxBound)
    vals   = [(fromEnum k, f k) | k <- [minBound..maxBound]]

-- | Memoize a function with two arguments.
memoize2 :: ∀ k1 k2 v.
            ( Bounded k1, Enum k1
            , Bounded k2, Enum k2
            )
         => (k1 -> k2 -> v) -> k1 -> k2 -> v
memoize2 f = case array bounds vals of
    memo -> \k1 k2 -> memo !
            ( fromEnum k1
            , fromEnum k2
            )
  where
    bounds = ( ( fromEnum @k1 minBound
               , fromEnum @k2 minBound
               )
             , ( fromEnum @k1 maxBound
               , fromEnum @k2 maxBound
               )
             )
    vals   = [( ( fromEnum k1
                , fromEnum k2
                )
              , f k1 k2
              ) | k1 <- [minBound..maxBound]
                , k2 <- [minBound..maxBound]]

-- | Memoize a function with three arguments.
memoize3 :: ∀ k1 k2 k3 v.
            ( Bounded k1, Enum k1
            , Bounded k2, Enum k2
            , Bounded k3, Enum k3
            )
         => (k1 -> k2 -> k3 -> v) -> k1 -> k2 -> k3 -> v
memoize3 f = case array bounds vals of
    memo -> \k1 k2 k3 -> memo !
            ( fromEnum k1
            , fromEnum k2
            , fromEnum k3
            )
  where
    bounds = ( ( fromEnum @k1 minBound
               , fromEnum @k2 minBound
               , fromEnum @k3 minBound
               )
             , ( fromEnum @k1 maxBound
               , fromEnum @k2 maxBound
               , fromEnum @k3 maxBound
               )
             )
    vals   = [( ( fromEnum k1
                , fromEnum k2
                , fromEnum k3
                )
              , f k1 k2 k3
              ) | k1 <- [minBound..maxBound]
                , k2 <- [minBound..maxBound]
                , k3 <- [minBound..maxBound]]

-- | Memoize a function with four arguments.
memoize4 :: ∀ k1 k2 k3 k4 v.
            ( Bounded k1, Enum k1
            , Bounded k2, Enum k2
            , Bounded k3, Enum k3
            , Bounded k4, Enum k4
            )
         => (k1 -> k2 -> k3 -> k4 -> v) -> k1 -> k2 -> k3 -> k4 -> v
memoize4 f = case array bounds vals of
    memo -> \k1 k2 k3 k4 -> memo !
            ( fromEnum k1
            , fromEnum k2
            , fromEnum k3
            , fromEnum k4
            )
  where
    bounds = ( ( fromEnum @k1 minBound
               , fromEnum @k2 minBound
               , fromEnum @k3 minBound
               , fromEnum @k4 minBound
               )
             , ( fromEnum @k1 maxBound
               , fromEnum @k2 maxBound
               , fromEnum @k3 maxBound
               , fromEnum @k4 maxBound
               )
             )
    vals   = [( ( fromEnum k1
                , fromEnum k2
                , fromEnum k3
                , fromEnum k4
                )
              , f k1 k2 k3 k4
              ) | k1 <- [minBound..maxBound]
                , k2 <- [minBound..maxBound]
                , k3 <- [minBound..maxBound]
                , k4 <- [minBound..maxBound]]

-- | Memoize a function with five arguments.
memoize5 :: ∀ k1 k2 k3 k4 k5 v.
            ( Bounded k1, Enum k1
            , Bounded k2, Enum k2
            , Bounded k3, Enum k3
            , Bounded k4, Enum k4
            , Bounded k5, Enum k5
            )
         => (k1 -> k2 -> k3 -> k4 -> k5 -> v) -> k1 -> k2 -> k3 -> k4 -> k5 -> v
memoize5 f = case array bounds vals of
    memo -> \k1 k2 k3 k4 k5 -> memo !
            ( fromEnum k1
            , fromEnum k2
            , fromEnum k3
            , fromEnum k4
            , fromEnum k5
            )
  where
    bounds = ( ( fromEnum @k1 minBound
               , fromEnum @k2 minBound
               , fromEnum @k3 minBound
               , fromEnum @k4 minBound
               , fromEnum @k5 minBound
               )
             , ( fromEnum @k1 maxBound
               , fromEnum @k2 maxBound
               , fromEnum @k3 maxBound
               , fromEnum @k4 maxBound
               , fromEnum @k5 maxBound
               )
             )
    vals   = [( ( fromEnum k1
                , fromEnum k2
                , fromEnum k3
                , fromEnum k4
                , fromEnum k5
                )
              , f k1 k2 k3 k4 k5
              ) | k1 <- [minBound..maxBound]
                , k2 <- [minBound..maxBound]
                , k3 <- [minBound..maxBound]
                , k4 <- [minBound..maxBound]
                , k5 <- [minBound..maxBound]]
