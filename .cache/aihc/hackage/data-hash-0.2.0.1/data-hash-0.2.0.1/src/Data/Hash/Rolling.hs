------------------------------------------------------------------------------
-- |
-- Module       : Data.Hash.Rolling
-- License      : BSD-style
--
-- Maintainer   : jcpetruzza@gmail.com
-- Stability    : experimental
-- Portability  : portable
--
-- Efficient implementation of a rolling hash, i.e., the computation of a hash through
-- a moving window of a fixed size @n@ over some stream. All operations are O(1) (in
-- particular, they do not depend on the size of the window).
--
-- Some laws that this type satisfies:
--
-- * @currentHash rh == foldl1 combine (lastHashes rh)@
--
-- * @length (lastHashes rh) <= windowSize rh@
--
-- * @length (lastHashes $ addAndRoll rh a) == windowSize rh -- whenever length (lastHashes rh) == windowSize rh@
--
-- * @last (lastHashes $ addAndRoll rh x) == hash a@
--
-- * @init (lastHashes $ addAndRoll rh a) `isSuffixOf` (lastHashes rh)@
-------------------------------------------------------------------------------
module Data.Hash.Rolling (
    -- * The @RollingHash@ type
    RollingHash,
    -- ** Construction and manipulation
    rollingHash, addAndRoll,
    -- ** Querying
    currentHash, lastHashes, windowSize
)

where

import Data.Hash.Base
import Data.Hash.Instances
import Data.Bits
import qualified Data.Sequence as S
import Data.Foldable
import Text.Show.Functions ()


data RollingHash a = RH {
     currentHash :: Hash
    ,windowSize  :: Int
    ,hseq        :: S.Seq Hash
    ,addHashImpl :: RollingHash a -> Hash -> RollingHash a
    } deriving Show

-- | @rollingHash n@ creates a @RollingHash@ of window
--   size @n@ (for @n > 0@)
rollingHash :: Int -> RollingHash a
rollingHash n
  | n == 0    = error $ "rollingHash: invalid window size " ++ show n
  | otherwise = RH {
       currentHash = initial_hash
      ,windowSize  = n
      ,hseq        = S.singleton initial_hash
      ,addHashImpl = accumulateNext (n - 1)
    }
    where initial_hash = hash () `combine` hash n

defaultAddHash :: RollingHash a -> Hash -> RollingHash a
defaultAddHash rh hv = rh { currentHash = (currentHash rh) `combine` (Hash $ rotate c1 k `xor` ck)
                          ,        hseq = (S.drop 1 $ hseq rh) S.|> hv
                          }
    where ck = asWord64 hv
          c1 = asWord64 $ S.index (hseq rh) 0
          k = S.length $ hseq rh


-- | @addAndRoll x rh@ adds a new input element and rolls the window
--   one place through the input (if at least @n@ elements were
--   already consumed).
addAndRoll ::  Hashable a => RollingHash a -> a -> RollingHash a
addAndRoll r a = (addHashImpl r) r (hash a)

accumulateNext :: Int -> RollingHash a -> Hash -> RollingHash a
accumulateNext n | n > 0 = \rh h -> rh {
                            currentHash = currentHash rh `combine` h,
                            hseq = (hseq rh) S.|> h,
                            addHashImpl = accumulateNext (n - 1)
                        }
             | otherwise = defaultAddHash

-- | @lastHashes rh@ returns the last @n@ hashes.
lastHashes :: RollingHash a -> [Hash]
lastHashes = toList . hseq
