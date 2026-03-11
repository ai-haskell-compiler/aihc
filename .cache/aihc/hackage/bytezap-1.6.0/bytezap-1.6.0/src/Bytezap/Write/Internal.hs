module Bytezap.Write.Internal where

import Bytezap.Poke ( Poke )

-- | A 'Poke' buffer write operation with the associated length to be written.
--
-- The length may be either exact or a maximum.
--
-- TODO strictness?
data Write (lt :: LengthType) s = Write
  { writeLength :: Int
  -- ^ Length of the write in bytes.
  --
  -- This is not statically asserted. Any time you construct a 'Write', you must
  -- promise this.
  --
  -- For @'Write' 'ExactLength' s@, this is an exact measurement.
  -- For @'Write' 'MaxLength'   s@, this is a maximum.

  , writeOp :: Poke s
  -- ^ The 'Poke' buffer write operation.
  }

-- | What a buffer write length field means.
data LengthType
  = ExactLength -- ^ Exact length to be written.
  | MaxLength   -- ^ Maximum length to be written.

-- | Sequence the writes, sum the lengths.
instance Semigroup (Write lt s) where
    -- TODO strictness? INLINE[1]? INLINE[0]?
    (<>) = writeCombine

-- | The empty 'Write' is the empty 'Poke', which writes zero bytes.
instance Monoid (Write lt s) where
    mempty = Write 0 mempty

-- | Turn a @'Write' 'ExactLength'@ into a @'Write' 'MaxLength'@.
writeMax :: Write ExactLength s -> Write MaxLength s
writeMax (Write l p) = Write l p

-- | Sequence a @'Write' 'MaxLength'@ and a @'Write' 'ExactLength'@
--   left-to-right.
writeMaxExact :: Write MaxLength s -> Write ExactLength s -> Write MaxLength s
writeMaxExact = writeCombine

-- | Sequence a @'Write' 'MaxLength'@ and a @'Write' 'ExactLength'@
--   left-to-right.
writeExactMax :: Write ExactLength s -> Write MaxLength s -> Write MaxLength s
writeExactMax = writeCombine

-- | Sequence two 'Write's left-to-right.
--
-- Unsafe, as it ignores 'LengthType's.
--
-- TODO strictness? INLINE[1]? INLINE[0]?
writeCombine :: Write ltl s -> Write ltr s -> Write lt s
writeCombine (Write ll lp) (Write rl rp) = Write (ll + rl) (lp <> rp)
