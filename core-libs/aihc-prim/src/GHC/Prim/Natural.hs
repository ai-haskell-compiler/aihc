{-# LANGUAGE MagicHash #-}

-- | The arbitrary-precision non-negative integer type.
--
-- The declaration lives here, beside 'GHC.Prim.Integer.Integer', rather
-- than in @GHC.Num.Natural@ where GHC's @ghc-bignum@ puts it. The type is
-- an asset of the compiler: it is the kind of a type-level natural
-- literal, so the type checker has to name it, and the compiler must not
-- depend on @aihc-base@ for anything it is built on. @GHC.Num.Natural@
-- re-exports it and declares its instances.
module GHC.Prim.Natural
  ( Natural (..),
    naturalFromInteger#,
  )
where

import GHC.Prim (ByteArray#, Word#, indexWordArray#, int2Word#, sizeofByteArray#, (==#))
import GHC.Prim.Integer (Integer (..))
import GHC.Types (Bool (..), isTrue#)

-- | An arbitrary-precision non-negative integer.
--
-- The representation mirrors @ghc-bignum@: a value that fits in a 'Word#'
-- stays unallocated in 'NS', and anything larger carries the canonical
-- little-endian magnitude of 'GHC.Prim.Integer.Integer' in 'NB'. 'NB'
-- therefore never holds a magnitude of a single limb.
data Natural
  = NS Word#
  | NB ByteArray#

-- | The 'Natural' with the same value as a non-negative 'Integer'.
--
-- The caller checks the sign. The compiler builds the evidence of a
-- @KnownNat@ constraint with this, and a type-level literal is never
-- negative; @GHC.Num.Natural.naturalFromInteger@ is the checked version
-- that underflows instead.
naturalFromInteger# :: Integer -> Natural
naturalFromInteger# (IS value) = NS (int2Word# value)
naturalFromInteger# (IP magnitude) =
  -- A single-limb magnitude is only ever an 'Integer' above
  -- @maxBound :: Int@, which still fits a 'Word#'.
  case isTrue# ((==#) (sizeofByteArray# magnitude) 8#) of
    True -> NS (indexWordArray# magnitude 0#)
    False -> NB magnitude
-- Unreachable: the argument is non-negative.
naturalFromInteger# (IN magnitude) = NB magnitude
