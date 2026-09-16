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
  )
where

import GHC.Prim (ByteArray#, Word#)

-- | An arbitrary-precision non-negative integer.
--
-- The representation mirrors @ghc-bignum@: a value that fits in a 'Word#'
-- stays unallocated in 'NS', and anything larger carries the canonical
-- little-endian magnitude of 'GHC.Prim.Integer.Integer' in 'NB'. 'NB'
-- therefore never holds a magnitude of a single limb.
data Natural
  = NS Word#
  | NB ByteArray#
