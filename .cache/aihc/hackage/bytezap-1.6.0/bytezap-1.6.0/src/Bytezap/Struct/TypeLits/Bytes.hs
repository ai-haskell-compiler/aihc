{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}

{- | Efficient type-level bytestring serialization via chunking.

I did a quick Core check and found that GHC seems to successfully generate
minimal code for this e.g. for an 8-byte magic, GHC will do one
@writeWord64OffAddr#@ of a constant. Great!

The only way I can think of to make this faster is to somehow obtain an 'Addr#'
with a known length. With that, we could @memcpy@. But that would be slower for
small magics, and maybe others. And I doubt we can conjure up an 'Addr#' at
compile time. So I'm fairly confident that this is the best you're gonna get.
-}

module Bytezap.Struct.TypeLits.Bytes
  (
  -- * Chunking design
  -- $chunking-design

    ReifyBytesW64(reifyBytesW64)
  , ReifyBytesW32(reifyBytesW32)
  , ReifyBytesW16(reifyBytesW16)
  , ReifyBytesW8(reifyBytesW8)
  ) where

import Data.Type.Byte
import Bytezap.Struct ( Poke, sequencePokes, emptyPoke, prim )
import Numeric.Natural ( Natural )

{- $chunking-design
@['Natural']@s have a convenient syntax, and we can use them as a type-level
bytestring by asserting that each 'Natural' is <=255 when reifying. This module
provides type classes which give you a serializer for a given @['Natural']@.

We maximize efficiency by grouping bytes into machine words. We have to be
pretty verbose to achieve this. Each type class attempts to group bytes into its
machine word type, and if it can't (i.e. not enough bytes remain), it hands off
to the next type class which handles the next smaller machine word.

TODO rewrite :)
-}

-- | Serialize a type-level bytestring, largest grouping 'Word64'.
class ReifyBytesW64 (bs :: [Natural]) where reifyBytesW64 :: Poke s

-- | Enough bytes to make a 'Word64'.
instance {-# OVERLAPPING #-}
  ( ReifyW8 b0
  , ReifyW8 b1
  , ReifyW8 b2
  , ReifyW8 b3
  , ReifyW8 b4
  , ReifyW8 b5
  , ReifyW8 b6
  , ReifyW8 b7
  , ReifyBytesW64 bs
  ) => ReifyBytesW64 (b0 ': b1 ': b2 ': b3 ': b4 ': b5 ': b6 ': b7 ': bs) where
    {-# INLINE reifyBytesW64 #-}
    reifyBytesW64 = sequencePokes
        (prim (reifyW64 @b0 @b1 @b2 @b3 @b4 @b5 @b6 @b7)) 8 (reifyBytesW64 @bs)

-- | Try to group 'Word32's next.
instance ReifyBytesW32 bs => ReifyBytesW64 bs where
    {-# INLINE reifyBytesW64 #-}
    reifyBytesW64 = reifyBytesW32 @bs

-- | Serialize a type-level bytestring, largest grouping 'Word32'.
class ReifyBytesW32 (bs :: [Natural]) where reifyBytesW32 :: Poke s

-- | Enough bytes to make a 'Word32'.
instance {-# OVERLAPPING #-}
  ( ReifyW8 b0
  , ReifyW8 b1
  , ReifyW8 b2
  , ReifyW8 b3
  , ReifyBytesW32 bs
  ) => ReifyBytesW32 (b0 ': b1 ': b2 ': b3 ': bs) where
    {-# INLINE reifyBytesW32 #-}
    reifyBytesW32 = sequencePokes
        (prim (reifyW32 @b0 @b1 @b2 @b3)) 4 (reifyBytesW32 @bs)

-- | Try to group 'Word16's next.
instance ReifyBytesW16 bs => ReifyBytesW32 bs where
    {-# INLINE reifyBytesW32 #-}
    reifyBytesW32 = reifyBytesW16 @bs

-- | Serialize a type-level bytestring, largest grouping 'Word16'.
class ReifyBytesW16 (bs :: [Natural]) where reifyBytesW16 :: Poke s

-- | Enough bytes to make a 'Word16'.
instance {-# OVERLAPPING #-}
  ( ReifyW8 b0
  , ReifyW8 b1
  , ReifyBytesW16 bs
  ) => ReifyBytesW16 (b0 ': b1 ': bs) where
    {-# INLINE reifyBytesW16 #-}
    reifyBytesW16 = sequencePokes
        (prim (reifyW16 @b0 @b1)) 2 (reifyBytesW16 @bs)

-- | Reify byte-by-byte next.
instance ReifyBytesW8 bs => ReifyBytesW16 bs where
    {-# INLINE reifyBytesW16 #-}
    reifyBytesW16 = reifyBytesW8 @bs

-- | Serialize a type-level bytestring, byte-by-byte.
class ReifyBytesW8 (bs :: [Natural]) where reifyBytesW8 :: Poke s

-- | Reify the next byte.
instance
  ( ReifyW8 b0
  , ReifyBytesW8 bs
  ) => ReifyBytesW8 (b0 ': bs) where
    {-# INLINE reifyBytesW8 #-}
    reifyBytesW8 = sequencePokes
        (prim (reifyW8 @b0)) 1 (reifyBytesW8 @bs)

-- | End of the line.
instance ReifyBytesW8 '[] where
    {-# INLINE reifyBytesW8 #-}
    reifyBytesW8 = emptyPoke
