{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}

{- | Efficient type-level bytestring parsing via chunking.

See 'Bytezap.Struct.TypeLits.Bytes' for an explanation on the chunking design.

On mismatch, the index of the failing byte and its value are returned. (This is
over-engineered to be extremely efficient.)

Type classes take a 'Natural' for tracking the current index in the type-level
bytestring. We do this on the type level for performance. Use @\@0@ when
calling.

The parsers take an error wrapper function to enable wrapping the error into any
parser with confidence that it won't do extra allocations/wrapping.

The parsers here either return the unit '()' or a pretty error. No 'Fail#'.

TODO check generated Core, assembly
-}

module Bytezap.Parser.Struct.TypeLits.Bytes
  ( ParseReifyBytesW64(parseReifyBytesW64)
  , ParseReifyBytesW32(parseReifyBytesW32)
  , ParseReifyBytesW16(parseReifyBytesW16)
  , ParseReifyBytesW8(parseReifyBytesW8)
  ) where

import Bytezap.Parser.Struct
import Data.Word ( Word8 )
import GHC.TypeNats ( Natural, type (+), KnownNat )
import Data.Type.Byte ( ReifyW8, reifyW64, reifyW32, reifyW16, reifyW8 )
import GHC.Exts ( (+#), Int(I#), Int#, Addr# )
import Bytezap.Common.TypeNats ( natValInt )
import Raehik.Compat.Data.Primitive.Types ( indexWord8OffAddrAs# )
import Data.Bits

-- | Parse a type-level bytestring, largest grouping 'Word64'.
class ParseReifyBytesW64 (idx :: Natural) (bs :: [Natural]) where
    parseReifyBytesW64 :: ParserT st (Int, Word8) ()

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
  , KnownNat idx
  , ParseReifyBytesW64 (idx+8) bs
  ) => ParseReifyBytesW64 idx (b0 ': b1 ': b2 ': b3 ': b4 ': b5 ': b6 ': b7 ': bs) where
    {-# INLINE parseReifyBytesW64 #-}
    parseReifyBytesW64 = parseReifyBytesHelper @idx @8
        wExpect indexWord8OffAddrAs# (parseReifyBytesW64 @(idx+8) @bs)
      where
        wExpect = reifyW64 @b0 @b1 @b2 @b3 @b4 @b5 @b6 @b7

-- | Try to group 'Word32's next.
instance ParseReifyBytesW32 idx bs => ParseReifyBytesW64 idx bs where
    {-# INLINE parseReifyBytesW64 #-}
    parseReifyBytesW64 = parseReifyBytesW32 @idx @bs

-- | Parse a type-level bytestring, largest grouping 'Word32'.
class ParseReifyBytesW32 (idx :: Natural) (bs :: [Natural]) where
    parseReifyBytesW32 :: ParserT st (Int, Word8) ()

-- | Enough bytes to make a 'Word32'.
instance {-# OVERLAPPING #-}
  ( ReifyW8 b0
  , ReifyW8 b1
  , ReifyW8 b2
  , ReifyW8 b3
  , KnownNat idx
  , ParseReifyBytesW32 (idx+4) bs
  ) => ParseReifyBytesW32 idx (b0 ': b1 ': b2 ': b3 ': bs) where
    {-# INLINE parseReifyBytesW32 #-}
    parseReifyBytesW32 = parseReifyBytesHelper @idx @4
        wExpect indexWord8OffAddrAs# (parseReifyBytesW32 @(idx+4) @bs)
      where
        wExpect = reifyW32 @b0 @b1 @b2 @b3

-- | Try to group 'Word16's next.
instance ParseReifyBytesW16 idx bs => ParseReifyBytesW32 idx bs where
    {-# INLINE parseReifyBytesW32 #-}
    parseReifyBytesW32 = parseReifyBytesW16 @idx @bs

-- | Parse a type-level bytestring, largest grouping 'Word16'.
class ParseReifyBytesW16 (idx :: Natural) (bs :: [Natural]) where
    parseReifyBytesW16 :: ParserT st (Int, Word8) ()

-- | Enough bytes to make a 'Word16'.
instance {-# OVERLAPPING #-}
  ( ReifyW8 b0
  , ReifyW8 b1
  , KnownNat idx
  , ParseReifyBytesW16 (idx+2) bs
  ) => ParseReifyBytesW16 idx (b0 ': b1 ': bs) where
    {-# INLINE parseReifyBytesW16 #-}
    parseReifyBytesW16 = parseReifyBytesHelper @idx @2
        wExpect indexWord8OffAddrAs# (parseReifyBytesW16 @(idx+2) @bs)
      where
        wExpect = reifyW16 @b0 @b1

-- | Parse byte-by-byte next.
instance ParseReifyBytesW8 idx bs => ParseReifyBytesW16 idx bs where
    {-# INLINE parseReifyBytesW16 #-}
    parseReifyBytesW16 = parseReifyBytesW8 @idx @bs

-- | Serialize a type-level bytestring, byte-by-byte.
class ParseReifyBytesW8 (idx :: Natural) (bs :: [Natural]) where
    parseReifyBytesW8 :: ParserT st (Int, Word8) ()

-- | Parse the next byte.
instance
  ( ReifyW8 b0
  , KnownNat idx
  , ParseReifyBytesW8 (idx+1) bs
  ) => ParseReifyBytesW8 idx (b0 ': bs) where
    {-# INLINE parseReifyBytesW8 #-}
    parseReifyBytesW8 = ParserT $ \fpc base# os# st ->
        let bExpect = reifyW8 @b0
            bActual = indexWord8OffAddrAs# base# os#
            idx     = natValInt @idx
        in  if   bExpect == bActual
            then runParserT# (parseReifyBytesW8 @(idx+1) @bs) fpc base# (os# +# 1#) st
            else Err# st (idx, bActual)

-- | End of the line.
instance ParseReifyBytesW8 idx '[] where
    {-# INLINE parseReifyBytesW8 #-}
    parseReifyBytesW8 = constParse ()

{-# INLINE parseReifyBytesHelper #-}
parseReifyBytesHelper
    :: forall (idx :: Natural) (len :: Natural) a st
    .  (KnownNat idx, KnownNat len, Integral a, FiniteBits a)
    => a -> (Addr# -> Int# -> a)
    -> ParserT st (Int, Word8) ()
    -> ParserT st (Int, Word8) ()
parseReifyBytesHelper aLit indexWord8OffAddrAsA# pCont = withLitErr
    len# aLit idx indexWord8OffAddrAsA# pCont
  where
    !(I# len#)     = natValInt @len
    idx            = natValInt @idx
