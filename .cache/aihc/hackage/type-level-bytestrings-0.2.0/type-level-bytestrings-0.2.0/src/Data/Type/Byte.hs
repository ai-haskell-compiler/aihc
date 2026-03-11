{-# LANGUAGE AllowAmbiguousTypes #-} -- fully ambiguous type class

{- | Utilities for using @Natural@s as type-level bytes.

TODO would benefit from GHC 9.8's @RequiredTypeArguments@!
TODO assert that this works regardless of endianness (pretty sure but still)
-}

module Data.Type.Byte
  ( reifyW8
  , reifyW16
  , reifyW32
  , reifyW64
  , ReifyW8(reifyW8')
  ) where

import Numeric.Natural ( Natural )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Bits ( unsafeShiftL, (.|.) )

{-# INLINE reifyW64 #-}
-- | Reify 8 type-level bytes to a 'Word64'.
reifyW64
    :: forall n1 n2 n3 n4 n5 n6 n7 n8
    .  ( ReifyW8 n1
       , ReifyW8 n2
       , ReifyW8 n3
       , ReifyW8 n4
       , ReifyW8 n5
       , ReifyW8 n6
       , ReifyW8 n7
       , ReifyW8 n8
    ) => Word64
reifyW64 =           (reifyW8' @n1)
    .|. unsafeShiftL (reifyW8' @n2)  8
    .|. unsafeShiftL (reifyW8' @n3) 16
    .|. unsafeShiftL (reifyW8' @n4) 24
    .|. unsafeShiftL (reifyW8' @n5) 32
    .|. unsafeShiftL (reifyW8' @n6) 40
    .|. unsafeShiftL (reifyW8' @n7) 48
    .|. unsafeShiftL (reifyW8' @n8) 56

{-# INLINE reifyW32 #-}
-- | Reify 4 type-level bytes to a 'Word32'.
reifyW32
    :: forall n1 n2 n3 n4
    .  ( ReifyW8 n1
       , ReifyW8 n2
       , ReifyW8 n3
       , ReifyW8 n4
    ) => Word32
reifyW32 =           (reifyW8' @n1)
    .|. unsafeShiftL (reifyW8' @n2)  8
    .|. unsafeShiftL (reifyW8' @n3) 16
    .|. unsafeShiftL (reifyW8' @n4) 24

{-# INLINE reifyW16 #-}
-- | Reify 2 type-level bytes to a 'Word16'.
reifyW16
    :: forall n1 n2
    .  ( ReifyW8 n1
       , ReifyW8 n2
    ) => Word16
reifyW16 =           (reifyW8' @n1)
    .|. unsafeShiftL (reifyW8' @n2)  8

{-# INLINE reifyW8 #-}
-- | Reify a type-level byte to a 'Word8'.
reifyW8 :: forall n. ReifyW8 n => Word8
reifyW8 = reifyW8' @n

-- | Reify a type-level byte (stored in a type-level 'Natural').
--
-- We use polymorphic literals to remove a layer of 'fromIntegral' that would
-- otherwise be required when reifying to non-'Word8' types (e.g. reifying
-- multiple type-level bytes into a multibyte type).
--
-- That 'fromIntegral' should always be removed at runtime, but it's ugly
-- syntactically.
--
-- Attempting to reify a 'Natural' larger than 255 results in a type error.
class ReifyW8 (n :: Natural) where reifyW8' :: Num a => a
instance ReifyW8 0x00 where reifyW8' = 0x00
instance ReifyW8 0x01 where reifyW8' = 0x01
instance ReifyW8 0x02 where reifyW8' = 0x02
instance ReifyW8 0x03 where reifyW8' = 0x03
instance ReifyW8 0x04 where reifyW8' = 0x04
instance ReifyW8 0x05 where reifyW8' = 0x05
instance ReifyW8 0x06 where reifyW8' = 0x06
instance ReifyW8 0x07 where reifyW8' = 0x07
instance ReifyW8 0x08 where reifyW8' = 0x08
instance ReifyW8 0x09 where reifyW8' = 0x09
instance ReifyW8 0x0a where reifyW8' = 0x0a
instance ReifyW8 0x0b where reifyW8' = 0x0b
instance ReifyW8 0x0c where reifyW8' = 0x0c
instance ReifyW8 0x0d where reifyW8' = 0x0d
instance ReifyW8 0x0e where reifyW8' = 0x0e
instance ReifyW8 0x0f where reifyW8' = 0x0f
instance ReifyW8 0x10 where reifyW8' = 0x10
instance ReifyW8 0x11 where reifyW8' = 0x11
instance ReifyW8 0x12 where reifyW8' = 0x12
instance ReifyW8 0x13 where reifyW8' = 0x13
instance ReifyW8 0x14 where reifyW8' = 0x14
instance ReifyW8 0x15 where reifyW8' = 0x15
instance ReifyW8 0x16 where reifyW8' = 0x16
instance ReifyW8 0x17 where reifyW8' = 0x17
instance ReifyW8 0x18 where reifyW8' = 0x18
instance ReifyW8 0x19 where reifyW8' = 0x19
instance ReifyW8 0x1a where reifyW8' = 0x1a
instance ReifyW8 0x1b where reifyW8' = 0x1b
instance ReifyW8 0x1c where reifyW8' = 0x1c
instance ReifyW8 0x1d where reifyW8' = 0x1d
instance ReifyW8 0x1e where reifyW8' = 0x1e
instance ReifyW8 0x1f where reifyW8' = 0x1f
instance ReifyW8 0x20 where reifyW8' = 0x20
instance ReifyW8 0x21 where reifyW8' = 0x21
instance ReifyW8 0x22 where reifyW8' = 0x22
instance ReifyW8 0x23 where reifyW8' = 0x23
instance ReifyW8 0x24 where reifyW8' = 0x24
instance ReifyW8 0x25 where reifyW8' = 0x25
instance ReifyW8 0x26 where reifyW8' = 0x26
instance ReifyW8 0x27 where reifyW8' = 0x27
instance ReifyW8 0x28 where reifyW8' = 0x28
instance ReifyW8 0x29 where reifyW8' = 0x29
instance ReifyW8 0x2a where reifyW8' = 0x2a
instance ReifyW8 0x2b where reifyW8' = 0x2b
instance ReifyW8 0x2c where reifyW8' = 0x2c
instance ReifyW8 0x2d where reifyW8' = 0x2d
instance ReifyW8 0x2e where reifyW8' = 0x2e
instance ReifyW8 0x2f where reifyW8' = 0x2f
instance ReifyW8 0x30 where reifyW8' = 0x30
instance ReifyW8 0x31 where reifyW8' = 0x31
instance ReifyW8 0x32 where reifyW8' = 0x32
instance ReifyW8 0x33 where reifyW8' = 0x33
instance ReifyW8 0x34 where reifyW8' = 0x34
instance ReifyW8 0x35 where reifyW8' = 0x35
instance ReifyW8 0x36 where reifyW8' = 0x36
instance ReifyW8 0x37 where reifyW8' = 0x37
instance ReifyW8 0x38 where reifyW8' = 0x38
instance ReifyW8 0x39 where reifyW8' = 0x39
instance ReifyW8 0x3a where reifyW8' = 0x3a
instance ReifyW8 0x3b where reifyW8' = 0x3b
instance ReifyW8 0x3c where reifyW8' = 0x3c
instance ReifyW8 0x3d where reifyW8' = 0x3d
instance ReifyW8 0x3e where reifyW8' = 0x3e
instance ReifyW8 0x3f where reifyW8' = 0x3f
instance ReifyW8 0x40 where reifyW8' = 0x40
instance ReifyW8 0x41 where reifyW8' = 0x41
instance ReifyW8 0x42 where reifyW8' = 0x42
instance ReifyW8 0x43 where reifyW8' = 0x43
instance ReifyW8 0x44 where reifyW8' = 0x44
instance ReifyW8 0x45 where reifyW8' = 0x45
instance ReifyW8 0x46 where reifyW8' = 0x46
instance ReifyW8 0x47 where reifyW8' = 0x47
instance ReifyW8 0x48 where reifyW8' = 0x48
instance ReifyW8 0x49 where reifyW8' = 0x49
instance ReifyW8 0x4a where reifyW8' = 0x4a
instance ReifyW8 0x4b where reifyW8' = 0x4b
instance ReifyW8 0x4c where reifyW8' = 0x4c
instance ReifyW8 0x4d where reifyW8' = 0x4d
instance ReifyW8 0x4e where reifyW8' = 0x4e
instance ReifyW8 0x4f where reifyW8' = 0x4f
instance ReifyW8 0x50 where reifyW8' = 0x50
instance ReifyW8 0x51 where reifyW8' = 0x51
instance ReifyW8 0x52 where reifyW8' = 0x52
instance ReifyW8 0x53 where reifyW8' = 0x53
instance ReifyW8 0x54 where reifyW8' = 0x54
instance ReifyW8 0x55 where reifyW8' = 0x55
instance ReifyW8 0x56 where reifyW8' = 0x56
instance ReifyW8 0x57 where reifyW8' = 0x57
instance ReifyW8 0x58 where reifyW8' = 0x58
instance ReifyW8 0x59 where reifyW8' = 0x59
instance ReifyW8 0x5a where reifyW8' = 0x5a
instance ReifyW8 0x5b where reifyW8' = 0x5b
instance ReifyW8 0x5c where reifyW8' = 0x5c
instance ReifyW8 0x5d where reifyW8' = 0x5d
instance ReifyW8 0x5e where reifyW8' = 0x5e
instance ReifyW8 0x5f where reifyW8' = 0x5f
instance ReifyW8 0x60 where reifyW8' = 0x60
instance ReifyW8 0x61 where reifyW8' = 0x61
instance ReifyW8 0x62 where reifyW8' = 0x62
instance ReifyW8 0x63 where reifyW8' = 0x63
instance ReifyW8 0x64 where reifyW8' = 0x64
instance ReifyW8 0x65 where reifyW8' = 0x65
instance ReifyW8 0x66 where reifyW8' = 0x66
instance ReifyW8 0x67 where reifyW8' = 0x67
instance ReifyW8 0x68 where reifyW8' = 0x68
instance ReifyW8 0x69 where reifyW8' = 0x69
instance ReifyW8 0x6a where reifyW8' = 0x6a
instance ReifyW8 0x6b where reifyW8' = 0x6b
instance ReifyW8 0x6c where reifyW8' = 0x6c
instance ReifyW8 0x6d where reifyW8' = 0x6d
instance ReifyW8 0x6e where reifyW8' = 0x6e
instance ReifyW8 0x6f where reifyW8' = 0x6f
instance ReifyW8 0x70 where reifyW8' = 0x70
instance ReifyW8 0x71 where reifyW8' = 0x71
instance ReifyW8 0x72 where reifyW8' = 0x72
instance ReifyW8 0x73 where reifyW8' = 0x73
instance ReifyW8 0x74 where reifyW8' = 0x74
instance ReifyW8 0x75 where reifyW8' = 0x75
instance ReifyW8 0x76 where reifyW8' = 0x76
instance ReifyW8 0x77 where reifyW8' = 0x77
instance ReifyW8 0x78 where reifyW8' = 0x78
instance ReifyW8 0x79 where reifyW8' = 0x79
instance ReifyW8 0x7a where reifyW8' = 0x7a
instance ReifyW8 0x7b where reifyW8' = 0x7b
instance ReifyW8 0x7c where reifyW8' = 0x7c
instance ReifyW8 0x7d where reifyW8' = 0x7d
instance ReifyW8 0x7e where reifyW8' = 0x7e
instance ReifyW8 0x7f where reifyW8' = 0x7f
instance ReifyW8 0x80 where reifyW8' = 0x80
instance ReifyW8 0x81 where reifyW8' = 0x81
instance ReifyW8 0x82 where reifyW8' = 0x82
instance ReifyW8 0x83 where reifyW8' = 0x83
instance ReifyW8 0x84 where reifyW8' = 0x84
instance ReifyW8 0x85 where reifyW8' = 0x85
instance ReifyW8 0x86 where reifyW8' = 0x86
instance ReifyW8 0x87 where reifyW8' = 0x87
instance ReifyW8 0x88 where reifyW8' = 0x88
instance ReifyW8 0x89 where reifyW8' = 0x89
instance ReifyW8 0x8a where reifyW8' = 0x8a
instance ReifyW8 0x8b where reifyW8' = 0x8b
instance ReifyW8 0x8c where reifyW8' = 0x8c
instance ReifyW8 0x8d where reifyW8' = 0x8d
instance ReifyW8 0x8e where reifyW8' = 0x8e
instance ReifyW8 0x8f where reifyW8' = 0x8f
instance ReifyW8 0x90 where reifyW8' = 0x90
instance ReifyW8 0x91 where reifyW8' = 0x91
instance ReifyW8 0x92 where reifyW8' = 0x92
instance ReifyW8 0x93 where reifyW8' = 0x93
instance ReifyW8 0x94 where reifyW8' = 0x94
instance ReifyW8 0x95 where reifyW8' = 0x95
instance ReifyW8 0x96 where reifyW8' = 0x96
instance ReifyW8 0x97 where reifyW8' = 0x97
instance ReifyW8 0x98 where reifyW8' = 0x98
instance ReifyW8 0x99 where reifyW8' = 0x99
instance ReifyW8 0x9a where reifyW8' = 0x9a
instance ReifyW8 0x9b where reifyW8' = 0x9b
instance ReifyW8 0x9c where reifyW8' = 0x9c
instance ReifyW8 0x9d where reifyW8' = 0x9d
instance ReifyW8 0x9e where reifyW8' = 0x9e
instance ReifyW8 0x9f where reifyW8' = 0x9f
instance ReifyW8 0xa0 where reifyW8' = 0xa0
instance ReifyW8 0xa1 where reifyW8' = 0xa1
instance ReifyW8 0xa2 where reifyW8' = 0xa2
instance ReifyW8 0xa3 where reifyW8' = 0xa3
instance ReifyW8 0xa4 where reifyW8' = 0xa4
instance ReifyW8 0xa5 where reifyW8' = 0xa5
instance ReifyW8 0xa6 where reifyW8' = 0xa6
instance ReifyW8 0xa7 where reifyW8' = 0xa7
instance ReifyW8 0xa8 where reifyW8' = 0xa8
instance ReifyW8 0xa9 where reifyW8' = 0xa9
instance ReifyW8 0xaa where reifyW8' = 0xaa
instance ReifyW8 0xab where reifyW8' = 0xab
instance ReifyW8 0xac where reifyW8' = 0xac
instance ReifyW8 0xad where reifyW8' = 0xad
instance ReifyW8 0xae where reifyW8' = 0xae
instance ReifyW8 0xaf where reifyW8' = 0xaf
instance ReifyW8 0xb0 where reifyW8' = 0xb0
instance ReifyW8 0xb1 where reifyW8' = 0xb1
instance ReifyW8 0xb2 where reifyW8' = 0xb2
instance ReifyW8 0xb3 where reifyW8' = 0xb3
instance ReifyW8 0xb4 where reifyW8' = 0xb4
instance ReifyW8 0xb5 where reifyW8' = 0xb5
instance ReifyW8 0xb6 where reifyW8' = 0xb6
instance ReifyW8 0xb7 where reifyW8' = 0xb7
instance ReifyW8 0xb8 where reifyW8' = 0xb8
instance ReifyW8 0xb9 where reifyW8' = 0xb9
instance ReifyW8 0xba where reifyW8' = 0xba
instance ReifyW8 0xbb where reifyW8' = 0xbb
instance ReifyW8 0xbc where reifyW8' = 0xbc
instance ReifyW8 0xbd where reifyW8' = 0xbd
instance ReifyW8 0xbe where reifyW8' = 0xbe
instance ReifyW8 0xbf where reifyW8' = 0xbf
instance ReifyW8 0xc0 where reifyW8' = 0xc0
instance ReifyW8 0xc1 where reifyW8' = 0xc1
instance ReifyW8 0xc2 where reifyW8' = 0xc2
instance ReifyW8 0xc3 where reifyW8' = 0xc3
instance ReifyW8 0xc4 where reifyW8' = 0xc4
instance ReifyW8 0xc5 where reifyW8' = 0xc5
instance ReifyW8 0xc6 where reifyW8' = 0xc6
instance ReifyW8 0xc7 where reifyW8' = 0xc7
instance ReifyW8 0xc8 where reifyW8' = 0xc8
instance ReifyW8 0xc9 where reifyW8' = 0xc9
instance ReifyW8 0xca where reifyW8' = 0xca
instance ReifyW8 0xcb where reifyW8' = 0xcb
instance ReifyW8 0xcc where reifyW8' = 0xcc
instance ReifyW8 0xcd where reifyW8' = 0xcd
instance ReifyW8 0xce where reifyW8' = 0xce
instance ReifyW8 0xcf where reifyW8' = 0xcf
instance ReifyW8 0xd0 where reifyW8' = 0xd0
instance ReifyW8 0xd1 where reifyW8' = 0xd1
instance ReifyW8 0xd2 where reifyW8' = 0xd2
instance ReifyW8 0xd3 where reifyW8' = 0xd3
instance ReifyW8 0xd4 where reifyW8' = 0xd4
instance ReifyW8 0xd5 where reifyW8' = 0xd5
instance ReifyW8 0xd6 where reifyW8' = 0xd6
instance ReifyW8 0xd7 where reifyW8' = 0xd7
instance ReifyW8 0xd8 where reifyW8' = 0xd8
instance ReifyW8 0xd9 where reifyW8' = 0xd9
instance ReifyW8 0xda where reifyW8' = 0xda
instance ReifyW8 0xdb where reifyW8' = 0xdb
instance ReifyW8 0xdc where reifyW8' = 0xdc
instance ReifyW8 0xdd where reifyW8' = 0xdd
instance ReifyW8 0xde where reifyW8' = 0xde
instance ReifyW8 0xdf where reifyW8' = 0xdf
instance ReifyW8 0xe0 where reifyW8' = 0xe0
instance ReifyW8 0xe1 where reifyW8' = 0xe1
instance ReifyW8 0xe2 where reifyW8' = 0xe2
instance ReifyW8 0xe3 where reifyW8' = 0xe3
instance ReifyW8 0xe4 where reifyW8' = 0xe4
instance ReifyW8 0xe5 where reifyW8' = 0xe5
instance ReifyW8 0xe6 where reifyW8' = 0xe6
instance ReifyW8 0xe7 where reifyW8' = 0xe7
instance ReifyW8 0xe8 where reifyW8' = 0xe8
instance ReifyW8 0xe9 where reifyW8' = 0xe9
instance ReifyW8 0xea where reifyW8' = 0xea
instance ReifyW8 0xeb where reifyW8' = 0xeb
instance ReifyW8 0xec where reifyW8' = 0xec
instance ReifyW8 0xed where reifyW8' = 0xed
instance ReifyW8 0xee where reifyW8' = 0xee
instance ReifyW8 0xef where reifyW8' = 0xef
instance ReifyW8 0xf0 where reifyW8' = 0xf0
instance ReifyW8 0xf1 where reifyW8' = 0xf1
instance ReifyW8 0xf2 where reifyW8' = 0xf2
instance ReifyW8 0xf3 where reifyW8' = 0xf3
instance ReifyW8 0xf4 where reifyW8' = 0xf4
instance ReifyW8 0xf5 where reifyW8' = 0xf5
instance ReifyW8 0xf6 where reifyW8' = 0xf6
instance ReifyW8 0xf7 where reifyW8' = 0xf7
instance ReifyW8 0xf8 where reifyW8' = 0xf8
instance ReifyW8 0xf9 where reifyW8' = 0xf9
instance ReifyW8 0xfa where reifyW8' = 0xfa
instance ReifyW8 0xfb where reifyW8' = 0xfb
instance ReifyW8 0xfc where reifyW8' = 0xfc
instance ReifyW8 0xfd where reifyW8' = 0xfd
instance ReifyW8 0xfe where reifyW8' = 0xfe
instance ReifyW8 0xff where reifyW8' = 0xff
