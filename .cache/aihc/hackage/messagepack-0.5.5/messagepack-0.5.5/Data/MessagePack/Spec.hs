{-|
Module      : Data.MessagePack.Spec
Description : Message Pack specification values
Copyright   : (c) Rodrigo Setti, 2014
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : portable

Define, in a single place, all the message-pack specification binary type
markers.
-}
module Data.MessagePack.Spec where

import Data.Word

posFixintMask = 0x80    -- 10000000
negFixintMask = 0xe0    -- 11100000
fixmapMask    = 0xf0    -- 11110000
fixarrayMask  = 0xf0    -- 11110000
fixstrMask    = 0xe0    -- 11100000

posFixint = 0x00        -- 0xxxxxxx
negFixint = 0xe0        -- 111xxxxx
fixmap    = 0x80        -- 1000xxxx
fixarray  = 0x90        -- 1001xxxx
fixstr    = 0xa0        -- 101xxxxx
nil       = 0xc0        -- 11000000
false     = 0xc2        -- 11000010
true      = 0xc3        -- 11000011
bin8      = 0xc4        -- 11000100
bin16     = 0xc5        -- 11000101
bin32     = 0xc6        -- 11000110
ext8      = 0xc7        -- 11000111
ext16     = 0xc8        -- 11001000
ext32     = 0xc9        -- 11001001
float32   = 0xca        -- 11001010
float64   = 0xcb        -- 11001011
uint8     = 0xcc        -- 11001100
uint16    = 0xcd        -- 11001101
uint32    = 0xce        -- 11001110
uint64    = 0xcf        -- 11001111
int8      = 0xd0        -- 11010000
int16     = 0xd1        -- 11010001
int32     = 0xd2        -- 11010010
int64     = 0xd3        -- 11010011
fixext1   = 0xd4        -- 11010100
fixext2   = 0xd5        -- 11010101
fixext4   = 0xd6        -- 11010110
fixext8   = 0xd7        -- 11010111
fixext16  = 0xd8        -- 11011000
str8      = 0xd9        -- 11011001
str16     = 0xda        -- 11011010
str32     = 0xdb        -- 11011011
array16   = 0xdc        -- 11011100
array32   = 0xdd        -- 11011101
map16     = 0xde        -- 11011110
map32     = 0xdf        -- 11011111

posFixintMask :: Word8
negFixintMask :: Word8
fixmapMask    :: Word8
fixarrayMask  :: Word8
fixstrMask    :: Word8

posFixint :: Word8
negFixint :: Word8
fixmap    :: Word8
fixarray  :: Word8
fixstr    :: Word8
nil       :: Word8
false     :: Word8
true      :: Word8
bin8      :: Word8
bin16     :: Word8
bin32     :: Word8
ext8      :: Word8
ext16     :: Word8
ext32     :: Word8
float32   :: Word8
float64   :: Word8
uint8     :: Word8
uint16    :: Word8
uint32    :: Word8
uint64    :: Word8
int8      :: Word8
int16     :: Word8
int32     :: Word8
int64     :: Word8
fixext1   :: Word8
fixext2   :: Word8
fixext4   :: Word8
fixext8   :: Word8
fixext16  :: Word8
str8      :: Word8
str16     :: Word8
str32     :: Word8
array16   :: Word8
array32   :: Word8
map16     :: Word8
map32     :: Word8

