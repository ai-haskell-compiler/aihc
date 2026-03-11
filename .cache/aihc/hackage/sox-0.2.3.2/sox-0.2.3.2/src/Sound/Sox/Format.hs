module Sound.Sox.Format (
   module Sound.Sox.Format,
   T,
   ) where

import Sound.Sox.Private.Format (T(Cons), )


custom :: String -> T
custom = Cons

aiff :: T
aiff = custom "aiff"

wave :: T
wave = custom "wav"

ogg :: T
ogg = custom "ogg"

mp3 :: T
mp3 = custom "mp3"

iff8svx :: T
iff8svx = custom "8svx"

muLaw :: T
muLaw = custom "ul"


signedByte, unsignedByte :: T
signedByte   = custom "s8"
unsignedByte = custom "u8"

signedWord, unsignedWord :: T
signedWord   = custom "s16"
unsignedWord = custom "u16"

signedLong, unsignedLong :: T
signedLong   = custom "s32"
unsignedLong = custom "u32"

ieeeSinglePrecision, ieeeDoublePrecision :: T
ieeeSinglePrecision = custom "f32"
ieeeDoublePrecision = custom "f64"
