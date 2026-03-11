module Sound.Sox.Frame (C(..), Frame.withSignal, Frame.numberOfChannels, ) where

import qualified Sound.Frame as Frame
import qualified Sound.Frame.Stereo as Stereo
import qualified Sound.Frame.MuLaw  as MuLaw

import qualified Sound.Sox.Format as Format

import Data.Word (Word8, Word16, Word32, )
import Data.Int (Int8, Int16, Int32, )


class Frame.C y => C y where
   format :: y -> Format.T


instance C Word8 where
   format _ = Format.unsignedByte

instance C Int8 where
   format _ = Format.signedByte

instance C Word16 where
   format _ = Format.unsignedWord

instance C Int16 where
   format _ = Format.signedWord

instance C Word32 where
   format _ = Format.unsignedLong

instance C Int32 where
   format _ = Format.signedLong

{- |
The floating point instances are dangerous,
because Storable Float may not use IEEE format
that sox uses according to its man page.
This is strange since sox uses the host's endianess for multi-byte values.
So, why does it not use the machine's floating point format?
-}
instance C Float where
   format _ = Format.ieeeSinglePrecision

instance C Double where
   format _ = Format.ieeeDoublePrecision

instance C MuLaw.T where
   format _ = Format.muLaw

{-
Shall we add instances for Float and Double?
Sox requires floating point numbers in IEEE formats,
but we cannot warrant that the Storable instances uses those formats.
-}

instance C a => C (Stereo.T a) where
   format y = format (Stereo.left y)
