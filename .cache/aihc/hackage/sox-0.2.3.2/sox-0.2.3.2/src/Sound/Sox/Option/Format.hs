module Sound.Sox.Option.Format (
   module Sound.Sox.Option.Format,
   T, single,
   ) where

import Sound.Sox.Private.Option (T, single, )

import qualified Sound.Sox.Private.Format as Format
import Data.Monoid (mempty, )


none :: T
none = mempty

numberOfChannels :: Int -> T
numberOfChannels n =
   single "-c" [show n]

numberOfChannelsAuto :: Int -> T
numberOfChannelsAuto n =
   if n == 1
     then mempty
     else numberOfChannels n

sampleRate :: Int -> T
sampleRate r =
   single "-r" [show r]

bitsPerSample :: Int -> T
bitsPerSample b =
   single "-b" [show b]

format :: Format.T -> T
format fmt =
   single "-t" [Format.decons fmt]
