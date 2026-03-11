module Sound.MED.Generic.Tempo (
  MEDTempo(..),
  Mode(..),
  song0Tempo,
  song2Tempo,

  update,
  toTime,
  ) where

import Sound.MED.Generic.Block(Cmd,Val)

import qualified Sound.MED.Raw.MMD0Song as MMD0Song
import qualified Sound.MED.Raw.MMD2Song as MMD2Song

import Sound.MED.Basic.Amiga

import Data.Bits (testBit, (.&.))
import Data.Bool.HT (if')

data MEDTempo = MEDTempo
  { mode :: Mode,
    primary, secondary :: Int
  }

data Mode = Speed | Octa | BPM {linesPerBeat :: Int}

tempoMode :: UBYTE -> UBYTE -> Mode
tempoMode flags flags2 =
  if' (testBit flags 6) Octa $
  if' (testBit flags2 5) (BPM $ fromIntegral (flags2 .&. 0x1F) + 1)
  Speed

song0Tempo :: MMD0Song.MMD0Song -> MEDTempo
song0Tempo song =
  MEDTempo
    { mode = tempoMode (MMD0Song.flags song) (MMD0Song.flags2 song)
    , primary = fromIntegral $ MMD0Song.deftempo song
    , secondary = fromIntegral $ MMD0Song.tempo2 song
    }

song2Tempo :: MMD2Song.MMD2Song -> MEDTempo
song2Tempo song =
  MEDTempo
    { mode = tempoMode (MMD2Song.flags song) (MMD2Song.flags2 song)
    , primary = fromIntegral $ MMD2Song.deftempo song
    , secondary = fromIntegral $ MMD2Song.tempo2 song
    }


{- |
Interpret tempo related commands.
-}
update :: MEDTempo -> (Cmd, Val) -> MEDTempo
update tempo (cmd,val) =
  case cmd of
    0x09 -> tempo{secondary = mod (val-1) 0x20 + 1}
    0x0F ->
      if 0 < val && val < 0xF0
        then tempo{primary = fromIntegral val}
        else tempo
    _ -> tempo


toTime :: Fractional a => MEDTempo -> a
toTime (MEDTempo mode_ tempo1 tempo2) =
  timeFromPrimary mode_ tempo1 * fromIntegral tempo2

-- numbers taken from uade/amigasrc/players/med/common/proplayer.a
ciabFreq :: Fractional a => a
ciabFreq = 715909

timerDiv :: Fractional a => a
timerDiv = 474326

_sttempo :: Fractional a => a
_sttempo = 2416.3

{-
Measured from output of uade.
In theory it should be 300 (50 Hz times 6 divisions).
-}
sttempoMeasured :: Fractional a => a
sttempoMeasured = 293.70

octaTempo :: Fractional a => a
octaTempo = 390.70

timeFromPrimary :: Fractional a => Mode -> Int -> a
timeFromPrimary mode_ tempo =
  case mode_ of
    BPM lpb -> 60 / (fromIntegral tempo * 6 * fromIntegral lpb)
    Octa -> fromIntegral (min 10 tempo) / octaTempo
    Speed ->
      if tempo<=10
        -- then fromIntegral tempo * sttempo / ciabFreq
        then fromIntegral tempo / sttempoMeasured
        else timerDiv / (ciabFreq * fromIntegral tempo)
