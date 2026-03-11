module Sound.ALSA.Sequencer.Time (
   T(Cons, mode, stamp), consAbs, consRel,
   Mode(Absolute, Relative), Stamp (Tick, Real),
   ) where

import Sound.ALSA.Sequencer.Marshal.Time as Time

