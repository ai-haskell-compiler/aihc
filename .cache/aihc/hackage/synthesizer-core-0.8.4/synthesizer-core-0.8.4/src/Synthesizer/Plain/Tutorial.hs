{- |
This module gives some introductory examples to signal processing
with plain Haskell lists.
For more complex examples
see "Synthesizer.Plain.Instrument"
and "Synthesizer.Plain.Effect".
The examples require a basic understanding of audio signal processing.

In the Haddock documentation you will only see the API.
In order to view the example code,
please use the \"Source code\" links beside the function documentation.
This requires however,
that the Haddock was executed with @hyperlink-source@ option.

Using plain lists is not very fast,
particularly not fast enough for serious real-time applications.
It is however the most flexible data structure,
which you can also use without knowledge of low level programming.
For real-time applications see "Synthesizer.Generic.Tutorial".
-}
module Synthesizer.Plain.Tutorial
{-# DEPRECATED "do not import that module, it is only intended for demonstration" #-}
 where

import qualified Synthesizer.Plain.Play as Play
import qualified Synthesizer.Plain.File as File
import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.Plain.Control as Ctrl
import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Plain.Filter.NonRecursive as Filt
import qualified Synthesizer.Plain.Filter.Recursive as FiltRec
import qualified Synthesizer.Plain.Filter.Recursive.Universal as UniFilter
import qualified Synthesizer.Basic.Wave as Wave

import qualified Algebra.Module as Module -- needed for Haddock

import System.Exit (ExitCode, )
import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


{- |
Play a simple sine tone at 44100 sample rate and 16 bit.
These are the parameters used for compact disks.
The period of the tone is @2*pi*10@.
Playing at sample rate 44100 Hz results in a tone of @44100 / (20*pi) Hz@,
that is about @702 Hz@.
This is simple enough to be performed in real-time,
at least on my machine.
For playback we use @SoX@.
-}
sine :: IO ExitCode
sine =
   Play.monoToInt16 (44100::Double) (map sin [0::Double,0.1..])

{- |
Now the same for a stereo signal.
Both stereo channels are slightly detuned
in order to achieve a stereophonic phasing effect.
In principle there is no limit of the number of channels,
but with more channels playback becomes difficult.
Many signal processes in our package
support any tuple and even nested tuples
using the notion of an algebraic @module@ (see 'Module.C').
A module is a vector space where the scalar numbers
do not need to support division.
A vector space is often also called a linear space,
because all we require of vectors is that they can be added and scaled
and these two operations fulfill some natural laws.
-}
sineStereo :: IO ExitCode
sineStereo =
   Play.stereoToInt16 (44100::Double) $ zip (map sin [0::Double,0.0998..]) (map sin [0::Double,0.1002..])

{- |
Of course we can also write a tone to disk using @sox@.
-}
writeSine :: IO ExitCode
writeSine =
   File.writeToInt16 "sine.aiff" (44100::Double) (take 50000 $ map sin [0::Double,0.1..])


{- |
For the following examples we will stick to monophonic sounds played at 44100 Hz.
Thus we define a function for convenience.
-}
play :: Sig.T Double -> IO ExitCode
play = Play.monoToInt16 (44100::Double)

{- |
Now, let's repeat the 'sine' example in a higher level style.
We use the oscillator 'Osci.static' that does not allow any modulation.
We can however use any waveform.
The waveform is essentially a function
which maps from the phase to the displacement.
Functional programming proves to be very useful here,
since anonymous functions as waveforms are optimally supported by the language.
We can also expect, that in compiled form
the oscillator does not have to call back the waveform function
by an expensive explicit function call,
but that the compiler will inline both oscillator and waveform
such that the oscillator is turned into a simple loop
which handles both oscillation and waveform computation.

Using the oscillator with 'Wave.sine' also has the advantage
that we do not have to cope with 'pi's any longer.
The frequency is given as ratio of the sample rate.
That is, @0.01@ at @44100 Hz@ sample rate means @441 Hz@.
This way all frequencies are given in the low-level signal processing.

It is not optimal to handle frequencies this way,
since all frequency values are bound to the sample rate.
For overcoming this problem, see the high level routines using physical dimensions.
For examples see "Synthesizer.Dimensional.RateAmplitude.Demonstration".
-}
oscillator :: IO ExitCode
oscillator =
   play (Osci.static Wave.sine 0 (0.01::Double))

{- |
It is very simple to switch to another waveform like a saw tooth wave.
Instead of a sharp saw tooth,
we use an extreme asymmetric triangle.
This is a poor man's band-limiting approach
that shall reduce aliasing at high oscillation frequencies.
We should really work on band-limited oscillators,
but this is hard in the general case.
-}
saw :: IO ExitCode
saw =
   play (Osci.static (Wave.triangleAsymmetric 0.9) 0 (0.01::Double))

{- |
When we apply a third power to each value of the saw tooths
we get an oscillator with cubic polynomial functions as waveform.
The distortion function applied to a saw wave can be used
to turn every function on the interval [-1,1] into a waveform.
-}
cubic :: IO ExitCode
cubic =
   play (Osci.static (Wave.distort (^3) Wave.saw) 0 (0.01::Double))

{- |
Now let's start with modulated tones.
The first simple example is changing the degree of asymmetry
according to a slow oscillator (LFO = low frequency oscillator).
-}
sawMorph :: IO ExitCode
sawMorph =
   play (Osci.shapeMod Wave.triangleAsymmetric 0 (0.01::Double) (Osci.static Wave.sine 0 (0.00001::Double)))

{- |
It's also very common to modulate the frequency of a tone.
-}
laser :: IO ExitCode
laser =
   play (Osci.freqMod Wave.saw 0 $ map (\f -> 0.02+0.01*f) $ Osci.static Wave.saw 0 (0.0001::Double))

pingSig :: Sig.T Double
pingSig =
   Filt.envelope (Ctrl.exponential 50000 1) (Osci.static Wave.sine 0 (0.01::Double))

{- |
A simple sine wave with exponentially decaying amplitude.
-}
ping :: IO ExitCode
ping =
   play pingSig

{- |
The 'ping' sound can also be used
to modulate the phase another oscillator.
This is a well-known effect used excessively in FM synthesis,
that was introduced by the Yamaha DX-7 synthesizer.
-}
fmPing :: IO ExitCode
fmPing =
   play (Osci.phaseMod Wave.sine (0.01::Double) $ map (2*) pingSig)

{- |
One of the most impressive sounds effects is certainly frequency filtering,
especially when the filter parameters are modulated.
In this example we use a resonant lowpass
whose resonance frequency is controlled by a slow sine wave.
The frequency filters usually use internal filter parameters
that are not very intuitive to use directly.
Thus we apply a function (here 'UniFilter.parameter')
in order to turn the intuitive parameters \"resonance frequency\" and \"resonance\"
(resonance frequency amplification while frequency zero is left unchanged)
into internal filter parameters.
We have not merged these two steps
since the computation of internal filter parameters
is more expensive then the filtering itself
and you may want to reduce the computation
by computing the internal filter parameters at a low sample rate
and interpolate them.
However, in the list implementation
this will not save you much time, if at all,
since the list operations are too expensive.

Now this is the example where my machine is no longer able to produce
a constant audio stream in real-time.
For tackling this problem, please continue with "Synthesizer.Generic.Tutorial".
-}
filterSaw :: IO ExitCode
filterSaw =
   play (map UniFilter.lowpass $ UniFilter.run (map (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.04+0.02*f)) $ Osci.static Wave.sine 0 (0.00001::Double)) $ Osci.static Wave.saw 0 (0.002::Double))
