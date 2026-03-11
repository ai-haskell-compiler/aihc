{-# LANGUAGE FlexibleContexts #-}
{- |
In this module we demonstrate techniques for getting sound in real-time.
Getting real-time performance is mostly an issue of the right signal data structure.
However, there is no one-size-fits-all data structure.
For choosing the right one, you need to understand how various data structures work,
what are their strengths and what are their weaknesses.
-}
module Synthesizer.Generic.Tutorial
{-# DEPRECATED "do not import that module, it is only intended for demonstration" #-}
 where

import qualified Synthesizer.Plain.Tutorial as Tutorial -- needed for Haddock

import qualified Sound.Sox.Play as Play
import qualified Sound.Sox.Write as Write
import qualified Sound.Sox.Option.Format as SoxOpt
import qualified Synthesizer.Basic.Binary as BinSmp
import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.State.Signal as Sig
import qualified Synthesizer.Causal.Process as Causal
import Control.Arrow ((&&&), (^<<), (<<^), (<<<), )

import qualified Synthesizer.Generic.Oscillator as Osci
import qualified Synthesizer.Generic.Piece as Piece
import qualified Synthesizer.Generic.Filter.NonRecursive as Filt
import qualified Synthesizer.Plain.Filter.Recursive as FiltRec
import qualified Synthesizer.Plain.Filter.Recursive.Universal as UniFilter
import qualified Synthesizer.Basic.Wave as Wave
import Synthesizer.Piecewise ((#|-), (-|#), (#|), (|#), )

import qualified Synthesizer.State.Control as CtrlS
import qualified Synthesizer.State.Oscillator as OsciS

import System.Exit (ExitCode, )
import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


{- |
First, we define a play routine for lazy storable vectors.
Storable lazy vectors are lazy lists of low-level arrays.
They are both efficient in time and memory consumption,
but the blocks disallow feedback by small delays.
Elements of a storable vector must be of type class Storable.
This means that elements must have fixed size
and advanced data types like functions cannot be used.
-}
play :: SigSt.T Double -> IO ExitCode
play =
   Play.simple SigSt.hPut SoxOpt.none 44100 .
   SigSt.map BinSmp.int16FromDouble

{- |
Here is a simple oscillator generated as lazy storable vector.
An oscillator is a signal generator,
that is it produces a signal
without consuming other signals that correspond in time.
Signal generators have the maximal block size as parameter.
This is the lower limit of possible feedback delays.
-}
oscillator :: IO ExitCode
oscillator =
   play (Osci.static SigG.defaultLazySize Wave.sine zero (0.01::Double))


{- |
A routine just for the case that we want to post-process a signal somewhere else.
-}
write :: FilePath -> SigSt.T Double -> IO ExitCode
write name =
   Write.simple SigSt.hPut SoxOpt.none name 44100 .
   SigSt.map BinSmp.int16FromDouble

{- |
The simple brass sound demonstrates
how to generate piecewise defined curves.
Some infix operators are used in order to make the pieces fit in height.
There are also operators for intended jumps.
-}
brass :: IO ExitCode
brass =
--   write "brass.aiff" $
   play $
   Filt.envelope
      (Piece.run SigG.defaultLazySize $
       0    |# ( 3000, Piece.cubic 0.002 0) #|-
       0.7 -|# (50000, Piece.step) #|-
       0.7 -|# (10000, Piece.exponential 0) #| (0.01::Double)) $
   SigG.fromState SigG.defaultLazySize $
   Filt.amplify 0.5 $
   SigG.mix
      (OsciS.static Wave.saw zero (0.00499::Double))
      (OsciS.static Wave.saw zero (0.00501::Double))


{- |
We rewrite the filter example 'Tutorial.filterSaw'
in terms of type classes for more signal types.
The constraints become quite large
because we must assert, that a particular sample type
can be used in the addressed signal type.
-}
filterSawSig ::
   (SigG.Write sig Double,
    SigG.Transform sig (UniFilter.Result Double),
    SigG.Transform sig (UniFilter.Parameter Double)) =>
   sig Double
filterSawSig =
   SigG.map UniFilter.lowpass $ SigG.modifyModulated UniFilter.modifier (SigG.map (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.04+0.02*f)) $ Osci.static SigG.defaultLazySize Wave.sine zero (0.00001::Double)) $ Osci.static SigG.defaultLazySize Wave.saw zero (0.002::Double)

{- |
Here we instantiate 'filterSawSig' for storable vectors and play it.
This means that all operations convert a storable vector into another storable vector.
While every single operation probably is as efficient as possible,
the composition of all those processes could be more efficient.
So keep on reading.
-}
filterSaw :: IO ExitCode
filterSaw =
   play filterSawSig


{- |
The next signal type we want to consider is the stateful signal generator.
It is not a common data structure, where the sample values are materialized.
Instead it is a description of how to generate sample values iteratively.
This is almost identical to the @Data.Stream@ module from the @stream-fusion@ package.
With respect to laziness and restrictions of the sample type (namely none),
this signal representation is equivalent to lists.
You can convert one into the other in a lossless way.
That is, function as sample type is possible.
Combination of such signal generators is easily possible
and does not require temporary storage,
because this signal representation needs no sample value storage at all.
However at the end of such processes, the signal must be materialized.
Here we write the result into a lazy storable vector and play that.
What the compiler actually does is to create a single loop,
that generates the storable vector to be played in one go.
-}
playState :: Sig.T Double -> IO ExitCode
playState =
   Play.simple SigSt.hPut SoxOpt.none 44100 .
   SigG.fromState SigG.defaultLazySize .
   Sig.map BinSmp.int16FromDouble

{- |
We demonstrate the stateful signal generator using the known 'filterSaw' example.
Actually we can reuse the code from above,
because the signal generator is also an instance of the generic signal class.
-}
filterSawState :: IO ExitCode
filterSawState =
   playState filterSawSig


{- |
Merging subsequent signal processes based on signal generators
into an efficient large signal processor is easy.
Not storing intermediate results is however a problem in another situation:
Sometimes you want to share one signal between several processes.
-}
filterPingStateProc :: Sig.T Double -> Sig.T Double
filterPingStateProc env =
   Filt.envelope env $ Sig.map UniFilter.lowpass $ Sig.modifyModulated UniFilter.modifier (Sig.map (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.03*f)) $ env) $ OsciS.static Wave.saw zero (0.002::Double)

{- |
In the following example we generate an exponential curve
which shall be used both as envelope
and as resonance frequency control of a resonant lowpass.
Actually, recomputing an exponential curve is not an issue,
since it only needs one multiplication per sample.
But it is simple enough to demonstrate the problem and its solutions.
The expression @let env = exponential2 50000 1@ fools the reader of the program,
since the @env@ that is shared, is only the signal generator,
that is, the description of how to compute the exponential curve successively.
That is wherever a signal process reads @env@, it is computed again.
-}
filterPingState :: IO ExitCode
filterPingState =
   playState $
   filterPingStateProc $
   CtrlS.exponential2 50000 1

{- |
You can achieve sharing by a very simple way.
You can write the result of the signal generator in a list ('Sig.toList')
and use this list as source for a new generator ('Sig.fromList').
'Sig.fromList' provides a signal generator that generates new sample values
by delivering the next sample from the list.

In a real world implementation you would move
the @Sig.fromList . Sig.toList@ to 'filterPingStateProc',
since the caller cannot know, that this function uses the signal twice,
and the implementor of 'filterPingStateProc' cannot know,
how expensive the computation of @env@ is.

You can use any other signal type for sharing, e.g. storable vectors,
but whatever type you choose, you also get its disadvantages.
Namely, storable vectors only work for storable samples
and lists are generally slow,
and they also cannot be optimized away,
since this only works, when no sharing is required.

Whenever a signal is shared as input between several signal processes,
the actual materialized data is that
between the slowest and the fastest reading process.
This is due to lazy evaluation and garbage collection.
If the different readers read with different speed,
then you will certainly need a temporary sample storage.
-}
filterPingShare :: IO ExitCode
filterPingShare =
   playState $
   filterPingStateProc $
   Sig.fromList $ Sig.toList $ CtrlS.exponential2 50000 1

{- |
It is however not uncommon that all readers read with the same speed.
In this case we would in principle only need to share the input signal per sample.
This way we would not need a data structure
for storing a sub-sequence of samples temporarily.
But how to do that practically?

The solution is not to think in terms of signals and signal processors,
e.g. @Sig.T a@ and @Sig.T a -> Sig.T b -> Sig.T c@, respectively,
but in terms of signal processors, that are guaranteed to run in sync.
That is we must assert that signal processors
process the samples in chronological order and emit one sample per input sample.
We call such processes \"causal\" processes.
For example @Causal.T (a,b) c@ represents the function @Sig.T (a,b) -> Sig.T c@
but it also carries the guarantee,
that for each input of type @(a,b)@
one sample of type @c@ is emitted or the output terminates.
Internally it is the Kleisli arrow of the @StateT Maybe@ monad.

Another important application of the Causal arrow is feedback.
Using causal processes guarantees, that a process cannot read ahead,
such that it runs into future data, which does still not exist due to recursion.

Programming with arrows needs a bit experience or Haskell extensions.
Haskell extensions are either an @Arrow@ syntax preprocessor
or the preprocessor that is built into GHC.
However, for computing with physical dimensions
you can no longer use the original @Arrow@ class
and thus you cannot use the arrow syntax.
So here is an example of how to program 'filterPingShare'
using @Arrow@ combinators.
-}
filterPingCausal :: IO ExitCode
filterPingCausal =
   playState $
   let proc =
          uncurry (*) ^<<
          ((UniFilter.lowpass ^<<
            UniFilter.causal <<<
            Causal.feedSnd (OsciS.static Wave.saw zero (0.002::Double)) <<^
            (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.03*f)))
           &&&
           Causal.id)
   in  Causal.apply proc $ CtrlS.exponential2 50000 1
