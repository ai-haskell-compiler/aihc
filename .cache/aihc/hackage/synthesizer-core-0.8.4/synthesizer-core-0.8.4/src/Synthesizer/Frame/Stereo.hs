module Synthesizer.Frame.Stereo (
   T, left, right, cons, map, swap,
   arrowFromMono, arrowFromMonoControlled, arrowFromChannels,
   Stereo.Channel(Left, Right), Stereo.select,
   Stereo.interleave,
   Stereo.sequence,
   Stereo.liftApplicative,
   ) where

import Sound.Frame.NumericPrelude.Stereo as Stereo
import Control.Arrow (Arrow, (^<<), (<<^), (&&&), )
import Data.Tuple.HT (mapSnd, )

import Prelude hiding (map, )


{- |
Run a causal process independently on each stereo channel.
-}
arrowFromMono ::
   (Arrow arrow) =>
   arrow a b -> arrow (Stereo.T a) (Stereo.T b)
arrowFromMono proc =
   uncurry Stereo.cons ^<<
   (proc<<^Stereo.left) &&& (proc<<^Stereo.right)

arrowFromMonoControlled ::
   (Arrow arrow) =>
   arrow (c,a) b -> arrow (c, Stereo.T a) (Stereo.T b)
arrowFromMonoControlled proc =
   uncurry Stereo.cons ^<<
   (proc <<^ mapSnd Stereo.left) &&& (proc <<^ mapSnd Stereo.right)

arrowFromChannels ::
   (Arrow arrow) =>
   arrow a b -> arrow a b -> arrow (Stereo.T a) (Stereo.T b)
arrowFromChannels leftChan rightChan =
   uncurry Stereo.cons ^<<
   (leftChan<<^Stereo.left) &&& (rightChan<<^Stereo.right)
