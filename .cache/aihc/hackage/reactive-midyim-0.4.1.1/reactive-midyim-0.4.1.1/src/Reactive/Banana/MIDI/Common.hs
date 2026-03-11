module Reactive.Banana.MIDI.Common where

import qualified Reactive.Banana.MIDI.Time as Time

import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import Sound.MIDI.Message.Channel (Channel, )
import Sound.MIDI.Message.Channel.Voice (Velocity, Pitch, Controller, Program, )

import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Numeric.NonNegative.Class as NonNeg

import Data.Monoid (mempty, )



-- * Constructors

channel :: Int -> Channel
channel = ChannelMsg.toChannel

pitch :: Int -> Pitch
pitch = VoiceMsg.toPitch

velocity :: Int -> Velocity
velocity = VoiceMsg.toVelocity

controller :: Int -> Controller
controller = VoiceMsg.toController

program :: Int -> Program
program = VoiceMsg.toProgram


normalVelocity :: Velocity
normalVelocity = VoiceMsg.normalVelocity



-- * Fractions

-- | properFraction is useless for negative numbers
splitFraction :: (RealFrac a) => a -> (Int, a)
splitFraction x =
   case floor x of
      n -> (n, x - fromIntegral n)


fraction :: RealFrac a => a -> a
fraction x =
   x - fromIntegral (floor x :: Integer)


-- * Notes

{-
The Ord instance is intended for use in a Map,
but it shall not express a notion of magnitude.
-}
data PitchChannel =
     PitchChannel Pitch Channel
   deriving (Eq, Ord, Show)

data PitchChannelVelocity =
     PitchChannelVelocity PitchChannel Velocity
   deriving (Eq, Show)


class VelocityField x where
   getVelocity :: x -> Velocity

instance VelocityField Velocity where
   getVelocity = id



-- * time stamped objects

{- |
The times are relative to the start time of the bundle
and do not need to be ordered.
-}
data Future m a = Future {futureTime :: Time.T m Time.Relative Time.Ticks, futureData :: a}
type Bundle m a = [Future m a]

singletonBundle :: a -> Bundle m a
singletonBundle ev = [now ev]

immediateBundle :: [a] -> Bundle m a
immediateBundle = map now

now :: a -> Future m a
now = Future mempty

instance Functor (Future m) where
   fmap f (Future dt a) = Future dt $ f a



-- * event list support

mergeStable ::
   (NonNeg.C time) =>
   EventList.T time body ->
   EventList.T time body ->
   EventList.T time body
mergeStable =
   EventList.mergeBy (\_ _ -> True)

mergeEither ::
   (NonNeg.C time) =>
   EventList.T time a ->
   EventList.T time b ->
   EventList.T time (Either a b)
mergeEither xs ys =
   mergeStable (fmap Left xs) (fmap Right ys)
