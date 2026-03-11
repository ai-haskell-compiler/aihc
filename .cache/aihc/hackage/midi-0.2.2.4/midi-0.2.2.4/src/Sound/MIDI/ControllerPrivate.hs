module Sound.MIDI.ControllerPrivate where

import Data.Ix (Ix)
import Sound.MIDI.Utility (checkRange,
          enumRandomR, boundedEnumRandom, chooseEnum, )

import Test.QuickCheck (Arbitrary(arbitrary), )
import System.Random (Random(random, randomR), )



{- |
We do not define 'Controller' as enumeration with many constructors,
because some controllers have multiple names and some are undefined.
It is also more efficient this way.
Thus you cannot use @case@ for processing controller types,
but you can use 'Data.List.lookup' instead.

> maybe (putStrLn "unsupported controller") putStrLn $
> lookup ctrl $
>    (portamento, "portamento") :
>    (modulation, "modulation") :
>    []

-}
newtype Controller = Controller {fromController :: Int} deriving (Show, Eq, Ord, Ix)

toController :: Int -> Controller
toController = checkRange "Controller" Controller


instance Random Controller where
   random  = boundedEnumRandom
   randomR = enumRandomR

instance Arbitrary Controller where
   arbitrary = chooseEnum


instance Enum Controller where
   toEnum   = toController
   fromEnum = fromController

instance Bounded Controller where
   minBound = Controller   0
   maxBound = Controller 119
   -- higher controller numbers have special meanings
