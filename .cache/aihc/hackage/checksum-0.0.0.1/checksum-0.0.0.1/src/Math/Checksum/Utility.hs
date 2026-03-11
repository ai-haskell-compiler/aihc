module Math.Checksum.Utility where

import qualified Control.Monad.Exception.Synchronous as ME
import Control.Monad.Exception.Synchronous (Exceptional(Success), throw)

import Data.Ix (inRange, index)

import Data.List (unfoldr)
import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (swap)


decomposePositional :: Integral a => a -> a -> [a]
decomposePositional b =
   unfoldr (\n -> toMaybe (n/=0) $ swap (divMod n b))

intFromDigit :: Char -> Exceptional String Int
intFromDigit c =
   if inRange ('0','9') c
      then Success $ index ('0','9') c
      else throw $ "not a digit: " ++ [c]

processValid :: Exceptional String Bool -> Maybe String
processValid =
   ME.switch Just (\b -> toMaybe (not b) "check sum does not match")
