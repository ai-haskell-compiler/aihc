 {-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-|
 Use @human-readable-duration@ if you want to replace
 "1002012 seconds left"
 by the easier to read:
 "11 days 14 hours 20 min 12s left"

 But also if you want to manipulate time duration easily
 by using seconds, minutes, hours and days.
 Typically if you prefer to write:

 @
 x + (5 * oneSecond) + (2 * minute) + (1 * day)
 @

 instead of

 @
 x + 5 * 2*60 * 1*60*60*24
 @
-}
module Data.Duration.Tutorial (
  -- * Introduction
  -- $introduction
  ) where

import Data.Duration


{- $introduction

So here how you use it:

 >>> humanReadableDuration 1002012.002
 "11 days 14 hours 20 min 12s 2ms"

 >>> humanReadableDuration $ (4 * hour) + (2 * minute) + (3 * ms)
 "4 hours 2 min 3ms"

 And one nice thing you could do:

 >>> let duration1 = (4 * hour) + (2 * minute) + (3 * ms)
 >>> let duration2 = (8 * hour) + (33 * minute) + (5 * oneSecond)
 >>> humanReadableDuration (4 * duration1 + 2 * duration2)
 "1 days 9 hours 14 min 10s 12ms"

 Happy hacking.
-}
