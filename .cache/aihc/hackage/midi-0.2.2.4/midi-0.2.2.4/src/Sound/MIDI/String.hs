{- |
Taken from Haskore.
-}

module Sound.MIDI.String where

import Control.Monad.Trans.State (State, runState)

unlinesS :: [ShowS] -> ShowS
unlinesS = concatS . map (. showString "\n")

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

rightS, leftS, centreS :: Int -> ShowS -> ShowS
rightS  n s = showString (right  n (s ""))
leftS   n s = showString (left   n (s ""))
centreS n s = showString (centre n (s ""))

right,left, centre :: Int -> String -> String
right  n s = spaces (n - length s) ++ s
left   n s = s ++ spaces (n - length s)
centre n s = spaces l ++ s ++ spaces (n'-l)
  where
    n' = n - length s
    l  = n' `div` 2

spaces :: Int -> String
spaces n = replicate (max 0 n) ' '

stateToReadS :: State String a -> ReadS a
stateToReadS state string =
   [runState state string]
