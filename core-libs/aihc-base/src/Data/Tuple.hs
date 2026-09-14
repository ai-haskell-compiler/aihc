module Data.Tuple
  ( Solo (MkSolo, Solo),
    getSolo,
    fst,
    snd,
    curry,
    uncurry,
    swap,
  )
where

import GHC.Tuple (Solo (MkSolo, Solo), getSolo)
import Prelude (curry, fst, snd, uncurry)

swap :: (a, b) -> (b, a)
swap (left, right) = (right, left)
