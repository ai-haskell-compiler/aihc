module Data.Tuple
  ( fst,
    snd,
    curry,
    uncurry,
    swap,
  )
where

import Prelude (curry, fst, snd, uncurry)

swap :: (a, b) -> (b, a)
swap (left, right) = (right, left)
