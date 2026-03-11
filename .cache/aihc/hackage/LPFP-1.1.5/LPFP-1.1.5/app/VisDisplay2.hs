{-# OPTIONS -Wall #-}

import Vis
import Linear

type R = Double

axes :: VisObject R
axes = VisObjects [Line Nothing [V3 0 0 0, V3 1 0 0] red
                  ,Line Nothing [V3 0 0 0, V3 0 1 0] green
                  ,Line Nothing [V3 0 0 0, V3 0 0 1] blue
                  ]

main :: IO ()
main = display defaultOpts axes
