{-# OPTIONS -Wall #-}

import Vis

type R = Double

blueCube :: VisObject R
blueCube = Cube 1 Solid blue

main :: IO ()
main = display defaultOpts blueCube
