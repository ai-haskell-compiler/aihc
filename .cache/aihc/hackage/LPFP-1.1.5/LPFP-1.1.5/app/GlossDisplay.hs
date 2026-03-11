{-# OPTIONS -Wall #-}

import Graphics.Gloss

displayMode :: Display
displayMode = InWindow "Axes" (1000, 700) (10, 10)

axes :: Picture
axes = Pictures [Color red   $ Line [(0,0),(100,  0)]
                ,Color green $ Line [(0,0),(  0,100)]
                ]

main :: IO ()
main = display displayMode black axes
