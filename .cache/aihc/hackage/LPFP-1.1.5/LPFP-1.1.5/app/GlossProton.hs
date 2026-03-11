{-# OPTIONS -Wall #-}

import LPFP.Mechanics3D
    ( simulateGloss
    , twoProtInitial, twoProtPicture, twoProtUpdate
    )

main :: IO ()
main = simulateGloss 1e-8 20
       twoProtInitial twoProtPicture twoProtUpdate
