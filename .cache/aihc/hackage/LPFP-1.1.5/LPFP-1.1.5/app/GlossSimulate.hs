{-# OPTIONS -Wall #-}

import Graphics.Gloss

displayMode :: Display
displayMode = InWindow "My Window" (1000, 700) (10, 10)

-- updates per second of real time
rate :: Int
rate = 2

disk :: Float -> Picture
disk radius = ThickCircle (radius / 2) radius

redDisk :: Picture
redDisk = Color red (disk 25)

type State = (Float,Float)

initialState :: State
initialState = (0,0)

displayFunc :: State -> Picture
displayFunc (x,y) = Translate x y redDisk

updateFunc :: Float -> State -> State
updateFunc dt (x,y) = (x + 10 * dt, y - 5 * dt)

main :: IO ()
main = simulate displayMode black rate initialState displayFunc
       (\_ -> updateFunc)
