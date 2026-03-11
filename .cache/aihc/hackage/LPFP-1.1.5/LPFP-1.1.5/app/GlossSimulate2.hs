{-# OPTIONS -Wall #-}

import Graphics.Gloss

displayMode :: Display
displayMode = InWindow "My Window" (1000, 700) (10, 10)

-- updates per second of real time
rate :: Int
rate = 24

disk :: Float -> Picture
disk radius = ThickCircle (radius / 2) radius

redDisk :: Picture
redDisk = Color red (disk 25)

type Position = (Float,Float)
type Velocity = (Float,Float)
type State = (Position,Velocity)

initialState :: State
initialState = ((0,0),(40,80))

displayFunc :: State -> Picture
displayFunc ((x,y),_) = Translate x y redDisk

updateFunc :: Float -> State -> State
updateFunc dt ((x,y),(vx,vy))
    = (( x + vx * dt, y +  vy * dt)
      ,(vx          ,vy - 9.8 * dt))

main :: IO ()
main = simulate displayMode black rate initialState displayFunc
       (\_ -> updateFunc)
