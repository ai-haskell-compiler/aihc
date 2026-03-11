{-# OPTIONS -Wall #-}

import Vis

type State = (Int,[Float])

-- seconds / update
dt :: Double
dt = 0.5

displayFunc :: State -> VisObject Double
displayFunc (n,ts) = Text2d (show n ++ " " ++ show (take 4 ts))
                     (100,100) Fixed9By15 orange

updateFunc :: Float -> State -> State
updateFunc t (n,ts) = (n+1,t:ts)

main :: IO ()
main = simulate defaultOpts dt (0,[]) displayFunc updateFunc
