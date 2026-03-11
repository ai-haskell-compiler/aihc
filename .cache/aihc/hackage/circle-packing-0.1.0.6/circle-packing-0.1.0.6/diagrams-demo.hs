import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Core.Envelope

import Optimisation.CirclePacking

colorize = zipWith fc $
    cycle [red,blue,yellow,magenta,cyan,bisque,firebrick,indigo]

objects = colorize $
    [ circle r  | r <- [0.1,0.2..1.6] ] ++
    [ hexagon r | r <- [0.1,0.2..0.7] ] ++
    [ decagon r | r <- [0.1,0.2..0.7] ]

-- Just a approximation, diagram objects do not have an exact radius
radiusApproximation :: Diagram b R2 -> Double
radiusApproximation o = maximum [ envelopeS (e (CircleFrac alpha)) o | alpha <- [0,0.1..1.0]]

main = defaultMain $
    position $ map (\(o,(x,y)) -> (p2 (x,y),o)) $
    packCircles radiusApproximation objects
