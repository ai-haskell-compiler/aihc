{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFP.SimpleVec
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 10 of the book Learn Physics with Functional Programming
-}

module LPFP.SimpleVec where

infixl 6 ^+^
infixl 6 ^-^
infixr 7 *^
infixl 7 ^*
infixr 7 ^/
infixr 7 <.>
infixl 7 ><

-- | A vector derivative takes a vector-valued function of a real variable (usually time) as input,
--   and produces a vector-valued function of a real variable as output.
type VecDerivative = (R -> Vec) -> R -> Vec

-- | Given a step size, calculate the vector derivative of a vector-valued function of a real variable
--   (usually time).
vecDerivative :: R -> VecDerivative
vecDerivative dt v t = (v (t + dt/2) ^-^ v (t - dt/2)) ^/ dt

v1 :: R -> Vec
v1 t = 2 *^ t**2 *^ iHat ^+^ 3 *^ t**3 *^ jHat ^+^ t**4 *^ kHat

xCompFunc :: (R -> Vec) -> R -> R
xCompFunc v t = xComp (v t)

-- | A derivative takes a real-valued function of a real variable (often time) as input,
--   and produces a real-valued function of a real variable as output.
type Derivative = (R -> R) -> R -> R

-- | Given a step size, calculate the derivative of a real-valued function of a real variable
--   (often time).
derivative :: R -> Derivative
derivative dt x t = (x (t + dt/2) - x (t - dt/2)) / dt

-- | Time is a real number.
type Time         = R
-- | The position of a particle can be represented as a vector.
type PosVec       = Vec
-- | Velocity is a vector.
type Velocity     = Vec
-- | Acceleration is a vector.
type Acceleration = Vec

-- | Given a time step and a position function, return a velocity function.
velFromPos :: R                   -- ^ dt
           -> (Time -> PosVec  )  -- ^ position function
           -> (Time -> Velocity)  -- ^ velocity function
velFromPos = vecDerivative

-- | Given a time step and a velocity function, return an acceleration function.
accFromVel :: R                       -- dt
           -> (Time -> Velocity)      -- velocity function
           -> (Time -> Acceleration)  -- acceleration function
accFromVel = vecDerivative

-- | Given initial position and a constant velocity, return a position function.
positionCV :: PosVec -> Velocity -> Time -> PosVec
positionCV r0 v0 t = v0 ^* t ^+^ r0

-- | Given initial velocity and a constant acceleration, return a velocity function.
velocityCA :: Velocity -> Acceleration -> Time -> Velocity
velocityCA v0 a0 t = a0 ^* t ^+^ v0

-- | Given initial position, initial velocity, and a constant acceleration, return a position function.
positionCA :: PosVec -> Velocity -> Acceleration
           -> Time -> PosVec
positionCA r0 v0 a0 t = 0.5 *^ t**2 *^ a0 ^+^ v0 ^* t ^+^ r0

-- | Given a nonzero velocity and an acceleration, return the component of acceleration
--   parallel to the velocity.
aParallel :: Vec -> Vec -> Vec
aParallel v a = let vHat = v ^/ magnitude v
                in (vHat <.> a) *^ vHat

-- | Given a nonzero velocity and an acceleration, return the component of acceleration
--   perpendicular to the velocity.
aPerp :: Vec -> Vec -> Vec
aPerp v a = a ^-^ aParallel v a

-- | Given velocity and acceleration, return the rate at which speed is changing.
speedRateChange :: Vec -> Vec -> R
speedRateChange v a = (v <.> a) / magnitude v

radiusOfCurvature :: Vec -> Vec -> R
radiusOfCurvature v a = (v <.> v) / magnitude (aPerp v a)

projectilePos :: PosVec -> Velocity -> Time -> PosVec
projectilePos r0 v0 = positionCA r0 v0 (9.81 *^ negateV kHat)

-- | An approximation to a real number.
type R = Double

data Mass = Mass R
            deriving (Eq,Show)

data Grade = Grade String Int
             deriving (Eq,Show)

grades :: [Grade]
grades = [Grade "Albert Einstein" 89
         ,Grade "Isaac Newton"    95
         ,Grade "Alan Turing"     91
         ]

data GradeRecord = GradeRecord { name  :: String
                               , grade :: Int
                               } deriving (Eq,Show)

gradeRecords1 :: [GradeRecord]
gradeRecords1 = [GradeRecord "Albert Einstein" 89
                ,GradeRecord "Isaac Newton"    95
                ,GradeRecord "Alan Turing"     91
                ]

gradeRecords2 :: [GradeRecord]
gradeRecords2 = [GradeRecord {name = "Albert Einstein", grade = 89}
                ,GradeRecord {name = "Isaac Newton"   , grade = 95}
                ,GradeRecord {name = "Alan Turing"    , grade = 91}
                ]

data MyBool = MyFalse | MyTrue
              deriving (Eq,Show)

data MyMaybe a = MyNothing
               | MyJust a
                deriving (Eq,Show)

-- | A type for three-dimensional vectors.
data Vec = Vec { xComp :: R  -- ^ x component of a vector
               , yComp :: R  -- ^ y component of a vector
               , zComp :: R  -- ^ z component of a vector
               } deriving (Eq)

instance Show Vec where
    show (Vec x y z) = "vec " ++ showDouble x ++ " "
                              ++ showDouble y ++ " "
                              ++ showDouble z

showDouble :: R -> String
showDouble x
    | x < 0      = "(" ++ show x ++ ")"
    | otherwise  = show x

-- | Form a vector by giving its x, y, and z components.
vec :: R  -- ^ x component
    -> R  -- ^ y component
    -> R  -- ^ z component
    -> Vec
vec = Vec

-- | A unit vector in the x direction.
iHat :: Vec
iHat = vec 1 0 0

-- | A unit vector in the y direction.
jHat :: Vec
jHat = vec 0 1 0

-- | A unit vector in the z direction.
kHat :: Vec
kHat = vec 0 0 1

-- | The zero vector.
zeroV :: Vec
zeroV = vec 0 0 0

-- | Negate a vector.
negateV :: Vec -> Vec
negateV (Vec ax ay az) = Vec (-ax) (-ay) (-az)

-- | Vector addition.
(^+^) :: Vec -> Vec -> Vec
Vec ax ay az ^+^ Vec bx by bz = Vec (ax+bx) (ay+by) (az+bz)

-- | Vector subtraction.
(^-^) :: Vec -> Vec -> Vec
Vec ax ay az ^-^ Vec bx by bz = Vec (ax-bx) (ay-by) (az-bz)

-- | Add a list of vectors.
sumV :: [Vec] -> Vec
sumV = foldr (^+^) zeroV

-- | Scalar multiplication of a number and a vector.
(*^)  :: R   -> Vec -> Vec
c *^ Vec ax ay az = Vec (c*ax) (c*ay) (c*az)

-- | Scalar multiplication of a vector and a number.
(^*)  :: Vec -> R   -> Vec
Vec ax ay az ^* c = Vec (c*ax) (c*ay) (c*az)

-- | Dot product of two vectors.
(<.>) :: Vec -> Vec -> R
Vec ax ay az <.> Vec bx by bz = ax*bx + ay*by + az*bz

-- | Cross product of two vectors.
(><)  :: Vec -> Vec -> Vec
Vec ax ay az >< Vec bx by bz
    = Vec (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

-- | Division of a vector by a number.
(^/) :: Vec -> R -> Vec
Vec ax ay az ^/ c = Vec (ax/c) (ay/c) (az/c)

-- | Magnitude of a vector.
magnitude :: Vec -> R
magnitude v = sqrt(v <.> v)

-- | Definite integral of a vector-valued function of a real number.
vecIntegral :: R           -- ^ step size dt
            -> (R -> Vec)  -- ^ vector-valued function
            -> R           -- ^ lower limit
            -> R           -- ^ upper limit
            -> Vec         -- ^ result
vecIntegral = undefined

maxHeight :: PosVec -> Velocity -> R
maxHeight = undefined

speedCA :: Velocity -> Acceleration -> Time -> R
speedCA = undefined

xyProj :: Vec -> Vec
xyProj = undefined

magAngles :: Vec -> (R,R,R)
magAngles = undefined

gEarth :: Vec
gEarth = undefined

vBall :: R -> Vec
vBall t = undefined t

speedRateChangeBall :: R -> R
speedRateChangeBall t = undefined t

rNCM :: (R, R -> R) -> R -> Vec
rNCM (radius, theta) t = undefined radius theta t

aPerpFromPosition :: R -> (R -> Vec) -> R -> Vec
aPerpFromPosition epsilon r t
    = let v = vecDerivative epsilon r
          a = vecDerivative epsilon v
      in aPerp (v t) (a t)
