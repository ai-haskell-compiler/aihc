module Numeric.Probability.Example.Barber where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Example.Queuing
   (Time, System, unit, evalSystem, idleAvgP, waiting)

import Numeric.Probability.Percentage
   (Dist, RDist, Trans, )

{- no Random instance for Rational
type Probability = Rational
type Dist a  = Dist.T  Probability a
type RDist a = Rnd.Distribution Probability a
type Trans a = Transition    Probability a
-}


-- * barber shop

custServ :: Dist Time
custServ = Dist.normal [5..10]

nextCust :: Trans Time -- not dependant on serving time
nextCust _ = Dist.normal [3..6]

barbers :: Int
barbers = 1

customers :: Int
customers = 20

runs :: Int
runs = 50

barberEvent :: ((), (Dist Time, Time -> Dist Time))
barberEvent =  unit (custServ, nextCust)

barberEvents :: [((), (Dist Time, Time -> Dist Time))]
barberEvents = replicate customers barberEvent

barberSystem :: (Ord b) => (System () -> b) -> RDist b
barberSystem eval = evalSystem runs barbers barberEvents eval


-- * category

data Category = ThreeOrLess | FourToTen | MoreThanTen
	deriving (Eq,Ord,Show)

cat :: Time -> Category
cat n | n <= 3 = ThreeOrLess
cat n | n <= 10 = FourToTen
cat _ = MoreThanTen

perc :: Float -> String
perc n | n <= 0.25 = "0% to 25%"
perc n | n <= 0.5 = "25% to 50%"
perc n | n <= 0.75 = "50% to 75%"
perc _ = "75% to 100%"

-- * evaluation

-- | avg barber idle time
barberIdle :: RDist String
barberIdle = barberSystem (perc . idleAvgP barbers)

-- | avg customer waiting time (unserved customers)
customerWait :: RDist Category
customerWait = barberSystem (cat . (`div` customers) . waiting barbers)
