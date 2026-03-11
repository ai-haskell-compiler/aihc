module Numeric.Probability.Example.Alarm where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??), (?=<<), )


type Probability = Rational
type Dist a = Dist.T Probability a
type PBool  = Dist.T Probability Bool


flp :: Probability -> PBool
flp p = Dist.choose p True False


-- * Numeric.Probability.Example.Alarm network

-- | prior burglary 1%
b :: PBool
b = flp 0.01

-- | prior earthquake 0.1%
e :: PBool
e = flp 0.001

-- | conditional probability of alarm given burglary and earthquake
a :: Bool -> Bool -> PBool
a b0 e0 =
   case (b0,e0) of
      (False,False) -> flp 0.01
      (False,True)  -> flp 0.1
      (True,False)  -> flp 0.7
      (True,True)   -> flp 0.8


-- | conditional probability of john calling given alarm
j :: Bool -> PBool
j a0 = if a0 then flp 0.8 else flp 0.05

-- | conditional probability of mary calling given alarm
m :: Bool -> PBool
m a0 = if a0 then flp 0.9 else flp 0.1

-- | calculate the full joint distribution
data Burglary = B { 	burglary :: Bool,
			earthquake :: Bool,
			alarm :: Bool,
			john :: Bool,
			mary :: Bool }
	deriving (Eq, Ord, Show)

bJoint :: Dist Burglary
bJoint = do b' <- b 		-- burglary
            e' <- e 		-- earthquake
            a' <- a b' e' 	-- alarm
	    j' <- j a' 		-- john
	    m' <- m a' 		-- mary
	    return (B b' e' a' j' m')

-- | what is the probability that mary calls given that john calls?
pmj :: Probability
pmj = mary ?? john ?=<< bJoint
