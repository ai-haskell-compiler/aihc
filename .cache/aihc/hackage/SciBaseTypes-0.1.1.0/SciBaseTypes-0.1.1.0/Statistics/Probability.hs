
-- | Probability-related types.
--
-- TODO instances for serialization and further stuff
-- TODO vector instances

module Statistics.Probability where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.Char (chr)
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed (Unbox)
import GHC.Generics(Generic)
import Numeric.Log

import Algebra.Structure.Semiring
import Numeric.LogDomain
import Numeric.Limits



data IsNormalized = Normalized | NotNormalized



-- * Probability in linear space

-- | @Prob@ wraps a @Double@ that encodes probabilities. If @Prob@ is tagged as
-- @Normalized@, the contained values are in the range @[0,...,1]@, otherwise
-- they are in the range @[0,...,∞]@.

newtype Probability (n ∷ IsNormalized) x = Prob { getProb ∷ x }
  deriving (Eq,Ord,Show,Read,Generic)

instance (NFData x) ⇒ NFData (Probability n x)

derivingUnbox "Probability"
  [t| forall n x. Unbox x ⇒ Probability n x → x |]  [| getProb |]  [| Prob |]

deriving instance (Enum       x) ⇒ Enum       (Probability n x)
deriving instance (Num        x) ⇒ Num        (Probability n x)
deriving instance (Fractional x) ⇒ Fractional (Probability n x)
deriving instance (Floating   x) ⇒ Floating   (Probability n x)
deriving instance (Real       x) ⇒ Real       (Probability n x)
deriving instance (RealFrac   x) ⇒ RealFrac   (Probability n x)
deriving instance (RealFloat  x) ⇒ RealFloat  (Probability n x)

instance ToJSON x ⇒ ToJSON (Probability n x) where
  toJSON = toJSON . getProb

instance FromJSON x ⇒ FromJSON (Probability n x) where
  parseJSON = fmap Prob . parseJSON

instance (Num r) ⇒ Semiring (Probability n r) where
  plus  = (+)
  times = (*)
  zero = 0
  one  = 1
  {-# Inline plus  #-}
  {-# Inline times #-}
  {-# Inline zero  #-}
  {-# Inline one   #-}

-- | Turns a value into a normalized probability. @error@ if the value is not
-- in the range @[0,...,1]@.

prob ∷ (Ord x, Num x, Show x) ⇒ x → Probability Normalized x
prob x
  | x >= 0 && x <= 1 = Prob x
  | otherwise        = error $ show x ++ " not in range of [0,...,1]"
{-# Inline prob #-}

-- | Simple wrapper around @Probability@ that fixes non-normalization.

prob' ∷ (Ord x, Num x, Show x) ⇒ x → Probability NotNormalized x
prob' = Prob
{-# Inline prob' #-}

-- | This simple function represents probabilities with characters between '0'
-- @0.0 -- 0.05@ up to '9' @0.85 -- 0.95@ and finally '*' for @>0.95@.

probabilityToChar ∷ (Num k, RealFrac k) ⇒ Probability Normalized k → Char
probabilityToChar (Prob p')
  | i >= 10 = '*'
  | otherwise = chr $ 48 + i
  where p = max 0.0 $ min p' 1.0
        i = round $ p * 10


-- -- * Probability in log space. A number of operations internally cast to @Log@
-- -- from @log-domain@, but the values themselves are *not* newtype-wrapped @Log
-- -- x@ values. This is because we want to be *explicit* that these are
-- -- log-probabilities.
-- --
-- -- @Log@ numbers in cases like @fromIntegral 1 :: Log Double@ are treated as
-- -- not being in the log-domain, hence @fromIntegral performs a @log@
-- -- operations.
-- 
-- newtype LogProb (n ∷ IsNormalized) x = LogProb { getLogProb ∷ x }
--   deriving (Eq,Ord,Show)
-- 
-- derivingUnbox "LogProb"
--   [t| forall n x. Unbox x ⇒ LogProb n x → x |]  [| getLogProb |]  [| LogProb |]
-- 
-- instance (Precise x, RealFloat x) ⇒ Num (LogProb n x) where
--   (+) = withLog2 (+)
--   {-# Inline (+) #-}
--   (*) = withLog2 (*)
--   {-# Inline (*) #-}
--   abs = withLog1 abs
--   {-# Inline abs #-}
--   signum = withLog1 signum
--   {-# Inline signum #-}
--   fromInteger = LogProb . fromInteger
--   {-# Inline fromInteger #-}
--   negate = withLog1 negate
--   {-# Inline negate #-}
--   (-) = withLog2 (-)
--   {-# Inline (-) #-}
-- 
-- instance (Precise r, RealFloat r, Num r) ⇒ SemiRing (LogProb n r) where
--   srplus = (+)
--   {-# Inline srplus #-}
--   srmul  = (*)
--   {-# Inline srmul #-}
--   srzero = 0
--   {-# Inline srzero #-}
--   srone  = 1
--   {-# Inline srone #-}
-- 
-- instance (Num d, Fractional d) ⇒ NumericLimits (LogProb n d) where
--   minFinite = LogProb 0
--   maxFinite = LogProb (1/0)
-- 
-- withLog1 ∷ (Log x → Log y) → LogProb n x → LogProb n y
-- withLog1 op (LogProb x) = LogProb . ln $ op (Exp x)
-- {-# Inline withLog1 #-}
-- 
-- withLog2 ∷ (Log x → Log y → Log z) → LogProb n x → LogProb n y → LogProb n z
-- withLog2 op (LogProb x) (LogProb y) = LogProb . ln $ op (Exp x) (Exp y)
-- {-# Inline withLog2 #-}
-- 
-- 
-- -- * Conversion between probability in linear and log-space.
-- 
-- -- | Turn probability into log-probability.
-- 
-- p2lp ∷ (Floating x) ⇒ Prob n x → LogProb n x
-- p2lp (Prob x) = LogProb $ log x
-- {-# Inline p2lp #-}
-- 
-- -- | Turn log-probability into probability.
-- 
-- lp2p ∷ (Floating x) ⇒ LogProb n x → Prob n x
-- lp2p (LogProb x) = Prob $ exp x
-- {-# Inline lp2p #-}
-- 
-- -- | An isomorphism between @Prob@ and @LogProb@.
-- 
-- aslp ∷ (Floating x) ⇒ Iso' (Prob n x) (LogProb n x)
-- aslp = iso p2lp lp2p
-- {-# Inline aslp #-}

