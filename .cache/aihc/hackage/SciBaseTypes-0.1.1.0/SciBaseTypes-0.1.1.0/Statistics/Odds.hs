
-- | Provides newtypes for odds, log-odds, and discretized versions.
--
-- TODO This is currently quite ad-hoc and needs better formalization. In
-- particular in terms of wrapping and usage of @Num@ and @Semiring@.

module Statistics.Odds where

import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)

import Data.Info

import Algebra.Structure.Semiring
import Numeric.Discretized
import Numeric.Limits



-- | Odds.

newtype Odds = Odds { getOdds ∷ Double }
  deriving (Generic,Eq,Ord,Show,Read,Num)

instance NFData Odds

deriving newtype instance Semiring Odds



-- | Encodes log-odds that have been rounded or clamped to integral numbers.
-- One advantage this provides is more efficient "maximum/minimum" calculations
-- compared to using @Double@s.
--
-- Note that these are "explicit" log-odds. Each numeric operation uses the
-- underlying operation on @Int@. If you want automatic handling, choose @Log
-- Odds@.

newtype DiscLogOdds (t∷k) = DiscLogOdds { getDiscLogOdds ∷ Discretized t }
  deriving (Generic,Eq,Ord,Show,Read)

deriving newtype instance (Num (Discretized (t∷k))) ⇒ Num (DiscLogOdds t)
deriving newtype instance (Semiring (Discretized (t∷k))) ⇒ Semiring (DiscLogOdds t)
deriving newtype instance (Fractional (Discretized (t∷k))) ⇒ Fractional (DiscLogOdds t)
deriving newtype instance (Real (Discretized (t∷k))) ⇒ Real (DiscLogOdds (t::k))

derivingUnbox "DiscretizedLogOdds"
  [t| forall t . DiscLogOdds t → Int |]  [| getDiscretized . getDiscLogOdds |]  [| DiscLogOdds . Discretized |]

instance Binary    (DiscLogOdds t)
instance Serialize (DiscLogOdds t)
instance Hashable  (DiscLogOdds t)

instance ToJSON (Discretized t) ⇒ ToJSON (DiscLogOdds t) where
  toJSON = toJSON . getDiscLogOdds

instance FromJSON (Discretized t) ⇒ FromJSON  (DiscLogOdds t) where
  parseJSON = fmap DiscLogOdds . parseJSON

instance (NFData (Discretized t)) ⇒ NFData (DiscLogOdds t) where
  rnf (DiscLogOdds k) = rnf k
  {-# Inline rnf #-}

instance (NumericLimits (Discretized t)) ⇒ NumericLimits (DiscLogOdds t) where
  minFinite = DiscLogOdds minFinite
  {-# Inline minFinite #-}
  maxFinite = DiscLogOdds maxFinite
  {-# Inline maxFinite #-}

instance Info (DiscLogOdds t) where
  info = info . getDiscLogOdds

