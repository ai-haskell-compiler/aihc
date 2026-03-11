
-- | Discretized floating point numbers, where the scaling factor is kept
-- as two phantom types denoting the rational number used for scaling.

module Numeric.Discretized where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Binary (Binary)
import Data.Coerce
import Data.Hashable (Hashable)
import Data.Proxy
import Data.Ratio
import Data.Serialize (Serialize)
import Data.Vector.Unboxed.Deriving
import Debug.Trace
import GHC.Generics
import GHC.Real (Ratio(..))
import GHC.TypeLits

import Data.Info

import Algebra.Structure.Semiring
import Numeric.Limits



-- | Some discretizations are of the type @ln 2 / 2@ (@PAM@ matrices in Blast
-- for example). Using this type, we can annotate as follows: @Discretized
-- (RTyLn 2 :% RTyId 2)@.
--
-- One may use @Unknown@ if the scale is not known. For example, the blast
-- matrices use different scales internally and one needs to read the header to
-- get the scale.

data RatioTy a = RTyExp a | RTyId a | RTyLn a | RTyPlus (RatioTy a) (RatioTy a) | RTyTimes (RatioTy a) (RatioTy a) | Unknown

class RatioTyConstant a where
  ratioTyConstant ∷ Proxy a → Ratio Integer

instance (KnownNat k, KnownNat l) ⇒ RatioTyConstant (k :% l) where
  {-# Inline ratioTyConstant #-}
  ratioTyConstant Proxy = let k = natVal @k Proxy; l = natVal @l Proxy in  k :% l

instance (KnownNat k) ⇒ RatioTyConstant (RTyExp (k∷Nat)) where
  {-# Inline ratioTyConstant #-}
  ratioTyConstant Proxy = let n = natVal @k Proxy in toRational (exp $ fromInteger n)

instance (KnownNat k) ⇒ RatioTyConstant (RTyId (k∷Nat)) where
  {-# Inline ratioTyConstant #-}
  ratioTyConstant Proxy = let n = natVal @k Proxy in toRational n

instance (KnownNat k) ⇒ RatioTyConstant (RTyLn (k∷Nat)) where
  {-# Inline ratioTyConstant #-}
  ratioTyConstant Proxy = let n = natVal @k Proxy in toRational (log $ fromInteger n)

instance (RatioTyConstant a, RatioTyConstant b) ⇒ RatioTyConstant (RTyPlus (a∷RatioTy k) (b∷RatioTy k)) where
  {-# Inline ratioTyConstant #-}
  ratioTyConstant Proxy = ratioTyConstant @a Proxy + ratioTyConstant @b Proxy

instance (RatioTyConstant a, RatioTyConstant b) ⇒ RatioTyConstant (RTyTimes (a∷RatioTy k) (b∷RatioTy k)) where
  {-# Inline ratioTyConstant #-}
  ratioTyConstant Proxy = ratioTyConstant @a Proxy * ratioTyConstant @b Proxy

-- | A discretized value takes a floating point number @n@ and produces a
-- discretized value. The actual discretization formula is given on the type
-- level, freeing us from having to carry around some scaling function.
--
-- Typically, one might use types likes @100@, @(100 :% 1)@, or @(RTyLn 2 :%
-- RTyId 2)@.
--
-- The main use of a 'Discretized' value is to enable calculations with 'Int'
-- while somewhat pretending to use floating point values.
--
-- Be careful with certain operations like @(*)@ as they will easily cause the
-- numbers to arbitrarily wrong. @(+)@ and @(-)@ are fine, however.
--
-- NOTE Export and import of data is in the form of floating points, which can
-- lead to additional loss of precision if one is careless!
--
-- TODO fast 'Show' methods required!
--
-- TODO blaze stuff?
--
-- TODO We might want to discretize @LogDomain@ style values. This requires
-- some thought on in which direction to wrap. Maybe, we want to log-domain
-- Discretized values, which probably just works.

newtype Discretized (b ∷ k) = Discretized { getDiscretized ∷ Int }
  deriving (Eq,Ord,Generic,Read)

instance Show (Discretized b) where
  show (Discretized d) = show d

instance Info (Discretized b) where
  -- TODO show @b@ information
  info = show . getDiscretized

fromUnknown ∷ Discretized Unknown → Discretized t
{-# Inline fromUnknown #-}
fromUnknown = coerce

derivingUnbox "Discretized"
  [t| forall t . Discretized t → Int |]  [| getDiscretized |]  [| Discretized |]

instance NFData (Discretized t) where
  rnf (Discretized k) = rnf k
  {-# Inline rnf #-}

instance Binary    (Discretized t)
instance Serialize (Discretized t)
instance Hashable  (Discretized t)

instance (KnownNat k, KnownNat l) ⇒ ToJSON (Discretized (k :% l)) where
  toJSON = toJSON . fromRational @Double . toRational

instance (KnownNat k, KnownNat l) ⇒ FromJSON (Discretized (k :% l)) where
  parseJSON = fmap (fromRational . toRational @Double) . parseJSON

instance Num (Discretized Unknown) where
  Discretized x + Discretized y = Discretized $ x+y
  Discretized x - Discretized y = Discretized $ x-y
  (*) = error "Discretized Unknown does not admit (*)"
  abs (Discretized x) = Discretized $ abs x
  signum (Discretized x) = Discretized $ signum x
  fromInteger = error "Discretized Unknown does not admit fromInteger"
  {-# Inline (+) #-}
  {-# Inline (-) #-}
  {-# Inline abs #-}
  {-# Inline signum #-}

instance (KnownNat u, KnownNat l) ⇒ Num (Discretized ((u∷Nat) :% (l∷Nat))) where
  {-# Inline (+) #-}
  Discretized x + Discretized y = Discretized (x+y)
  {-# Inline (-) #-}
  Discretized x - Discretized y = Discretized (x-y)
  -- TODO it should be possible to generalize this over arbitrary value, or
  -- replace @KnownNat@ with the above @ratioTyConstant@.
  {-# Inline (*) #-}
  Discretized x * Discretized y =
    let u = fromInteger $ natVal @u Proxy
        l = fromInteger $ natVal @l Proxy
    in  Discretized $ (x*y*u) `div` l
  {-# Inline abs #-}
  abs (Discretized x) = Discretized (abs x)
  {-# Inline signum #-}
  signum (Discretized x) = Discretized $ signum x
  {-# Inline fromInteger #-}
  fromInteger x =
    let u = fromInteger $ natVal @u Proxy
        l = fromInteger $ natVal @l Proxy
    in  Discretized $ (fromInteger x*u) `div` l

instance Enum (Discretized b) where
  toEnum = Discretized
  {-# Inline toEnum #-}
  fromEnum = getDiscretized
  {-# Inline fromEnum #-}

-- instance (Enum (Discretized b), KnownNat u, KnownNat l) ⇒ Integral (Discretized u l) where

instance (KnownNat u, KnownNat l) ⇒ Fractional (Discretized ((u∷Nat) :% (l∷Nat))) where
  {-# Inline (/) #-}
  Discretized x / Discretized y =
    let u = fromInteger $ natVal @u Proxy
        l = fromInteger $ natVal @l Proxy
    in  Discretized $ (x * l) `div` (y * u)
  {-# Inline recip #-}
  recip (Discretized x) =
    let u = fromInteger $ natVal @u Proxy
        l = fromInteger $ natVal @l Proxy
    in  error "need to find approximately ok transformation"
  {-# Inline fromRational #-}
  fromRational (a :% b) =
    let u = natVal @u Proxy
        l = natVal @l Proxy
    in  Discretized . fromInteger $ (a * l) `div` (b * u)

instance (KnownNat u, KnownNat l) ⇒ Real (Discretized ((u∷Nat) :% (l∷Nat))) where
  {-# Inline toRational #-}
  toRational (Discretized d) =
    let u = natVal @u Proxy
        l = natVal @l Proxy
    in  (fromIntegral d * u) % l

instance (Num (Discretized k)) ⇒ Semiring (Discretized k) where
  plus = (+)
  times = (*)
  zero = 0
  one = 1
  {-# Inline plus  #-}
  {-# Inline times #-}
  {-# Inline zero  #-}
  {-# Inline one   #-}

instance (NumericLimits (Discretized t)) where
  minFinite = Discretized minFinite
  {-# Inline minFinite #-}
  maxFinite = Discretized maxFinite
  {-# Inline maxFinite #-}

-- | Discretizes any @Real a@ into the @Discretized@ value. This conversion
-- is /lossy/ and uses a type-level rational of @u :% l@!

discretizeRatio ∷ forall a u l . (Real a, KnownNat u, KnownNat l) ⇒ a → Discretized ((u∷Nat) :% (l∷Nat))
{-# Inline discretizeRatio #-}
discretizeRatio a =
  let u = natVal @u Proxy
      l = natVal @l Proxy
      k = toRational a
  in  Discretized . fromIntegral $ numerator k * l `div` (denominator k * u)

