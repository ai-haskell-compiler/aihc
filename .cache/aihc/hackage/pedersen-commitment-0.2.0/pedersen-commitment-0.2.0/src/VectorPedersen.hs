module VectorPedersen (
  dot,
  mkGs,
  scalarGenerateN,
  ecCommit,
  ecOpen,
  ecVerifyAddCommitments,
  vecSum,
  ecAddVector,
  ecVerifyAddVector,

  ECReveal(..),
  ECPedersen(..),
  ECCommitParams(..),
  ECCommitment(..)
) where

import           Protolude                  hiding (hash)
import           Crypto.Hash
import           Crypto.Number.Serialize    (os2ip)
import qualified Crypto.PubKey.ECC.Prim     as ECC
import qualified Crypto.PubKey.ECC.Types    as ECC
import           Crypto.Random.Types (MonadRandom(..))
import qualified Data.ByteArray             as BA
import           Data.Monoid                ((<>))
import Pedersen (
    ecSetup,
    ECCommitParams(..),
    ECCommitment(..)
  )

-------------------------------------------------------------------------------
-- Vector Pedersen Commitment Scheme - Elliptic Curve (SECP256k1)
-------------------------------------------------------------------------------

-- | ecRevealVal is a vector of scalars
data ECReveal = ECReveal
  { ecRevealVal :: [Integer]
  , ecRevealScalar :: Integer
  }

data ECPedersen = ECPedersen
  { ecCommitment :: ECCommitment
  , ecReveal     :: ECReveal
  }

-- | Outputs unpredictable but deterministic random values
oracle :: ECC.Curve -> ByteString -> Integer
oracle curve x = os2ip (sha256 x) `mod` n
  where
    -- | Order of the curve
    n :: Integer
    n = ECC.ecc_n $ ECC.common_curve curve

-- | Secure cryptographic hash function
sha256 :: ByteString -> ByteString
sha256 bs = BA.convert (hash bs :: Digest SHA3_256)

-- | Generate a commit value which is a vector of N elements
scalarGenerateN :: MonadRandom m => ECC.Curve -> Word8 -> m [Integer]
scalarGenerateN curve n = scalarGenerateN' curve n []

scalarGenerateN' :: MonadRandom m => ECC.Curve -> Word8 -> [Integer] -> m [Integer]
scalarGenerateN' curve n v
  | n == 0 = return v
  | otherwise = do
      vi <- ECC.scalarGenerate curve
      if vi `elem` v
        then scalarGenerateN' curve n v
        else scalarGenerateN' curve (n-1) (vi:v)

-- | Dot product between a vector of scalars and a vector of ECC.Points
dot :: ECC.Curve -> [Integer] -> [ECC.Point] -> ECC.Point
dot curve scalars points = foldl' (ECC.pointAdd curve) ECC.PointO $
  zipWith (ECC.pointMul curve) scalars points

-- | Concatenate point coordinates to create a hashable type
appendCoordinates :: ECC.Point -> ByteString
appendCoordinates ECC.PointO      = ""
appendCoordinates (ECC.Point x y) = show x <> show y

-- | Generate vector of generators in a deterministic way from the curve generator g
-- by applying H(encode(g) || i) where H is a secure hash function
mkGs :: ECC.Curve -> [ECC.Point]
mkGs curve =
  fmap (ECC.pointBaseMul curve . oracle curve . (<> appendCoordinates g) . show) [1..]
  where
      g = ECC.ecc_g $ ECC.common_curve curve

-- | Commitment function. The value we commit to is now a vector
ecCommit :: MonadRandom m => [Integer] -> ECCommitParams -> m ECPedersen
ecCommit v (ECCommitParams curve h) = do
  r <- ECC.scalarGenerate curve

  let vG = dot curve v (mkGs curve)
  let rH = ECC.pointMul curve r h

  let commitment = ECCommitment $ ECC.pointAdd curve vG rH
  let reveal = ECReveal v r
  return $ ECPedersen commitment reveal

-- | Open commitment to check its validity
ecOpen :: ECCommitParams -> ECCommitment -> ECReveal -> Bool
ecOpen (ECCommitParams curve h) (ECCommitment c) (ECReveal v r) =
    c == ECC.pointAdd curve vG rH
  where
    vG = dot curve v (mkGs curve)
    rH = ECC.pointMul curve r h

-- | Sum of vectors in a curve
vecSum :: ECC.Curve -> [Integer] -> [Integer] -> [Integer]
vecSum curve = zipWith (\a b -> a + b `mod` n)
    where
      n :: Integer
      n = ECC.ecc_n $ ECC.common_curve curve

-- | Verify the addition of two EC Vector Pedersen Commitments by constructing
-- the new Pedersen commitment on the uncommitted values.
ecVerifyAddCommitments
  :: ECCommitParams
  -> ECPedersen
  -> ECPedersen
  -> ECPedersen
ecVerifyAddCommitments (ECCommitParams curve h) p1 p2 =
    ECPedersen newCommitment newReveal
  where
    ECReveal v r1 = ecReveal p1
    ECReveal w r2 = ecReveal p2

    vw = vecSum curve v w
    r = r1 + r2

    vwG = dot curve vw (mkGs curve)
    rH = ECC.pointMul curve r h

    newCommitment = ECCommitment $ ECC.pointAdd curve vwG rH
    newReveal = ECReveal vw r

-- | Add a vector to the committed value such that C'= C + wG 
ecAddVector :: ECCommitParams -> ECCommitment -> [Integer] -> ECCommitment
ecAddVector (ECCommitParams curve h) (ECCommitment c) w =
    ECCommitment $ ECC.pointAdd curve wG c
  where
    wG = dot curve w (mkGs curve)

-- Access the reveal values of the vector pedersen commitment (r and v)
-- Return a new commitment adding an input vector w such that C' = (v + w)G + rH
ecVerifyAddVector :: ECCommitParams -> ECPedersen -> [Integer] -> ECPedersen
ecVerifyAddVector (ECCommitParams curve h) p w =
    ECPedersen newCommitment newReveal
  where
    ECReveal v r = ecReveal p

    vw = vecSum curve v w

    vwG = dot curve vw (mkGs curve)
    rH = ECC.pointMul curve r h -- rH doesn't change

    newCommitment = ECCommitment $ ECC.pointAdd curve vwG rH
    newReveal = ECReveal vw r -- r doesn't change
