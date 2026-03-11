{-|

The Pedersen commitment scheme has three operations:

- Setup
- Commit
- Open

-}
{-# LANGUAGE NoImplicitPrelude #-}

module Pedersen (
  -- ** Safe Prime Field Pedersen Commitments
  Pedersen(..),
  CommitParams(..),
  Commitment(..),
  Reveal(..),

  setup,
  commit,
  open,

  addCommitments,
  verifyAddCommitments,

  verifyCommitParams,


  -- ** Elliptic Curve Pedersen Commitments
  ECPedersen(..),
  ECCommitParams(..),
  ECCommitment(..),
  ECReveal(..),

  ecSetup,
  ecCommit,
  ecOpen,

  ecAddCommitments,
  ecVerifyAddCommitments,
  ecAddInteger,
  ecVerifyAddInteger,

  verifyECCommitParams

) where

import Protolude
import qualified Prelude

import Crypto.Hash
import Crypto.Number.Serialize (os2ip)
import Crypto.Random.Types (MonadRandom(..))
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.PubKey.ECC.Types as ECC

import Data.Bits (xor, popCount)
import qualified Data.ByteArray as BA
import qualified Data.Map as Map

import PrimeField

-------------------------------------------------------------------------------
-- Pedersen Commitment Scheme
-------------------------------------------------------------------------------

data CommitParams = CommitParams
  { pedersenSPF :: SPF     -- ^ Safe prime field for pedersen commitment
  , pedersenH   :: Integer -- ^ \(h = g^a \mod p\) where a is random
  }

newtype Commitment = Commitment { unCommitment :: Integer }
  deriving (Eq)

data Reveal = Reveal
  { revealVal :: Integer -- ^ Original value comitted
  , revealExp :: Integer -- ^ random exponent r, \(g^x \cdot h^r\)
  }

data Pedersen = Pedersen
  { commitment :: Commitment
  , reveal     :: Reveal
  }

-- | Generates a Safe Prime Field (p,q,g) and a random value
-- \(a \in Z_q\) such that \(g^a = h\), where g and h are the bases
-- to be used in the pedersen commit function.
setup :: MonadRandom m => Int -> m (Integer, CommitParams)
setup nbits = do
  spf <- mkSPF nbits
  (a,h) <- runSPFT spf $ do
    a <- randomInZqM
    h <- gexpSafeSPFM a
    return (a,h)
  return (a, CommitParams spf h)

-- | Commit a value by generating a random number \(r \in Z_q\)
-- and computing \(C(x) = g^x \cdot h^r\) where x is the value to commit
commit :: MonadRandom m => Integer -> CommitParams -> m Pedersen
commit x (CommitParams spf h) = do
  (r,c) <- runSPFT spf $ do
    r <- randomInZqM
    c <- gexpSafeSPFM x |*| expSafeSPFM h r
    return (r,c)
  return $ Pedersen (Commitment c) (Reveal x r)

-- | Open the commit by supplying the value commited, `x`, the
-- random value `r` and the pedersen bases `g` and `h`, and
-- verifying that \(C(x) \overset{!}{=} g^x * h^r\)
open :: CommitParams -> Commitment -> Reveal -> Bool
open (CommitParams spf h) (Commitment c) (Reveal x r) =
    resCommit == c
  where
    resCommit = runSPFM spf $
      gexpSafeSPFM x |*| expSafeSPFM h r

-- | This addition should be recorded as the previous commits are unable
-- to be extracted from this new commitment. The only way to open this commiment
-- is to tell the committing party the two commitments that were added so that the
-- commitment can be validated and opening parameters can be created.
addCommitments :: CommitParams -> Commitment -> Commitment -> Commitment
addCommitments cp c1 c2 = Commitment $
  modp (pedersenSPF cp) $ unCommitment c1 * unCommitment c2

-- | This function validates a homomorphic addition of two commitments using the
-- original pedersen commits and reveals to compute the new commitment without
-- homomorphic addition.
verifyAddCommitments :: CommitParams -> Pedersen -> Pedersen -> Pedersen
verifyAddCommitments (CommitParams spf h) p1 p2 =
    Pedersen newCommitment $ Reveal newVal newExp
  where
    (Reveal x r)  = reveal p1
    (Reveal y r') = reveal p2

    newVal = modp spf $ x + y
    newExp = modp spf $ r + r'

    newCommitment = Commitment $ runSPFM spf $
      gexpSafeSPFM newVal |*| expSafeSPFM h newExp

-- | Check that `g^a = h` to verify integrity of a counterparty's commitment
verifyCommitParams :: Integer -> CommitParams -> Bool
verifyCommitParams a (CommitParams spf h) =
  runSPFM spf $ do
    h' <- gexpSafeSPFM a
    return $ h' == h

-------------------------------------------------------------------------------
-- Pedersen Commitment Scheme - Elliptic Curve (SECP256k1)
-------------------------------------------------------------------------------

secp256k1 :: ECC.Curve
secp256k1 = ECC.getCurveByName ECC.SEC_p256k1

data ECCommitParams = ECCommitParams
  { ecCurve :: ECC.Curve
  , ecH     :: ECC.Point
  }

data ECCommitment = ECCommitment { unECCommitment :: ECC.Point }
  deriving Eq

data ECReveal = ECReveal
  { ecRevealVal :: Integer
  , ecRevealScalar :: Integer
  }

data ECPedersen = ECPedersen
  { ecCommitment :: ECCommitment
  , ecReveal     :: ECReveal
  }

-- | Setup EC Pedersen commit params, defaults to curve secp256k1
ecSetup :: MonadRandom m => Maybe ECC.CurveName -> m ECCommitParams
ecSetup mCurveName = do
    a <- ECC.scalarGenerate curve
    let h = ECC.pointBaseMul curve a
    return $ ECCommitParams curve h
  where
    curve = case mCurveName of
      Nothing -> secp256k1
      Just cn' -> ECC.getCurveByName cn'

ecCommit :: MonadRandom m => Integer -> ECCommitParams -> m ECPedersen
ecCommit x (ECCommitParams curve h) = do
  r <- ECC.scalarGenerate curve
  let xG = ECC.pointBaseMul curve x
  let rH = ECC.pointMul curve r h
  let commitment = ECCommitment $ ECC.pointAdd curve xG rH
  let reveal = ECReveal x r
  return $ ECPedersen commitment reveal

ecOpen :: ECCommitParams -> ECCommitment -> ECReveal -> Bool
ecOpen (ECCommitParams curve h) (ECCommitment c) (ECReveal x r) =
    c == ECC.pointAdd curve xG rH
  where
    xG = ECC.pointBaseMul curve x
    rH = ECC.pointMul curve r h

-- | In order for this resulting commitment to be opened, the commiter
-- must construct a new set of reveal parameters. The new reveal is then
-- sent to the counterparty to open the homomorphically added commitment.
ecAddCommitments
  :: ECCommitParams
  -> ECCommitment
  -> ECCommitment
  -> ECCommitment
ecAddCommitments ecp (ECCommitment c1) (ECCommitment c2) =
  ECCommitment $ ECC.pointAdd (ecCurve ecp) c1 c2

-- | Verify the addition of two EC Pedersen Commitments by constructing
-- the new Pedersen commitment on the uncommitted values.
ecVerifyAddCommitments
  :: ECCommitParams
  -> ECPedersen
  -> ECPedersen
  -> ECPedersen
ecVerifyAddCommitments (ECCommitParams curve h) p1 p2 =
    ECPedersen newCommitment newReveal
  where
    ECReveal x1 r1 = ecReveal p1
    ECReveal x2 r2 = ecReveal p2

    newVal = x1 + x2
    newScalar = r1 + r2

    xG = ECC.pointBaseMul curve newVal
    rH = ECC.pointMul curve newScalar h

    newCommitment = ECCommitment $ ECC.pointAdd curve xG rH
    newReveal = ECReveal newVal newScalar

-- | Add an integer to the committed value. The committer should be informed
-- of the integer added to the commitment so that a valid pedersen reveal
-- can be constructed and the resulting commitment can be opened
ecAddInteger :: ECCommitParams -> ECCommitment -> Integer -> ECCommitment
ecAddInteger (ECCommitParams curve h) (ECCommitment c) n =
    ECCommitment $ ECC.pointAdd curve nG c
  where
    nG = ECC.pointBaseMul curve n

ecVerifyAddInteger :: ECCommitParams -> ECPedersen -> Integer -> ECPedersen
ecVerifyAddInteger (ECCommitParams curve h) p n =
    ECPedersen newCommitment newReveal
  where
    ECReveal x r = ecReveal p

    newVal = x + n

    xG = ECC.pointBaseMul curve newVal
    rH = ECC.pointMul curve r h -- rH doesn't change

    newCommitment = ECCommitment $ ECC.pointAdd curve xG rH
    newReveal = ECReveal newVal r -- r doesn't change

verifyECCommitParams :: Integer -> ECCommitParams -> Bool
verifyECCommitParams a (ECCommitParams curve h) = h == ECC.pointBaseMul curve a
