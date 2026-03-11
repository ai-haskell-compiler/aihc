{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PrimeField (
    P,
    unP,
    Q,
    unQ,
    G,
    unG,

    SPF,
    spfP,
    spfQ,
    spfG,
    mkSPF,
    mkSPF',

    SPFM,
    runSPFT,
    runSPFM,

    gexpSafeSPF,
    gexpSafeSPFM,
    expSafeSPF,
    expSafeSPFM,

    randomInZq,
    randomInZqM,
    randomInZp,
    randomInZpM,

    modp,
    modpM,

    (|*|),
    (|+|),
)where

import Protolude

import Crypto.Random.Types (MonadRandom(..))
import Crypto.Number.Generate (generateBetween)
import Crypto.Number.ModArithmetic (expSafe)
import Crypto.Number.Prime (generateSafePrime, isProbablyPrime)

-------------------------------------------------------------------------------
-- Types for Safe Prime fields
-------------------------------------------------------------------------------

-- | A large, safe prime, p = 2q + 1, where q is a large prime
newtype P = P { unP :: Integer }
  deriving (Show, Eq, Ord)

-- | A large prime such that p = 2q + 1 and p is also prime
newtype Q = Q { unQ :: Integer }
  deriving (Show, Eq, Ord)

-- | A generator order Q for prime field order P
newtype G = G { unG :: Integer }
  deriving (Show, Eq, Ord)

-- | A Safe Prime Field (Zp):
--     Q = large prime
--     P = 2Q + 1, also prime
--     G = generator for Zp order q
data SPF = SPF
  { spfP :: P
  , spfQ :: Q
  , spfG :: G
  }

mkSPF :: MonadRandom m => Int -> m SPF
mkSPF nbits = do
  p <- generateSafePrime nbits
  let q = (p - 1) `div` 2
  g <- generateBetween 2 (q-1)
  return $ SPF (P p) (Q q) (G g)

mkSPF' :: Integer -> Integer -> Integer -> Maybe SPF
mkSPF' p g q
  | isPPrime &&
    isQPrime &&
    isPSafePrime &&
    isGGenerator = Just $
      SPF (P p) (Q q) (G g)
  | otherwise = Nothing
  where
    isPPrime = isProbablyPrime p
    isQPrime = isProbablyPrime q
    isPSafePrime = p == (2*q + 1)
    isGGenerator = g > 1 && g < p

-- | For computations using Safe Prime Field params
type SPFM = ReaderT SPF

runSPFT :: SPF -> SPFM m a -> m a
runSPFT = flip runReaderT

runSPFM :: SPF -> SPFM Identity a -> a
runSPFM spf = runIdentity . runSPFT spf

-------------------------------------------------------------------------------
-- Operations in Safe Prime fields
-------------------------------------------------------------------------------

-- | Compute g^e `mod` p
gexpSafeSPF :: SPF -> Integer -> Integer
gexpSafeSPF (SPF p _ g) e = expSafe (unG g) e (unP p)

gexpSafeSPFM :: Monad m => Integer -> SPFM m Integer
gexpSafeSPFM e = liftM (`gexpSafeSPF` e) ask

-- | Compute b^e `mod` p
expSafeSPF :: SPF -> Integer -> Integer -> Integer
expSafeSPF (SPF p _ _) b e = expSafe b e (unP p)

expSafeSPFM :: Monad m => Integer -> Integer -> SPFM m Integer
expSafeSPFM b e = (\spf -> expSafeSPF spf b e) <$> ask

-- | Generate random number in Zq
randomInZq :: MonadRandom m => SPF -> m Integer
randomInZq (SPF _ q _) = generateBetween 1 (unQ q - 1)

randomInZqM :: MonadRandom m => SPFM m Integer
randomInZqM = lift . randomInZq =<< ask

-- | Generate random number in Zp
randomInZp :: MonadRandom m => SPF -> m Integer
randomInZp (SPF p _ _) = generateBetween 1 (unP p - 1)

randomInZpM :: MonadRandom m => SPFM m Integer
randomInZpM = lift . randomInZp =<< ask

modp :: SPF -> Integer -> Integer
modp (SPF p _ _) n = n `mod` unP p

modpM :: Monad m => Integer -> SPFM m Integer
modpM n = flip modp n <$> ask

(|*|) :: Monad m => SPFM m Integer -> SPFM m Integer -> SPFM m Integer
x |*| y = modpM =<< liftM2 (*) x y

(|+|) :: Monad m => SPFM m Integer -> SPFM m Integer -> SPFM m Integer
x |+| y = modpM =<< liftM2 (+) x y
