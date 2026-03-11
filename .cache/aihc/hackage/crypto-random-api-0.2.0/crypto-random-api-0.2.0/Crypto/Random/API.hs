-- |
-- Module      : Crypto.Random.API
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good

module Crypto.Random.API
    ( CPRG(..)
    , ReseedPolicy(..)
    , genRandomBytes
    , genRandomBytes'
    , withRandomBytes
    , getSystemEntropy
    -- * System Random generator
    , SystemRandom
    , getSystemRandomGen
    ) where

import Control.Applicative
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified System.Entropy as SE
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Word

-- | This is the reseed policy requested by the CPRG
data ReseedPolicy =
      NeverReseed          -- ^ there is no need to reseed as either
                           -- the RG doesn't supports it, it's done automatically
                           -- or pratically the reseeding period exceed a Word64 type.
    | ReseedInBytes Word64 -- ^ the RG need to be reseed in the number
                           -- of bytes joined to the type. it should be done before
                           -- the number reached 0, otherwise an user of the RG
                           -- might request too many bytes and get repeated random bytes.
    deriving (Show,Eq)

-- | A class of Cryptographic Secure Random generator.
--
-- The main difference with the generic haskell RNG is that
-- it return bytes instead of integer.
--
-- It is quite similar to the CryptoRandomGen class in crypto-api
-- except that error are not returned to the user. Instead
-- the user is suppose to handle reseeding by using the NeedReseed
-- and SupplyEntropy methods. For other type of errors, the user
-- is expected to generate bytes with the parameters bounds explicity
-- defined here.
-- 
-- The CPRG need to be able to generate up to 2^20 bytes in one call,
--
class CPRG g where
    -- | Provide a way to query the CPRG to calculate when new entropy
    -- is required to be supplied so the CPRG doesn't repeat output, and
    -- break assumptions. This returns the number of bytes before
    -- which supply entropy should have been called.
    cprgNeedReseed    :: g -> ReseedPolicy

    -- | Supply entropy to the CPRG, that can be used now or later
    -- to reseed the CPRG. This should be used in conjunction to
    -- NeedReseed to know when to supply entropy.
    cprgSupplyEntropy :: ByteString -> g -> g

    -- | Generate bytes using the CPRG and the number specified.
    --
    -- For user of the API, it's recommended to use genRandomBytes
    -- instead of this method directly. the CPRG need to be able
    -- to supply at minimum 2^20 bytes at a time.
    cprgGenBytes      :: Int -> g -> (ByteString, g)

-- | Generate bytes using the cprg in parameter.
--
-- If the number of bytes requested is really high,
-- it's preferable to use 'genRandomBytes' for better memory efficiency.
genRandomBytes :: CPRG g => Int -- ^ number of bytes to return
                         -> g   -- ^ CPRG to use
                         -> (ByteString, g)
genRandomBytes len rng = (\(lbs,g) -> (B.concat lbs, g)) $ genRandomBytes' len rng

-- | Generate bytes using the cprg in parameter.
--
-- This is not tail recursive and an excessive len (>= 2^29) parameter would
-- result in stack overflow.
genRandomBytes' :: CPRG g => Int -- ^ number of bytes to return
                          -> g   -- ^ CPRG to use
                          -> ([ByteString], g)
genRandomBytes' len rng
    | len < 0    = error "genBytes: cannot request negative amount of bytes."
    | otherwise  = loop rng len
            where loop g len
                    | len == 0  = ([], g)
                    | otherwise = let itBytes  = min (2^20) len
                                      (bs, g') = cprgGenBytes itBytes g
                                      (l, g'') = genRandomBytes' (len-itBytes) g'
                                   in (bs:l, g'')

-- | this is equivalent to using Control.Arrow 'first' with 'genRandomBytes'.
--
-- namely it generate @len bytes and map the bytes to the function @f
withRandomBytes :: CPRG g => g -> Int -> (ByteString -> a) -> (a, g)
withRandomBytes rng len f = (f bs, rng')
    where (bs, rng') = genRandomBytes len rng

-- | Return system entropy using the entropy package 'getEntropy'
getSystemEntropy :: Int -> IO ByteString
getSystemEntropy = SE.getEntropy

-- | This is a simple generator that pull bytes from the system entropy
-- directly. Its randomness and security properties are absolutely
-- depends on the underlaying system implementation.
data SystemRandom = SystemRandom [B.ByteString]

-- | Get a random number generator based on the standard system entropy source
getSystemRandomGen :: IO SystemRandom
getSystemRandomGen = do
    ch <- SE.openHandle
    let getBS = unsafeInterleaveIO $ do
        bs   <- SE.hGetEntropy ch 8192
        more <- getBS
        return (bs:more)
    SystemRandom <$> getBS

instance CPRG SystemRandom where
   cprgNeedReseed      _ = NeverReseed
   cprgSupplyEntropy _ g = g
   cprgGenBytes n (SystemRandom l) = (B.concat l1, SystemRandom l2)
        where (l1, l2) = lbsSplitAt n l
              lbsSplitAt rBytes (x:xs)
                | xLen >= rBytes =
                    let (b1,b2) = B.splitAt rBytes x
                     in  ([b1], b2:xs)
                | otherwise =
                    let (l1,l2) = lbsSplitAt (rBytes-xLen) xs
                     in (x:l1,l2)
                where xLen = B.length x
