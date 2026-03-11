{-# Language PatternSynonyms, CApiFFI, ViewPatterns #-}
-- | Configuration and results for FP computation.
module LibBF.Opts
  (  -- * Options
    BFOpts(..)
  , allowSubnormal

    -- ** Presets
  , float16
  , float32
  , float64
  , float128
  , float256

    -- ** Precision
  , precBits
  , getPrecBits
  , precBitsMin
  , precBitsMax
  , infPrec

    -- ** Exponent Size
  , expBits
  , getExpBits
  , expBitsMin
  , expBitsMax

    -- ** Rounding mode
  , rnd
  , RoundMode(..)
  , pattern NearEven
  , pattern ToZero
  , pattern ToNegInf
  , pattern ToPosInf
  , pattern NearAway
  , pattern Away
  , pattern Faithful


  -- ** Pretty printing options
  , ShowFmt(..)
  , showRnd
  , showFixed
  , showFrac
  , showFree
  , showFreeMin
  , addPrefix
  , forceExp
  , radixMax

  -- * Status
  , Status(..)
  , pattern Ok
  , pattern InvalidOp
  , pattern DivideByZero
  , pattern Overflow
  , pattern Underflow
  , pattern Inexact
  , pattern MemError

  -- * Internal
  , LimbT
  , SLimbT
  , FlagsT
  )
  where

import Data.Word
import Data.Int
import Foreign.C.Types
import Data.Bits
import Data.List
#include <libbf.h>

-- | Internal: type for limbs
type LimbT  = #{type limb_t}

-- | Internal: type for signed limbs
type SLimbT = #{type slimb_t}

-- | Internal: type for flags
type FlagsT = #{type bf_flags_t}

-- | Specifies various computation settings, combined with 'Semigroup'.
data BFOpts = BFOpts !LimbT !FlagsT

instance Semigroup BFOpts where
  BFOpts l f <> BFOpts l1 f1 = BFOpts (max l l1) (f .|. f1)


-- | Use infinite precision.  This should be used with caution,
-- as it could exhause memory, and at the moment the library
-- does not handle this gracefully at all (core dumps).
infPrec :: BFOpts
infPrec = BFOpts #{const BF_PREC_INF} 0

-- | Use this many bits to represent the mantissa in the computation.
-- The input should be in the interval defined by 'precMin' and 'precMax'
precBits :: Word -> BFOpts
precBits n = BFOpts (fromIntegral n) 0

-- | Retrieve how many bits to represent the mantissa in the computation.
getPrecBits :: BFOpts -> Word
getPrecBits (BFOpts n _) = fromIntegral n

-- | Use the given rounding mode.
-- If none is specified, then the default is 'NearEven'.
rnd :: RoundMode -> BFOpts
rnd (RoundMode r) = BFOpts 0 r

-- | The smallest supported precision (in bits).
foreign import capi "libbf.h value BF_PREC_MIN"
  precBitsMin :: Int

-- | The largest supported precision (in bits).
-- Memory could run out before we run out of precision.
foreign import capi "libbf.h value BF_PREC_MAX"
  precBitsMax :: Int

{- | Allow denormalized answers. -}
allowSubnormal :: BFOpts
allowSubnormal = BFOpts 0 #{const BF_FLAG_SUBNORMAL}


foreign import capi "libbf.h bf_set_exp_bits"
  bf_set_exp_bits :: CInt -> FlagsT

foreign import capi "libbf.h bf_get_exp_bits"
  bf_get_exp_bits :: FlagsT -> CInt

-- | Set how many bits to use to represent the exponent.
-- Should fit in the range defined by 'expBitsMin' and 'expBitsMax'.
expBits :: Int -> BFOpts
expBits n = BFOpts 0 (bf_set_exp_bits (fromIntegral n))

-- | Get the number of exponent bits from a @BFOpts@ value.
getExpBits :: BFOpts -> Int
getExpBits (BFOpts _ f) = fromIntegral (bf_get_exp_bits f)

{-| The smallest supported number of bits in the exponent. -}
foreign import capi "libbf.h value BF_EXP_BITS_MIN"
  expBitsMin :: Int

{-| The largest number of exponent bits supported. -}
foreign import capi "libbf.h value BF_EXP_BITS_MAX"
  expBitsMax :: Int



--------------------------------------------------------------------------------

-- | Precision 11, exponent 5
float16:: RoundMode -> BFOpts
float16 r = rnd r <> precBits 11 <> expBits 5

-- | Precision 24, exponent 8
float32 :: RoundMode -> BFOpts
float32 r = rnd r <> precBits 24 <> expBits 8

-- | Precision 53, exponent 11
float64 :: RoundMode -> BFOpts
float64 r = rnd r <> precBits 53 <> expBits 11

-- | Precision 113, exponent 15
float128 :: RoundMode -> BFOpts
float128 r = rnd r <> precBits 113 <> expBits 15

-- | Precision 237, exponent 19
float256 :: RoundMode -> BFOpts
float256 r = rnd r <> precBits 237 <> expBits 19


--------------------------------------------------------------------------------

-- | Settings for rendering numbers as 'String'.
data ShowFmt = ShowFmt !LimbT !FlagsT

-- | Use this rounding mode.
showRnd :: RoundMode -> ShowFmt
showRnd (RoundMode r) = ShowFmt 1 r

instance Semigroup ShowFmt where
  ShowFmt a x <> ShowFmt b y = ShowFmt (max a b) (x .|. y)

{-| Show this many significant digits total . -}
showFixed :: Word -> ShowFmt
showFixed n = ShowFmt (fromIntegral n) #{const BF_FTOA_FORMAT_FIXED}

{-| Show this many digits after the decimal point. -}
showFrac :: Word -> ShowFmt
showFrac n = ShowFmt (fromIntegral n) #{const BF_FTOA_FORMAT_FRAC}

{-| Use as many digits as necessary to match the required precision
   rounding to nearest and the subnormal+exponent configuration of 'FlagsT'.
   The result is meaningful only if the input is already rounded to
   the wanted precision.

   Infinite precision, indicated by giving 'Nothing' for the precision
   is supported when the radix is a power of two. -}
showFree :: Maybe Word -> ShowFmt
showFree mb = ShowFmt prec #{const BF_FTOA_FORMAT_FREE}
  where prec = case mb of
                 Nothing -> #{const BF_PREC_INF}
                 Just n  -> fromIntegral n


{-| same as 'showFree' but uses the minimum number of digits
(takes more computation time). -}
showFreeMin :: Maybe Word -> ShowFmt
showFreeMin mb = ShowFmt prec #{const BF_FTOA_FORMAT_FREE_MIN}
  where prec = case mb of
                 Nothing -> #{const BF_PREC_INF}
                 Just n  -> fromIntegral n



{- | add 0x prefix for base 16, 0o prefix for base 8 or 0b prefix for
   base 2 if non zero value -}
addPrefix :: ShowFmt
addPrefix = ShowFmt 0 #{const BF_FTOA_ADD_PREFIX}

-- | Show in exponential form.
forceExp :: ShowFmt
forceExp = ShowFmt 0 #{const BF_FTOA_FORCE_EXP}


-- | Maximum radix when rendering to a for @bf_atof@ and @bf_froa@.
foreign import capi "libbf.h value BF_RADIX_MAX"
  radixMax :: Int





--------------------------------------------------------------------------------
-- | Specifies how to round when the result can't be precise.
newtype RoundMode = RoundMode FlagsT
                      deriving Show

{-| Round to nearest, ties go to even. -}
pattern NearEven :: RoundMode
pattern NearEven = RoundMode #{const BF_RNDN}

{-| Round toward zero. -}
pattern ToZero :: RoundMode
pattern ToZero = RoundMode #{const BF_RNDZ}

{-| Round down (toward -inf). -}
pattern ToNegInf :: RoundMode
pattern ToNegInf = RoundMode #{const BF_RNDD}

{-| Round up (toward +inf). -}
pattern ToPosInf :: RoundMode
pattern ToPosInf = RoundMode #{const BF_RNDU}

{-| Round to nearest, ties go away from zero. -}
pattern NearAway :: RoundMode
pattern NearAway = RoundMode #{const BF_RNDNA}

{-| Round away from zero -}
pattern Away :: RoundMode
pattern Away = RoundMode #{const BF_RNDA}

{-| Faithful rounding (nondeterministic, either 'ToPosInf' or 'ToNegInf').
    The 'Inexact' flag is always set. -}
pattern Faithful :: RoundMode
pattern Faithful = RoundMode #{const BF_RNDF}


--------------------------------------------------------------------------------

-- | A set of flags indicating things that might go wrong.
newtype Status = Status CInt deriving (Eq,Ord)

instance Semigroup Status where
  Status a <> Status b = Status (a .|. b)

instance Monoid Status where
  mempty = Ok
  mappend = (<>)

checkStatus :: CInt -> Status -> Bool
checkStatus n (Status x) = (x .&. n) > 0

-- | Succeeds if everything is OK.
pattern Ok :: Status
pattern Ok = Status 0

-- | We tried to perform an invalid operation.
pattern InvalidOp :: Status
pattern InvalidOp <- (checkStatus #{const BF_ST_INVALID_OP} -> True)
  where InvalidOp = Status #{const BF_ST_INVALID_OP}

-- | We divided by zero.
pattern DivideByZero :: Status
pattern DivideByZero <- (checkStatus #{const BF_ST_DIVIDE_ZERO} -> True)
  where DivideByZero = Status #{const BF_ST_DIVIDE_ZERO}

-- | The result can't be represented because it is too large.
pattern Overflow :: Status
pattern Overflow <- (checkStatus #{const BF_ST_OVERFLOW} -> True)
  where Overflow = Status #{const BF_ST_OVERFLOW}

-- | The result can't be represented because it is too small.
pattern Underflow :: Status
pattern Underflow <- (checkStatus #{const BF_ST_UNDERFLOW} -> True)
  where Underflow = Status #{const BF_ST_UNDERFLOW}

-- | The result is not exact.
pattern Inexact :: Status
pattern Inexact <- (checkStatus #{const BF_ST_INEXACT} -> True)
  where Inexact = Status #{const BF_ST_INEXACT}

-- | Memory error.  @NaN@ is returned.
pattern MemError :: Status
pattern MemError <- (checkStatus #{const BF_ST_MEM_ERROR} -> True)
  where MemError = Status #{const BF_ST_MEM_ERROR}

instance Show Status where
  show x@(Status i) = case x of
                        Ok -> "Ok"
                        _  -> case checkInv of
                                [] -> "(Status " ++ show i ++ ")"
                                xs -> "[" ++ intercalate "," xs ++ "]"
    where
    checkInv = case x of
                 InvalidOp -> "InvalidOp" : checkZ
                 _         -> checkZ

    checkZ = case x of
               DivideByZero -> "DivideByZero" : checkO
               _            -> checkO

    checkO = case x of
               Overflow -> "Overflow" : checkU
               _        -> checkU

    checkU = case x of
               Underflow -> "Underflow" : checkI
               _ -> checkI

    checkI = case x of
               Inexact -> "Inexact" : checkM
               _       -> checkM

    checkM = case x of
               MemError -> ["MemError"]
               _        -> []


