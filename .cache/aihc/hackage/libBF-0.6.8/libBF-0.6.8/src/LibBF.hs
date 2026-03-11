{-# Language BangPatterns #-}
{-# Language BlockArguments #-}
{-# Language Trustworthy #-}
-- | Computation with high-precision floats.
module LibBF
  (
    -- * Constants
    BigFloat
  , bfPosZero, bfNegZero
  , bfPosInf, bfNegInf
  , bfNaN

    -- * Conversions
  , bfFromWord
  , bfFromInt
  , bfFromDouble
  , bfFromInteger
  , bfFromString
  , bfToDouble
  , bfToString
  , bfToRep
  , BFRep(..)
  , BFNum(..)
  , bfFromBits
  , bfToBits

    -- * Predicates
  , bfIsFinite
  , bfIsInf
  , bfIsZero
  , bfIsNaN
  , bfIsNormal
  , bfIsSubnormal
  , bfCompare
  , bfSign
  , bfExponent
  , bfIsPos
  , bfIsNeg
  , Sign(..)

    -- * Arithmetic
  , bfNeg, bfAbs
  , bfAdd, bfSub, bfMul, bfDiv, bfRem
  , bfFMA, bfMulWord, bfMulInt, bfMul2Exp
  , bfSqrt
  , bfPow

    -- * Rounding
  , bfRoundFloat, bfRoundInt

    -- * Mutability
  , bfUnsafeThaw
  , bfUnsafeFreeze

    -- * Limits


    -- * Configuration
  , module LibBF.Opts
  ) where


import Data.Bits
import Data.Data (Data(..))
import Data.Hashable
import Data.Word
import Data.Int
import System.IO.Unsafe

import LibBF.Mutable as M
import LibBF.Opts
import Control.DeepSeq

-- | Arbitrary precision floating point numbers.
newtype BigFloat = BigFloat BF

instance Data BigFloat where
  -- BigFloat is exported as an abstract data type, so we intentionally define
  -- the Data instance in a simplistic way so as to avoid leaking the BF
  -- internals.
  gfoldl _ z = z
  gunfold _ _ = error "Data.Data.gunfold(BigFloat)"
  toConstr _ = error "Data.Data.toConstr(BigFloat)"
  dataTypeOf _ = error "Data.Data.dataTypeOf(BigFloat)"

instance NFData BigFloat where
  rnf x = x `seq` ()


instance Show BigFloat where
  show = bfToString 16 (showFreeMin Nothing <> addPrefix)

--------------------------------------------------------------------------------
{-# NOINLINE ctxt #-}
{-# OPTIONS_GHC -fno-cse #-}
ctxt :: BFContext
ctxt = unsafePerformIO newContext

newBigFloat :: (BF -> IO ()) -> BigFloat
newBigFloat f = unsafe $
  do bf <- new ctxt
     f bf
     pure (BigFloat bf)

newBigFloat' :: (BF -> IO a) -> (BigFloat,a)
newBigFloat' f = unsafe $
  do bf <- new ctxt
     a <- f bf
     pure (BigFloat bf, a)

unsafe :: IO a -> a
unsafe = unsafePerformIO

--------------------------------------------------------------------------------
-- Constants

-- | Positive zero.
bfPosZero :: BigFloat
bfPosZero = newBigFloat (setZero Pos)

-- | Negative zero.
bfNegZero :: BigFloat
bfNegZero = newBigFloat (setZero Neg)

-- | Positive infinity.
bfPosInf :: BigFloat
bfPosInf = newBigFloat (setInf Pos)

-- | Negative infinity.
bfNegInf :: BigFloat
bfNegInf = newBigFloat (setInf Neg)

-- | Not-a-number.
bfNaN :: BigFloat
bfNaN = newBigFloat setNaN

-- | A floating point number corresponding to the given word.
bfFromWord :: Word64 -> BigFloat
bfFromWord = newBigFloat . setWord

-- | A floating point number corresponding to the given int.
bfFromInt :: Int64 -> BigFloat
bfFromInt = newBigFloat . setInt

-- | A floating point number corresponding to the given double.
bfFromDouble :: Double -> BigFloat
bfFromDouble = newBigFloat . setDouble

-- | A floating point number corresponding to the given integer.
bfFromInteger :: Integer -> BigFloat
bfFromInteger = newBigFloat . setInteger

-- | IEEE 754 equality
instance Eq BigFloat where
  BigFloat x == BigFloat y = unsafe (cmpEq x y)

-- | IEEE 754 comparisons
instance Ord BigFloat where
  BigFloat x < BigFloat y  = unsafe (cmpLT x y)
  BigFloat x <= BigFloat y = unsafe (cmpLEQ x y)
  BigFloat x > BigFloat y  = unsafe (cmpLT y x)
  BigFloat x >= BigFloat y  = unsafe (cmpLEQ y x)


instance Hashable BigFloat where
  hashWithSalt s x = hashWithSalt s (bfToRep x)


{-| Compare the two numbers.  The special values are ordered like this:

      * -0 < 0
      * NaN == NaN
      * NaN is larger than all other numbers

Note that this differs from `(<=)`
-}
bfCompare :: BigFloat -> BigFloat -> Ordering
bfCompare (BigFloat x) (BigFloat y) = unsafe (cmp x y)


-- | Is this a finite (i.e., non-infinite, non NaN) number.
bfIsFinite :: BigFloat -> Bool
bfIsFinite (BigFloat x) = unsafe (isFinite x)

-- | Is this value NaN.
bfIsNaN :: BigFloat -> Bool
bfIsNaN (BigFloat x) = unsafe (M.isNaN x)

-- | Is this value infinite
bfIsInf :: BigFloat -> Bool
bfIsInf (BigFloat x) = unsafe (isInf x)

-- | This is a "normal" number, which means it is not
--   a NaN, not a zero, not infinite, and not subnormal.
bfIsNormal :: BFOpts -> BigFloat -> Bool
bfIsNormal opts bf =
  case bfToRep bf of
    rep@(BFRep _sgn (Num _ _)) -> not (repIsSubnormal opts rep)
    _ -> False

-- | This number is "subnormal", which means it is among the smallest
--   representable numbers for the given precision and exponent bits.
--   These numbers differ from "normal" numbers in that they do not use
--   an implicit leading 1 bit in the binary representation.
bfIsSubnormal :: BFOpts -> BigFloat -> Bool
bfIsSubnormal opts bf = repIsSubnormal opts (bfToRep bf)

-- | Get the sign of a number.  Returns 'Nothing' if the number is `NaN`.
bfSign :: BigFloat -> Maybe Sign
bfSign (BigFloat x) = unsafe (getSign x)

-- | Compute the absolute value of a number.
bfAbs :: BigFloat -> BigFloat
bfAbs bf =
  case bfSign bf of
    Just Neg -> bfNeg bf
    _        -> bf

-- | Is this value positive
bfIsPos :: BigFloat -> Bool
bfIsPos bf =
  case bfSign bf of
    Just Pos -> True
    _ -> False

-- | Is this value negative
bfIsNeg :: BigFloat -> Bool
bfIsNeg bf =
  case bfSign bf of
    Just Neg -> True
    _ -> False

-- | Get the exponent for the given number.
-- Infinity, zero and NaN do not have an exponent.
bfExponent :: BigFloat -> Maybe Int64
bfExponent (BigFloat x) = unsafe (getExp x)

-- | Is this value a zero.
bfIsZero :: BigFloat -> Bool
bfIsZero (BigFloat x) = unsafe (isZero x)

-- | Negate a floating point number.
bfNeg :: BigFloat -> BigFloat
bfNeg (BigFloat x) = newBigFloat (\bf -> setBF x bf >> fneg bf)

-- | Add two numbers useing the given options.
bfAdd :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfAdd opt (BigFloat x) (BigFloat y) = newBigFloat' (fadd opt x y)

-- | Subtract two numbers useing the given options.
bfSub :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfSub opt (BigFloat x) (BigFloat y) = newBigFloat' (fsub opt x y)

-- | Multiply two numbers using the given options.
bfMul :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfMul opt (BigFloat x) (BigFloat y) = newBigFloat' (fmul opt x y)

-- | Multiply a number and a word, using the given options.
bfMulWord :: BFOpts -> BigFloat -> Word64 -> (BigFloat,Status)
bfMulWord opt (BigFloat x) y = newBigFloat' (fmulWord opt x y)

-- | Multiply a number and an int, using the given options.
bfMulInt :: BFOpts -> BigFloat -> Int64 -> (BigFloat,Status)
bfMulInt opt (BigFloat x) y = newBigFloat' (fmulInt opt x y)

-- | Multiply a number by @2^e@.
bfMul2Exp :: BFOpts -> BigFloat -> Int -> (BigFloat,Status)
bfMul2Exp opt (BigFloat x) e = newBigFloat' (\p ->
  do setBF x p
     fmul2Exp opt e p)

-- | Divide two numbers useing the given options.
bfDiv :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfDiv opt (BigFloat x) (BigFloat y) = newBigFloat' (fdiv opt x y)

-- | Compute the remainder @x - y * n@ where @n@ is the integer
--   nearest to @x/y@ (with ties broken to even values of @n@).
bfRem :: BFOpts -> BigFloat -> BigFloat -> (BigFloat, Status)
bfRem opt (BigFloat x) (BigFloat y) = newBigFloat' (frem opt x y)

-- | Compute the fused-multiply-add @(x*y)+z@
bfFMA :: BFOpts -> BigFloat -> BigFloat -> BigFloat -> (BigFloat, Status)
bfFMA opt (BigFloat x) (BigFloat y) (BigFloat z) = newBigFloat' (ffma opt x y z)

-- | Square root of two numbers useing the given options.
bfSqrt :: BFOpts -> BigFloat -> (BigFloat,Status)
bfSqrt opt (BigFloat x) = newBigFloat' (fsqrt opt x)

-- | Round to a float matching the input parameters.
bfRoundFloat :: BFOpts -> BigFloat -> (BigFloat,Status)
bfRoundFloat opt (BigFloat x) = newBigFloat' (\bf ->
  do setBF x bf
     fround opt bf
  )

-- | Round to an integer using the given rounding mode.
bfRoundInt :: RoundMode -> BigFloat -> (BigFloat,Status)
bfRoundInt r (BigFloat x) = newBigFloat' (\bf ->
  do setBF x bf
     frint r bf
  )

-- | Exponentiate a word to a positive integer power.
bfPow :: BFOpts -> BigFloat -> BigFloat -> (BigFloat, Status)
bfPow opts (BigFloat x) (BigFloat y) = newBigFloat' (fpow opts x y)

-- | Constant to a 'Double'
bfToDouble :: RoundMode -> BigFloat -> (Double, Status)
bfToDouble r (BigFloat x) = unsafe (toDouble r x)

-- | Render as a 'String', using the given settings.
bfToString :: Int {- ^ Base -} -> ShowFmt -> BigFloat -> String
bfToString radix opts (BigFloat x) =
  unsafe (toString radix opts x)

-- | Parse a number from the given string.
-- Returns @NaN` if the string does not correspond to a number we recognize.
bfFromString :: Int {- ^ Base -} -> BFOpts -> String -> (BigFloat,Status)
bfFromString radix opts str =
  newBigFloat' \bf ->
  do (status,_,usedAll) <- setString radix opts str bf
     if usedAll
        then pure status
        else do setNaN bf
                pure Ok

-- | The float as an exponentiated 'Integer'.
bfToRep :: BigFloat -> BFRep
bfToRep (BigFloat x) = unsafe (toRep x)

-- | Make a number mutable.
-- WARNING: This does not copy the number,
-- so it could break referential transperancy.
bfUnsafeThaw :: BigFloat -> BF
bfUnsafeThaw (BigFloat x) = x

-- | Make a number immutable.
-- WARNING: This does not copy the number,
-- so it could break referential transperancy.
bfUnsafeFreeze :: BF -> BigFloat
bfUnsafeFreeze = BigFloat

--------------------------------------------------------------------------------

-- | Make a float using "raw" bits representing the bitvector
--   representation of a floating-point value with the
--   exponent and precision bits given by the options.
bfFromBits ::
  BFOpts ->
  Integer {- ^ Raw bits -} ->
  BigFloat

bfFromBits opts bits
  | expoBiased == 0 && mant == 0 =            -- zero
    if isNeg then bfNegZero else bfPosZero

  | expoBiased == eMask && mant ==  0 =       -- infinity
    if isNeg then bfNegInf else bfPosInf

  | expoBiased == eMask = bfNaN               -- NaN

  | expoBiased == 0 =                         -- Subnormal
    case bfMul2Exp opts' (bfFromInteger mant) (expoVal + 1) of
      (num,Ok) -> if isNeg then bfNeg num else num
      (_,s)    -> error $ unwords ["bfFromBits", "subnormal case", "Unexpected status:", show s, show bits, show mant, show expoVal, show e, show p ]

  | otherwise =                               -- Normal
    case bfMul2Exp opts' (bfFromInteger mantVal) expoVal of
      (num,Ok) -> if isNeg then bfNeg num else num
      (_,s)    -> error $ unwords ["bfFromBits", "normal case", "Unexpected status:", show s, show bits, show mantVal, show expoVal, show e, show p ]

  where
  e = getExpBits opts
  p = getPrecBits opts

  opts' = opts <> allowSubnormal

  p'         = fromIntegral p - 1                          :: Int
  eMask      = (1 `shiftL` e) - 1                          :: Int
  pMask      = (1 `shiftL` p') - 1                         :: Integer

  isNeg      = testBit bits (e + p')

  mant       = pMask .&. bits                              :: Integer
  mantVal    = mant `setBit` p'                            :: Integer
  -- accounts for the implicit 1 bit

  expoBiased = eMask .&. fromInteger (bits `shiftR` p')    :: Int
  bias       = eMask `shiftR` 1                            :: Int
  expoVal    = expoBiased - bias - fromIntegral p'         :: Int


-- | Turn a float into raw bits.
-- @NaN@ is represented as a positive "quiet" @NaN@
-- (most significant bit in the significand is set, the rest of it is 0).
bfToBits :: BFOpts -> BigFloat -> Integer
bfToBits opts bf = res
  where
  res =     (isNeg      `shiftL` (e+p'))
        .|. (expBiased  `shiftL` p')
        .|. (mant       `shiftL` 0)

  e = getExpBits opts
  p = getPrecBits opts

  p' = fromIntegral p - 1 :: Int

  eMask = (1 `shiftL` e) - 1   :: Integer
  pMask = (1 `shiftL` p') - 1   :: Integer

  (isNeg, expBiased, mant) =
    case bfToRep bf of
      BFNaN       -> (0,  eMask, 1 `shiftL` (p' - 1))
      BFRep s num -> (sign, be, ma)
        where
        sign = case s of
                Neg -> 1
                Pos -> 0

        (be,ma) =
          case num of
            Zero     -> (0,0)
            Num i ev
              | ex <= 0 ->
                  (0, i `shiftL` (p'-m-1+fromInteger ex)) -- subnormal case
              | otherwise ->
                  (ex, (i `shiftL` (p' - m)) .&. pMask) -- normal case
              where
              m    = msb 0 i - 1
              bias = eMask `shiftR` 1
              ex   = toInteger ev + bias + toInteger m

            Inf -> (eMask,0)

  msb !n j = if j == 0 then n else msb (n+1) (j `shiftR` 1)

-- | test if a given big float representation is subnormal
repIsSubnormal :: BFOpts -> BFRep -> Bool
repIsSubnormal opts (BFRep _s (Num i ev)) = ex <= 0
  where
  e = getExpBits opts
  eMask = (1 `shiftL` e) - 1   :: Integer
  bias = eMask `shiftR` 1

  m    = msb (0 :: Int) i - 1
  ex   = toInteger ev + bias + toInteger m

  msb !n j = if j == 0 then n else msb (n+1) (j `shiftR` 1)

repIsSubnormal _opts _rep = False
