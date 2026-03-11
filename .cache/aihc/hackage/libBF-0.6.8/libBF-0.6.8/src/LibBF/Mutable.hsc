{-# Language ForeignFunctionInterface, CApiFFI #-}
{-# Language PatternSynonyms #-}
{-# Language MultiWayIf #-}
{-# Language BlockArguments #-}
{-# Language DeriveDataTypeable #-}
-- | Mutable big-float computation.
module LibBF.Mutable
  ( -- * Allocation
    newContext, BFContext
  , new, BF

    -- * Assignment
  , setNaN
  , setZero
  , setInf
  , Sign(..)
  , setWord
  , setInt
  , setDouble
  , setInteger
  , setBF
  , setString

    -- * Queries and Comparisons
  , cmpEq
  , cmpLT
  , cmpLEQ
  , cmpAbs
  , cmp
  , getSign
  , getExp

  , isFinite
  , isInf
  , LibBF.Mutable.isNaN
  , isZero

    -- * Arithmetic
  , fneg
  , fadd
  , faddInt
  , fsub
  , fmul
  , fmulInt
  , fmulWord
  , fmul2Exp
  , ffma
  , fdiv
  , frem
  , fsqrt
  , fpow
  , fround
  , frint


  -- * Convert from a number
  , toDouble
  , toString
  , toRep, BFRep(..), BFNum(..)

  -- * Configuration
  , module LibBF.Opts
  , toChunks

  ) where


import Foreign.Marshal.Alloc(alloca,free)
import Foreign.Ptr(Ptr,FunPtr,minusPtr)
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
import Data.Data (Data)
import Data.Word
import Data.Int
import Data.Bits
import Data.Hashable
import Data.List(unfoldr)
import Control.Monad(foldM,when)

import Foreign.Storable

#include <libbf.h>

import LibBF.Opts

-- | State of the current computation context.
newtype BFContext = BFContext (ForeignPtr BFContext)

foreign import ccall "bf_context_init_hs"
  bf_context_init_hs :: Ptr BFContext -> IO ()

foreign import ccall "&bf_context_end"
  bf_context_end :: FunPtr (Ptr BFContext -> IO ())

{-| Allocate a new numeric context. -}
newContext :: IO BFContext
newContext =
  do fptr <- mallocForeignPtrBytes #{size bf_context_t}
     withForeignPtr fptr bf_context_init_hs
     addForeignPtrFinalizer bf_context_end fptr
     pure (BFContext fptr)


-- | A mutable high precision floating point number.
newtype BF = BF (ForeignPtr BF)

foreign import ccall "bf_init"
  bf_init :: Ptr BFContext -> Ptr BF -> IO ()

foreign import ccall "&bf_delete_hs"
  bf_delete :: FunPtr (Ptr BF -> IO ())

{-| Allocate a new number.  Starts off as zero. -}
new :: BFContext -> IO BF
new (BFContext fctx) =
  withForeignPtr fctx \ctx ->
  do fptr <- mallocForeignPtrBytes #{size bf_t}
     withForeignPtr fptr (bf_init ctx)
     addForeignPtrFinalizer bf_delete fptr
     pure (BF fptr)

--------------------------------------------------------------------------------
-- FFI Helpers

signToC :: Sign -> CInt
signToC s = case s of
              Pos -> 0
              Neg -> 1

asSign :: CInt -> Sign
asSign s = if s == 0 then Pos else Neg

asBool :: CInt -> Bool
asBool = (/= 0)

asOrd :: CInt -> Ordering
asOrd x
  | x < 0     = LT
  | x > 0     = GT
  | otherwise = EQ


bf1 :: (Ptr BF -> IO a) -> BF -> IO a
bf1 f (BF fout) = withForeignPtr fout f

bfQuery :: (Ptr BF -> IO CInt) -> BF -> IO Bool
bfQuery f = bf1 (fmap asBool . f)

bfRel :: (Ptr BF -> Ptr BF -> IO CInt) -> BF -> BF -> IO Bool
bfRel f = bf2 \x y -> asBool <$> f y x

bfOrd :: (Ptr BF -> Ptr BF -> IO CInt) -> BF -> BF -> IO Ordering
bfOrd f = bf2 \x y -> asOrd <$> f y x

bf2 :: (Ptr BF -> Ptr BF -> IO a) -> BF -> BF -> IO a
bf2 f (BF fin1) (BF fout) =
  withForeignPtr fin1 \in1 ->
  withForeignPtr fout \out1 ->
    f out1 in1

bf3 :: (Ptr BF -> Ptr BF -> Ptr BF -> IO a) -> BF -> BF -> BF -> IO a
bf3 f (BF fin1) (BF fin2) (BF fout) =
  withForeignPtr fin1 \in1 ->
  withForeignPtr fin2 \in2 ->
  withForeignPtr fout \out ->
    f out in1 in2






--------------------------------------------------------------------------------
-- Assignment


-- | Indicates if a number is positive or negative.
data Sign = Neg {-^ Negative -} | Pos {-^ Positive -}
             deriving (Data,Eq,Ord,Show)


foreign import ccall "bf_set_nan"
  bf_set_nan :: Ptr BF -> IO ()

-- | Assign @NaN@ to the number.
setNaN :: BF -> IO ()
setNaN (BF fptr) = withForeignPtr fptr bf_set_nan


foreign import ccall "bf_set_zero"
  bf_set_zero :: Ptr BF -> CInt -> IO ()

-- | Assign a zero to the number.
setZero :: Sign -> BF -> IO ()
setZero sig = bf1 (`bf_set_zero` signToC sig)


foreign import ccall "bf_set_inf"
  bf_set_inf :: Ptr BF -> CInt -> IO ()

-- | Assign an infinty to the number.
setInf :: Sign -> BF -> IO ()
setInf sig = bf1 (`bf_set_inf` signToC sig)


foreign import ccall "bf_set_ui"
  bf_set_ui :: Ptr BF -> Word64 -> IO ()

{-| Assign from a word -}
setWord :: Word64 -> BF -> IO ()
setWord w = bf1 (`bf_set_ui` w)


foreign import ccall "bf_set_si"
  bf_set_si :: Ptr BF -> Int64 -> IO ()

{-| Assign from an int -}
setInt :: Int64 -> BF -> IO ()
setInt s = bf1 (`bf_set_si` s)

-- | Set an integer.  If the integer is larger than the primitive types,
-- this does repreated Int64 additions and multiplications.
setInteger :: Integer -> BF -> IO ()
setInteger n0 bf0
  | n0 >= 0 && n0 <= toInteger (maxBound :: Word64) =
    setWord (fromInteger n0) bf0
  | n0 < 0 && n0 >= toInteger (minBound :: Int64) =
    setInt (fromInteger n0) bf0
  | otherwise =
  do setZero Pos bf0
     go (abs n0) bf0
     when (n0 < 0) (fneg bf0)
  where
  chunk = toInteger (maxBound :: Int64) + 1

  go n bf
    | n == 0 = pure ()
    | otherwise =
      do let (next,this) = n `divMod` chunk
         go next bf
         Ok <- fmulWord infPrec bf (fromIntegral chunk) bf
         Ok <- faddInt  infPrec bf (fromIntegral this)  bf
         pure ()

-- | Chunk a non-negative integer into words,
-- least significatn first
toChunks :: Integer -> [LimbT]
toChunks = unfoldr step
  where
  step n = if n == 0 then Nothing
                     else Just (leastChunk n, n `shiftR` unit)

  unit = #{const LIMB_BITS} :: Int
  mask = (1 `shiftL` unit) - 1

  leastChunk :: Integer -> LimbT
  leastChunk n = fromIntegral (n .&. mask)



foreign import ccall "bf_set_float64"
  bf_set_float64 :: Ptr BF -> Double -> IO ()

{-| Assign from a double -}
setDouble :: Double -> BF -> IO ()
setDouble d = bf1 (`bf_set_float64` d)


foreign import ccall "bf_set"
  bf_set :: Ptr BF -> Ptr BF -> IO ()

{-| Assign from another number. -}
setBF :: BF -> BF {-^ This number is changed -} -> IO ()
setBF = bf2 (\out in1 -> bf_set out in1)


--------------------------------------------------------------------------------
-- Comparisons

foreign import capi "libbf.h bf_cmp_eq"
  bf_cmp_eq :: Ptr BF -> Ptr BF -> IO CInt

{-| Check if the two numbers are equal. -}
cmpEq :: BF -> BF -> IO Bool
cmpEq = bfRel bf_cmp_eq


foreign import capi "libbf.h bf_cmp_lt"
  bf_cmp_lt :: Ptr BF -> Ptr BF -> IO CInt

{-| Check if the first number is strictly less than the second. -}
cmpLT :: BF -> BF -> IO Bool
cmpLT = bfRel bf_cmp_lt


foreign import capi "libbf.h bf_cmp_le"
  bf_cmp_le :: Ptr BF -> Ptr BF -> IO CInt

{-| Check if the first number is less than, or equal to, the second. -}
cmpLEQ :: BF -> BF -> IO Bool
cmpLEQ = bfRel bf_cmp_le


foreign import ccall "bf_cmpu"
  bf_cmpu :: Ptr BF -> Ptr BF -> IO CInt

{-| Compare the absolute values of the two numbers. See also 'cmp'. -}
cmpAbs :: BF -> BF -> IO Ordering
cmpAbs = bfOrd bf_cmpu


foreign import ccall "bf_cmp_full"
  bf_cmp_full :: Ptr BF -> Ptr BF -> IO CInt

{-| Compare the two numbers.  The special values are ordered like this:

      * -0 < 0
      * NaN == NaN
      * NaN is larger than all other numbers
-}
cmp :: BF -> BF -> IO Ordering
cmp = bfOrd bf_cmp_full







foreign import capi "libbf.h bf_is_finite"
  bf_is_finite :: Ptr BF -> IO CInt

foreign import capi "libbf.h bf_is_nan"
  bf_is_nan :: Ptr BF -> IO CInt

foreign import capi "libbf.h bf_is_zero"
  bf_is_zero :: Ptr BF -> IO CInt

{-| Check if the number is "normal", i.e. (not infinite or NaN) -}
isFinite :: BF -> IO Bool
isFinite = bfQuery bf_is_finite

{-| Check if the number is NaN -}
isNaN :: BF -> IO Bool
isNaN = bfQuery bf_is_nan

{-| Check if the given number is a zero. -}
isZero :: BF -> IO Bool
isZero = bfQuery bf_is_zero

foreign import capi "libbf.h bf_neg"
  bf_neg :: Ptr BF -> IO ()

foreign import ccall "bf_add"
  bf_add :: Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_add_si"
  bf_add_si :: Ptr BF -> Ptr BF -> Int64 -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_sub"
  bf_sub :: Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_mul"
  bf_mul :: Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_mul_si"
  bf_mul_si :: Ptr BF -> Ptr BF -> Int64 -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_mul_ui"
  bf_mul_ui :: Ptr BF -> Ptr BF -> Word64 -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_mul_2exp"
  bf_mul_2exp :: Ptr BF -> SLimbT -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_div"
  bf_div :: Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_rem"
  bf_rem :: Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> CInt -> IO Status

foreign import ccall "bf_pow"
  bf_pow :: Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_round"
  bf_round :: Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_rint"
  bf_rint :: Ptr BF -> CInt -> IO Status

foreign import ccall "bf_sqrt"
  bf_sqrt :: Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status



bfArith :: (Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status) ->
           BFOpts -> BF -> BF -> BF -> IO Status
bfArith fun (BFOpts prec flags) (BF fa) (BF fb) (BF fr) =
  withForeignPtr fa \a ->
  withForeignPtr fb \b ->
  withForeignPtr fr \r ->
  fun r a b prec flags


-- | Negate the number.
fneg :: BF -> IO ()
fneg = bf1 bf_neg

-- | Add two numbers, using the given settings, and store the
-- result in the last.
fadd :: BFOpts -> BF -> BF -> BF -> IO Status
fadd = bfArith bf_add

-- | Add a number and an int64 and store the result in the last.
faddInt :: BFOpts -> BF -> Int64 -> BF -> IO Status
faddInt (BFOpts p f) x y z = bf2 (\out in1 -> bf_add_si out in1 y p f) x z

-- | Subtract two numbers, using the given settings, and store the
-- result in the last.
fsub :: BFOpts -> BF -> BF -> BF -> IO Status
fsub = bfArith bf_sub

-- | Multiply two numbers, using the given settings, and store the
-- result in the last.
fmul :: BFOpts -> BF -> BF -> BF -> IO Status
fmul = bfArith bf_mul

-- | Compute the fused-multiply-add.
--   @ffma opts x y z r@ computes @r := (x*y)+z@.
ffma :: BFOpts -> BF -> BF -> BF -> BF -> IO Status
ffma (BFOpts prec f) (BF x) (BF y) (BF z) (BF r) =
  withForeignPtr x \xp ->
  withForeignPtr y \yp ->
  withForeignPtr z \zp ->
  withForeignPtr r \out ->
    do s1 <- bf_mul out xp yp #{const BF_PREC_INF} #{const BF_RNDN}
       case s1 of
         MemError -> return s1
         _ ->
           do s2 <- bf_add out out zp prec f
              pure (s1 <> s2)

-- | Multiply the number by the given word, and store the result
-- in the second number.
fmulWord :: BFOpts -> BF -> Word64 -> BF -> IO Status
fmulWord (BFOpts p f) x y z = bf2 (\out in1 -> bf_mul_ui out in1 y p f) x z

-- | Multiply the number by the given int, and store the result
-- in the second number.
fmulInt :: BFOpts -> BF -> Int64 -> BF -> IO Status
fmulInt (BFOpts p f) x y z = bf2 (\out in1 -> bf_mul_si out in1 y p f) x z

-- | Multiply the number by @2^e@.
fmul2Exp :: BFOpts -> Int -> BF -> IO Status
fmul2Exp (BFOpts p f) e = bf1 (\out -> bf_mul_2exp out (fromIntegral e :: SLimbT) p f)

-- | Divide two numbers, using the given settings, and store the
-- result in the last.
fdiv :: BFOpts -> BF -> BF -> BF -> IO Status
fdiv = bfArith bf_div

-- | Compute the remainder @x - y * n@ where @n@ is the integer
--   nearest to @x/y@ (with ties broken to even values of @n@).
--   Output is written into the final argument.
frem :: BFOpts -> BF -> BF -> BF -> IO Status
frem (BFOpts p f) (BF fin1) (BF fin2) (BF fout) =
  withForeignPtr fin1 \in1 ->
  withForeignPtr fin2 \in2 ->
  withForeignPtr fout \out ->
    bf_rem out in1 in2 p f #{const BF_RNDN}

-- | Compute the square root of the first number and store the result
-- in the second.
fsqrt :: BFOpts -> BF -> BF -> IO Status
fsqrt (BFOpts p f) = bf2 (\res inp -> bf_sqrt res inp p f)

-- | Round to the nearest float matching the configuration parameters.
fround :: BFOpts -> BF -> IO Status
fround (BFOpts p f) = bf1 (\ptr -> bf_round ptr p f)

-- | Round to the neareset integer.
frint :: RoundMode -> BF -> IO Status
frint (RoundMode r) = bf1 (\ptr -> bf_rint ptr (fromIntegral r :: CInt))

-- | Exponentiate the first number by the second,
-- and store the result in the third number.
fpow :: BFOpts -> BF -> BF -> BF -> IO Status
fpow (BFOpts prec flags) = bf3 (\out in1 in2 -> bf_pow out in1 in2 prec flags)





--------------------------------------------------------------------------------
-- export

foreign import ccall "bf_get_float64"
  bf_get_float64 :: Ptr BF -> Ptr Double -> RoundMode -> IO Status

-- | Get the current value of a 'BF' as a Haskell `Double`.
toDouble :: RoundMode -> BF -> IO (Double, Status)
toDouble r = bf1 (\inp ->
  alloca (\out ->
   do s <- bf_get_float64 inp out r
      d <- peek out
      pure (d, s)
  ))


foreign import ccall "bf_atof"
  bf_atof ::
    Ptr BF -> CString -> Ptr CString -> CInt -> LimbT -> FlagsT -> IO CInt


{- | Set the value to the float parsed out of the given string.
  * The radix should not exceed 'LibBF.Opts.maxRadix'.
  * Sets the number to @NaN@ on failure.
  * Assumes that characters are encoded with a single byte each.
  * Retruns:
      - Status for the conversion
      - How many bytes we consumed
      - Did we consume the whole input
-}
setString :: Int -> BFOpts -> String -> BF -> IO (Status,Int,Bool)
setString radix (BFOpts prec flags) inStr =
  bf1    \bfPtr ->
  alloca \nextPtr ->
  withCAString inStr \strPtr ->
  do stat <- bf_atof bfPtr strPtr nextPtr (fromIntegral radix) prec flags
     next <- peek nextPtr
     let consumed = next `minusPtr` strPtr
         usedAll = length inStr == consumed
     consumed `seq` usedAll `seq` pure (Status stat, consumed, usedAll)


foreign import ccall "bf_ftoa"
  bf_ftoa :: Ptr CSize -> Ptr BF -> CInt -> LimbT -> FlagsT -> IO CString

-- | Render a big-float as a Haskell string.
-- The radix should not exceed 'LibBF.Opts.maxRadix'.
toString :: Int -> ShowFmt -> BF -> IO String
toString radix (ShowFmt ds flags) =
  bf1 \inp ->
  alloca \out ->
  do ptr <- bf_ftoa out inp (fromIntegral radix) ds flags
     len <- peek out
     if len > 0
       then
         do res <- peekCString ptr
            free ptr
            pure res
       else pure "(error)" -- XXX: throw an exception


-- | An explicit representation for big nums.
data BFRep  = BFRep !Sign !BFNum    -- ^ A signed number
            | BFNaN                 -- ^ Not a number
              deriving (Data,Eq,Ord,Show)

instance Hashable BFRep where
  hashWithSalt s BFNaN           = s `hashWithSalt` (0::Int)
  hashWithSalt s (BFRep Pos num) = s `hashWithSalt` (1::Int) `hashWithSalt` num
  hashWithSalt s (BFRep Neg num) = s `hashWithSalt` (2::Int) `hashWithSalt` num

-- | Representations for unsigned floating point numbers.
data BFNum  = Zero                 -- ^ zero
            | Num Integer !Int64   -- ^ @x * 2 ^ y@
            | Inf                  -- ^ infinity
              deriving (Data,Eq,Ord,Show)

instance Hashable BFNum where
  hashWithSalt s Zero         = s `hashWithSalt` (0::Int)
  hashWithSalt s (Num mag ex) = s `hashWithSalt` (1::Int) `hashWithSalt` mag `hashWithSalt` ex
  hashWithSalt s Inf          = s `hashWithSalt` (2::Int)

-- | Returns 'Nothing' for @NaN@.
getSign :: BF -> IO (Maybe Sign)
getSign = bf1 (\ptr ->
  do e <- #{peek bf_t, expn} ptr
     if (e :: SLimbT) == #{const BF_EXP_NAN}
        then pure Nothing
        else (Just . asSign) <$> #{peek bf_t, sign} ptr)

-- | Get the exponent of the number.
-- Returns 'Nothing' for inifinity, zero and NaN.
getExp :: BF -> IO (Maybe Int64)
getExp = bf1 (\ptr ->
  do e <- #{peek bf_t, expn} ptr
     pure $! if (e :: SLimbT) < #{const BF_EXP_INF} &&
                e > #{const BF_EXP_ZERO} then Just (fromIntegral e)
                                         else Nothing)

{-| Check if the given numer is infinite. -}
isInf :: BF -> IO Bool
isInf = bf1 (\ptr ->
  do e <- #{peek bf_t, expn} ptr
     if | (e :: SLimbT) == #{const BF_EXP_INF} -> pure True
        | otherwise -> pure False)

-- | Get the representation of the number.
toRep :: BF -> IO BFRep
toRep = bf1 (\ptr ->
  do s <- #{peek bf_t, sign} ptr
     let sgn = if asBool s then Neg else Pos
     e <- #{peek bf_t, expn} ptr :: IO SLimbT
     if | e == #{const BF_EXP_NAN}  -> pure BFNaN
        | e == #{const BF_EXP_INF}  -> pure (BFRep sgn Inf)
        | e == #{const BF_EXP_ZERO} -> pure (BFRep sgn Zero)
        | otherwise ->
        do l <- #{peek bf_t, len}  ptr
           p <- #{peek bf_t, tab}  ptr
           let len = fromIntegral (l :: Word64) :: Int
               -- This should not really limit precision as it counts
               -- number of Word64s (not bytes)

               step x i = do w <- peekElemOff p i
                             pure ((x `shiftL` 64) + fromIntegral (w :: Word64))

           base <- foldM step 0 (reverse (take len [ 0 .. ]))
           let bias = 64 * fromIntegral len

               -- `e :: SLimbT`, and we need it to be 64 bits in the code below.
               -- On 64-bit architectures, `SLimbT = Int64`, making this a
               -- no-op. On 32-bit architectures, `SLimbT = Int32`, so this code
               -- will extend `e` to 64 bits.
               eInt64 :: Int64
               eInt64 = fromIntegral e

               norm bs bi
                 | even bs    = norm (bs `shiftR` 1) (bi - 1)
                 | otherwise  = BFRep sgn (Num bs (eInt64 - bi))

           pure (norm base bias) -- (BFRep sgn (Num base (e - bias)))
  )
