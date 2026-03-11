{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- NOTE: defined(MIN_VERSION_integer_gmp) == package configured with -fgmp

#if defined(MIN_VERSION_integer_gmp)
{-# LANGUAGE MagicHash #-}
#endif

-- |
-- Module    : Data.BitVector
-- Copyright : (c) 2012-2016 Iago Abal
--             (c) 2012-2013 HASLab & University of Minho
-- License   : BSD3
-- Maintainer: Iago Abal <mail@iagoabal.eu>
--
-- Bit-vector arithmetic inspired by SMT-LIB <http://smt-lib.org/>
-- and Cryptol <http://cryptol.net/>.
--
-- Bit-vectors are represented as a pair /size/ and /value/,
-- where sizes are of type 'Int' and values are 'Integer'.
--
-- * Bit-vectors are interpreted as unsigned integers
--   (i.e. natural numbers) except for some specific /signed/ operations.
--
-- * Most operations are in some way /size-polymorphic/ and, if required,
--   will perform padding to adjust the size of input bit-vectors.
--
-- For documentation purposes we will write @[n]k@ to denote a bit-vector
-- of size @n@ representing the natural number @k@.
module Data.BitVector
  ( -- * Bit-vectors
    BitVector
  , BV
  , size, width
  , nat, uint, int
    -- * Creation
  , nil
  , bitVec
  , bitVecs
  , ones, zeros
    -- * Test
  , isNat
  , isPos
    -- * Comparison
  , (==.), (/=.)
  , (<.), (<=.), (>.), (>=.)
  , slt, sle, sgt, sge
    -- * Indexing
  , (@.), index
  , (@@), extract
  , (@:)
  , (!.)
  , least, most
  , msb, lsb, msb1, lsb1
  -- * Arithmetic
  , signumI
  , pow
  , sdiv, srem, smod
  , lg2
  -- * List-like operations
  , (#), cat, append, concat
  , zeroExtend, signExtend
  , foldl, foldl_
  , foldr, foldr_
  , reverse, reverse_
  , replicate, replicate_
  , and, and_
  , or, or_
  , split
  , group, group_
  , join
  -- * Bitwise operations
  , module Data.Bits
  , not, not_
  , nand, nor, xnor
  , (<<.), shl, (>>.), shr, ashr
  , (<<<.), rol, (>>>.), ror
  -- * Conversion
  , fromBool
  , fromBits
  , toBits
  -- * Pretty-printing
  , showBin
  , showOct
  , showHex
  ) where

import           Control.Monad ( Monad(..), when )
import           Control.Exception ( assert )

import           Data.Bits
import           Data.Bool ( Bool(..), otherwise, (&&), (||))
import qualified Data.Bool as Bool
import           Data.Data ( Data )
import qualified Data.List as List
import           Data.Monoid ( Monoid(..) )
import           Data.Ord
#ifdef __GLASGOW_HASKELL__
import qualified Text.Read as R
#endif
import           Data.Typeable ( Typeable )

#if defined(MIN_VERSION_integer_gmp)
import qualified GHC.Integer.Logarithms as I
import           GHC.Prim ( (+#) )
import           GHC.Types ( Int(..) )
#else
import           Data.Int ( Int )
#endif

import           Prelude
  ( Char
  , Eq(..)
  , Enum(..), Num(..)
  , Integral(..), Integer
  , Maybe(..)
  , Real(..)
#if MIN_VERSION_base(4,11,0)
  , Semigroup(..)
#endif
  , Show(..), String
  , const
  , error
  , flip, fromIntegral
  , maxBound
  , snd
  , undefined
  , ($), (.), (^), (++)
  )

{-# DEPRECATED foldl_, foldr_, reverse_, replicate_, and_, or_, group_, not_ "Use corresponding versions without underscore" #-}
{-# DEPRECATED cat "Use (#) or append instead" #-}

----------------------------------------------------------------------
--- Bit-vectors

-- | Big-endian /pseudo size-polymorphic/ bit-vectors.
data BV
    = BV {
      size :: !Int      -- ^ The /size/ of a bit-vector.
    , nat  :: !Integer  -- ^ The value of a bit-vector, as a natural number.
    }
  deriving (Data,Typeable)

-- | An alias for 'BV'.
type BitVector = BV

-- | An alias for 'size'.
width :: BV -> Int
width = size
{-# INLINE width #-}

-- | An alias for 'nat'.
uint :: BV -> Integer
uint = nat
{-# INLINE uint #-}

-- | 2's complement value of a bit-vector.
--
-- >>> int [2]3
-- -1
--
-- >>> int [4]12
-- -4
int :: BV -> Integer
int u | msb u     = - nat(-u)
      | otherwise = nat u
{-# INLINE int #-}

instance Show BV where
  show (BV n a) = "[" ++ show n ++ "]" ++ show a

#ifdef __GLASGOW_HASKELL__
instance R.Read BV where
  readPrec = do
    R.Punc "[" <- R.lexP
    n <- R.step R.readPrec
    when (n < 0) R.pfail
    R.Punc "]" <- R.lexP
    a <- R.step R.readPrec
    when (a < 0) R.pfail
    return (bitVec (n::Int) (a::Integer))
#endif

----------------------------------------------------------------------
--- Safety checking & Errors

checkBounds :: Bool
#if CHECK_BOUNDS
checkBounds = True
#else
checkBounds = False
#endif

check :: Bool -> Bool
check c = checkBounds && c
{-# INLINE check #-}

----------------------------------------------------------------------
--- Construction

-- | The /empty/ bit-vector, ie. @[0]0@.
nil :: BV
nil = BV 0 0
{-# INLINE nil #-}

-- | Create a bit-vector given a size and an integer value.
--
-- >>> bitVec 4 3
-- [4]3
--
-- This function also handles negative values.
--
-- >>> bitVec 4 (-1)
-- [4]15
bitVec :: Integral a => Int -> a -> BV
bitVec n a | n < 0     = error "Data.BitVector.bitVec: negative size"
           | a >= 0    = BV n (a' `mod` 2^n)
           | otherwise = negate $ BV n ((-a') `mod` 2^n)
  where a' = fromIntegral a
{-# INLINE bitVec #-}

-- | List of bit-vector literals of the same size
--
-- When a list of integer literals is interpreted as a list of bit-vectors,
-- 'fromInteger' is applied to each element invidually:
--
-- >>> [1,3,5] :: [BV]
-- [ [1]1, [2]3, [3]5 ]
--
-- Sometimes we want to specify a list of bit-vectors literals of the same
-- size, and for that we can use 'bitVects':
--
-- >>> bitVecs 3 [1,3,5]
-- [ [3]1, [3]3, [3]5 ]
bitVecs :: Integral a => Int -> [a] -> [BV]
bitVecs = List.map . bitVec
{-# INLINE bitVecs #-}

-- | Create a mask of ones.
ones :: Int -> BV
ones n | n < 0     = error "Data.BitVector.ones: negative size"
       | otherwise = BV n (2^n - 1)
{-# INLINE ones #-}

-- | Create a mask of zeros.
zeros :: Int -> BV
zeros n | n < 0     = error "Data.BitVector.zeros: negative size"
        | otherwise = BV n 0
{-# INLINE zeros #-}

----------------------------------------------------------------------
--- Test

-- | Test if the signed value of a bit-vector is a natural number.
isNat :: BV -> Bool
isNat = Bool.not . msb
{-# INLINE isNat #-}

-- | Test if the signed value of a bit-vector is a positive number.
isPos :: BV -> Bool
isPos a = int(a) > 0
{-# INLINE isPos #-}

----------------------------------------------------------------------
--- Comparison

infix 4 ==., /=., <., <=., >., >=.
infix 4 `slt`, `sle`, `sgt`, `sge`

instance Eq BV where
  (BV _ a) == (BV _ b) = a == b
  {-# INLINE (==) #-}

instance Ord BV where
  compare = comparing nat
  {-# INLINE compare #-}

-- | Fixed-size equality.
--
-- In contrast with '==', which is /size-polymorphic/, this equality
-- requires both bit-vectors to be of equal size.
--
-- >>> [n]k ==. [m]k
-- False
--
-- >>> [n]k ==. [n]k
-- True
(==.) :: BV -> BV -> Bool
(BV n a) ==. (BV m b) = n == m && a == b
{-# INLINE (==.) #-}

-- | Fixed-size inequality.
--
-- The negated version of '==.'.
(/=.) :: BV -> BV -> Bool
u /=. v = Bool.not $ u ==. v
{-# INLINE (/=.) #-}

-- | Fixed-size /less-than/.
(<.) :: BV -> BV -> Bool
(BV n a) <. (BV m b) = n == m && a < b
{-# INLINE (<.) #-}

-- | Fixed-size /less-than-or-equals/.
(<=.) :: BV -> BV -> Bool
(BV n a) <=. (BV m b) = n == m && a <= b
{-# INLINE (<=.) #-}

-- | Fixed-size /greater-than/.
(>.) :: BV -> BV -> Bool
(BV n a) >. (BV m b) = n == m && a > b
{-# INLINE (>.) #-}

-- | Fixed-size /greater-than-or-equals/.
(>=.) :: BV -> BV -> Bool
(BV n a) >=. (BV m b) = n == m && a >= b
{-# INLINE (>=.) #-}

-- | Fixed-size signed /less-than/.
slt :: BV -> BV -> Bool
u@BV{size=n} `slt` v@BV{size=m} = n == m && int u < int v
{-# INLINE slt #-}

-- | Fixed-size signed /less-than-or-equals/.
sle :: BV -> BV -> Bool
u@BV{size=n} `sle` v@BV{size=m} = n == m && int u <= int v
{-# INLINE sle #-}

-- | Fixed-size signed /greater-than/.
sgt :: BV -> BV -> Bool
u@BV{size=n} `sgt` v@BV{size=m} = n == m && int u > int v
{-# INLINE sgt #-}

-- | Fixed-size signed /greater-than-or-equals/.
sge :: BV -> BV -> Bool
u@BV{size=n} `sge` v@BV{size=m} = n == m && int u >= int v
{-# INLINE sge #-}

----------------------------------------------------------------------
--- Indexing

infixl 9 @., @@, @:, !.

-- | Bit indexing.
--
-- @u \@. i@ stands for the /i/-th bit of /u/.
--
-- >>> [4]2 @. 0
-- False
--
-- >>> [4]2 @. 1
-- True
(@.) :: Integral ix => BV -> ix -> Bool
(BV n a) @. i | check(i' < 0 || n <= i') = error "Data.BitVector.(@.): index of out bounds"
              | otherwise                = testBit a i'
  where i' = fromIntegral i
{-# INLINE (@.) #-}

-- | @index i a == a \@. i@
index :: Integral ix => ix -> BV -> Bool
index = flip (@.)
{-# INLINE index #-}

-- | Bit-string extraction.
--
-- @u \@\@ (j,i) == fromBits (map (u \@.) [j,j-1..i])@
--
-- >>> [4]7 @@ (3,1)
-- [3]3
(@@) :: Integral ix => BV -> (ix,ix) -> BV
(BV _ a) @@ (j,i) | check(i < 0 || j < i) = error "Data.BitVector.(@@): invalid range"
                  | otherwise             = BV m $ (a `shiftR` i') `mod` 2^m
  where i' = fromIntegral i
        m  = fromIntegral $ j - i + 1
{-# INLINE (@@) #-}

-- | @extract j i a == a \@\@ (j,i)@
extract :: Integral ix => ix -> ix -> BV -> BV
extract j i = (@@ (j,i))
{-# INLINE extract #-}

-- | Bit list indexing.
--
-- prop> u @: is ==. fromBits $ List.map (u @.) is
(@:) :: Integral ix => BV -> [ix] -> BV
(BV n a) @: is = fromBits $ List.map testBitAux is
  -- NB: Failing _late_ (the bounds check is done by 'testBitAux') avoids
  -- duplicating calls to 'fromIntegral' **and** this code should allow GHC
  -- to fuse 'fodlr' (from inlining 'frombits') with 'map'.
  where testBitAux i
          | check(i' < 0 || n <= i') = error "Data.BitVector.(@:): index out of bounds"
          | otherwise                = testBit a i'
          where i' = fromIntegral i
{-# INLINE (@:) #-}

-- | Reverse bit-indexing.
--
-- Index starting from the most significant bit.
--
-- @u !. i == u \@. (size u - i - 1) @
--
-- >>> [3]3 !. 0
-- False
(!.) :: Integral ix => BV -> ix -> Bool
(BV n a) !. i | check(i' < 0 || n <= i') = error "Data.BitVector.(!.): index out of bounds"
              | otherwise                = testBit a (n-i'-1)
  where i' = fromIntegral i
{-# INLINE (!.) #-}

-- | Take least significant bits.
--
-- @least m u == u \@\@ (m-1,0)@
least :: Integral ix => ix -> BV -> BV
least m (BV _ a) | check(m' < 1) = error "Data.BitVector.least: non-positive index"
                 | otherwise     = BV m' $ a `mod` 2^m
  where m' = fromIntegral m
{-# INLINE least #-}

-- | Take most significant bits.
--
-- @most m u == u \@\@ (n-1,n-m)@
most :: Integral ix => ix -> BV -> BV
most m (BV n a) | check(m' < 1) = error "Data.BitVector.most: non-positive index"
                | check(m' > n) = error "Data.BitVector.most: index out of bounds"
                | otherwise     = BV m' $ a `shiftR` (n-m')
  where m' = fromIntegral m
{-# INLINE most #-}

-- | Most significant bit.
--
-- @msb u == u !. 0@
msb :: BV -> Bool
msb = (!. (0::Int))
{-# INLINE msb #-}

-- | Least significant bit.
--
-- @lsb u == u \@. 0@
lsb :: BV -> Bool
lsb = (@. (0::Int))
{-# INLINE lsb #-}

-- | Most significant 1-bit.
--
-- /Pre/: input must be non-zero.
--
-- >>> msb1 [4]2
-- 1
--
-- >>> msb1 [4]4
-- 2
msb1 :: BV -> Int
msb1 (BV _ 0) = error "Data.BitVector.msb1: zero bit-vector"
msb1 (BV n a) = go (n-1)
  where go i | testBit a i = i
             | otherwise   = go (i-1)

-- | Least significant 1-bit.
--
-- /Pre/: input must be non-zero.
--
-- >>> msb1 [4]3
-- 0
--
-- >>> msb1 [4]6
-- 1
lsb1 :: BV -> Int
lsb1 (BV _ 0) = error "Data.BitVector.lsb1: zero bit-vector"
lsb1 (BV _ a) = go 0
  where go i | testBit a i = i
             | otherwise   = go (i+1)

----------------------------------------------------------------------
--- Arithmetic

instance Num BV where
  (BV n1 a) + (BV n2 b) = BV n $ (a + b) `mod` 2^n
    where n = max n1 n2
  {-# INLINE (+) #-}
  (BV n1 a) * (BV n2 b) = BV n $ (a * b) `mod` 2^n
    where n = max n1 n2
  {-# INLINE (*) #-}
  negate u@(BV _ 0) = u
  negate   (BV n a) = BV n $ 2^n - a
  {-# INLINE negate #-}
  abs u | msb u     = negate u
        | otherwise = u
  {-# INLINE abs #-}
  signum u = bitVec 2 $ signum $ int u
  {-# INLINE signum #-}
  fromInteger !i = bitVec n i
    where !n = if i >= 0 then integerWidth i + 1 else integerWidth i
  {-# INLINE fromInteger #-}

-- | Bit-vector 'signum' as an 'Integral'.
signumI :: Integral a => BV -> a
signumI = fromInteger . signum . int

instance Real BV where
  toRational = toRational . nat

instance Enum BV where
  toEnum = fromIntegral
  fromEnum (BV _ a) = assert (a < max_int) $ fromIntegral a
    where max_int = toInteger (maxBound::Int)

instance Integral BV where
  quotRem (BV n1 a) (BV n2 b) = (BV n q,BV n r)
    where n = max n1 n2
          (q,r) = quotRem a b
  {-# INLINE quotRem #-}
  divMod = quotRem
  {-# INLINE divMod #-}
  toInteger = int
  {-# INLINE toInteger #-}

-- | Bit-vector exponentiation.
--
-- @pow [n]k e@ computes @k@ raised to @e@ modulo @n@.
--
-- This is faster than Haskell's (^) operator because it performs
-- modulo division just once. Besides, @a^0 == [1]0@ !!!
pow :: Integral exp => BV -> exp -> BV
pow (BV n a) e = BV n (a^e `mod` m)
  where m = 2^n
{-# INLINE pow #-}

-- | 2's complement signed division.
sdiv :: BV -> BV -> BV
sdiv u@(BV n1 _) v@(BV n2 _) = bitVec n q
  where n = max n1 n2
        q = int u `quot` int v
{-# INLINE sdiv #-}

-- | 2's complement signed remainder (sign follows dividend).
srem :: BV -> BV -> BV
srem u@(BV n1 _) v@(BV n2 _) = bitVec n r
  where n = max n1 n2
        r = int u `rem` int v
{-# INLINE srem #-}

-- | 2's complement signed remainder (sign follows divisor).
smod :: BV -> BV -> BV
smod u@(BV n1 _) v@(BV n2 _) = bitVec n r
  where n = max n1 n2
        r = int u `mod` int v
{-# INLINE smod #-}

-- | Ceiling logarithm base 2.
--
-- /Pre/: input bit-vector must be non-zero.
lg2 :: BV -> BV
lg2 (BV _ 0) = error "Data.BitVector.lg2: zero bit-vector"
lg2 (BV n 1) = BV n 0
#if defined(MIN_VERSION_integer_gmp)
lg2 (BV n a) = BV n (toInteger a')
  where a' = I# (I.integerLog2# a)
#else
lg2 (BV n a) = BV n $ go 0 1
  where go !k !b | b == a    = k
                 | b > a     = k-1
                 | otherwise = go (k+1) (2*b)
#endif
{-# INLINE lg2 #-}

----------------------------------------------------------------------
--- List-like operations

infixr 5 #

-- | Concatenation of two bit-vectors.
(#), cat, append :: BV -> BV -> BV
(BV n a) # (BV m b) = BV (n + m) ((a `shiftL` m) + b)
{-# INLINE (#) #-}

cat = (#)
{-# INLINE cat #-}

append = (#)
{-# INLINE append #-}

-- | An alias for 'join'.
concat :: [BV] -> BV
concat = join

-- This is the most sensible monoid instance until we have size types!

instance Monoid BV where
  mempty  = nil
  {-# INLINE mempty #-}
  mconcat = join
  {-# INLINE mconcat #-}
#if !MIN_VERSION_base(4,11,0)
  mappend = (#)
  {-# INLINE mappend #-}
#else

instance Semigroup BV where
  (<>) = (#)
  {-# INLINE (<>) #-}
#endif

-- | Logical extension.
--
-- >>> zeroExtend 3 [1]1
-- [4]1
zeroExtend :: Integral size => size -> BV -> BV
zeroExtend d (BV n a) = BV (n+d') a
  where d' = fromIntegral d
{-# INLINE zeroExtend #-}

-- | Arithmetic extension.
--
-- >>> signExtend 2 [2]1
-- [4]1
--
-- >>> signExtend 2 [2]3
-- [4]15
signExtend :: Integral size => size -> BV -> BV
signExtend d (BV n a)
  | testBit a (n-1) = BV (n+d') $ (maxNat d `shiftL` n) + a
  | otherwise       = BV (n+d') a
  where d' = fromIntegral d
{-# INLINE signExtend #-}

-- |
-- @foldl f z (fromBits [un, ..., u1, u0]) == ((((z \`f\` un) \`f\` ...) \`f\` u1) \`f\` u0)@
--
-- @foldl f e = fromBits . foldl f e . toBits@
foldl, foldl_ :: (a -> Bool -> a) -> a -> BV -> a
foldl f e (BV n a) = go (n-1) e
  where go i !x | i >= 0    = let !b = testBit a i in go (i-1) $ f x b
                | otherwise = x
foldl_ = foldl
{-# INLINE foldl #-}

-- |
-- @foldr f z (fromBits [un, ..., u1, u0]) == un \`f\` (... \`f\` (u1 \`f\` (u0 \`f\` z)))@
--
-- @foldr f e = fromBits . foldr f e . toBits@
foldr, foldr_ :: (Bool -> a -> a) -> a -> BV -> a
foldr f e (BV n a) = go (n-1) e
 where go i !x | i >= 0    = let !b = testBit a i in f b (go (i-1) x)
               | otherwise = x
foldr_ = foldr
{-# INLINE foldr #-}

-- |
-- @reverse == fromBits . reverse . toBits@
reverse, reverse_ :: BV -> BV
reverse bv@(BV n _) = BV n $ snd $ foldl go (1,0) bv
  where go (v,acc) b | b         = (v',acc+v)
                     | otherwise = (v',acc)
          where v' = 2*v
reverse_ = reverse
{-# INLINE reverse #-}

-- |
-- /Pre/: if @replicate_ n u@ then @n > 0@ must hold.
--
-- @replicate_ n == fromBits . concat . replicate n . toBits @
replicate, replicate_ :: Integral size => size -> BV -> BV
replicate 0 _ = error "Data.BitVector.replicate: cannot replicate 0-times"
replicate n u = go (n-1) u
  where go 0 !acc = acc
        go k !acc = go (k-1) (u # acc)
replicate_ = replicate
{-# INLINE replicate #-}

-- | Conjunction.
--
-- Essentially, @and == foldr1 (.&.)@.
--
-- Returns @[1]1@ if the input list is empty.
and, and_ :: [BV] -> BV
and [] = ones 1
and ws = BV n' $ List.foldl1' (.&.) $ List.map nat ws
  where n' = List.maximum $ List.map size ws
and_ = and
{-# INLINE and #-}

-- | Disjunction.
--
-- Essentially, @or == foldr1 (.|.)@.
--
-- Returns @[1]0@ if the input list is empty.
or, or_ :: [BV] -> BV
or [] = zeros 1
or ws = BV n' $ List.foldl1' (.|.) $ List.map nat ws
  where n' = List.maximum $ List.map size ws
or_ = or
{-# INLINE or #-}

-- | Split a bit-vector /k/ times.
--
-- >>> split 3 [4]15
-- [[2]0,[2]3,[2]3]
split :: Integral times => times -> BV -> [BV]
split k (BV n a) | k <= 0    = error "Data.BitVector.split: non-positive splits"
                 | otherwise = List.map (BV s) $ splitInteger s k' a
  where k' = fromIntegral k
        (q,r) = divMod n k'
        s = q + signum r
{-# INLINE split #-}

-- | Split a bit-vector into /n/-wide pieces.
--
-- >>> group 3 [4]15
-- [[3]1,[3]7]
group, group_ :: Integral size => size -> BV -> [BV]
group s (BV n a) | s <= 0    = error "Data.BitVector.group: non-positive size"
                 | otherwise = List.map (BV s') $ splitInteger s' k a
  where s' = fromIntegral s
        (q,r) = divMod n s'
        k = q + signum r
group_ = group
{-# INLINE group #-}

splitInteger :: (Integral size, Integral times) =>
                    size -> times -> Integer -> [Integer]
splitInteger n = go []
  where n' = fromIntegral n
        go acc 0 _ = acc
        go acc k a = go (v:acc) (k-1) a'
          where v  = a `mod` 2^n
                a' = a `shiftR` n'
{-# INLINE splitInteger #-}

-- | Concatenate a (possibly empty) list of bit-vectors.
--
-- >>> join [[2]3,[2]2]
-- [4]14
join :: [BV] -> BV
join = List.foldl' (#) nil
{-# INLINE join #-}

----------------------------------------------------------------------
--- Bitwise operations

infixl 8 <<., `shl`, >>., `shr`, `ashr`, <<<., `rol`, >>>., `ror`

instance Bits BV where
  (BV n1 a) .&. (BV n2 b) = BV n $ a .&. b
    where n = max n1 n2
  {-# INLINE (.&.) #-}
  (BV n1 a) .|. (BV n2 b) = BV n $ a .|. b
    where n = max n1 n2
  {-# INLINE (.|.) #-}
  (BV n1 a) `xor` (BV n2 b) = BV n $ a `xor` b
    where n = max n1 n2
  {-# INLINE xor #-}
  complement (BV n a) = BV n $ 2^n - 1 - a
  {-# INLINE complement #-}
#if MIN_VERSION_base(4,7,0)
  zeroBits = BV 1 0
  {-# INLINE zeroBits #-}
#endif
  bit i = BV (i+1) (2^i)
  {-# INLINE bit #-}
  testBit (BV n a) i | i < n     = testBit a i
                     | otherwise = False
  {-# INLINE testBit #-}
  bitSize = undefined
#if MIN_VERSION_base(4,7,0)
  bitSizeMaybe = const Nothing
#endif
  isSigned = const False
  shiftL (BV n a) k
    | k > n     = BV n 0
    | otherwise = BV n $ shiftL a k `mod` 2^n
  {-# INLINE shiftL #-}
  shiftR (BV n a) k
    | k > n     = BV n 0
    | otherwise = BV n $ shiftR a k
  {-# INLINE shiftR #-}
  rotateL bv       0 = bv
  rotateL (BV n a) k
    | k == n    = BV n a
    | k > n     = rotateL (BV n a) (k `mod` n)
    | otherwise = BV n $ h + l
    where s = n - k
          l = a `shiftR` s
          h = (a `shiftL` k) `mod` 2^n
  {-# INLINE rotateL #-}
  rotateR bv       0 = bv
  rotateR (BV n a) k
    | k == n    = BV n a
    | k > n     = rotateR (BV n a) (k `mod` n)
    | otherwise = BV n $ h + l
    where s = n - k
          l = a `shiftR` k
          h = (a `shiftL` s) `mod` 2^n
  {-# INLINE rotateR #-}
  popCount (BV _ a) = assert (a >= 0) $ popCount a
  {-# INLINE popCount #-}

-- | An alias for 'complement'.
not, not_ :: BV -> BV
not = complement
not_ = not
{-# INLINE not #-}

-- | Negated '.&.'.
nand :: BV -> BV -> BV
nand u v = not $ u .&. v
{-# INLINE nand #-}

-- | Negated '.|.'.
nor :: BV -> BV -> BV
nor u v = not $ u .|. v
{-# INLINE nor #-}

-- | Negated 'xor'.
xnor :: BV -> BV -> BV
xnor u v = not $ u `xor` v
{-# INLINE xnor #-}

-- | Left shift.
(<<.), shl :: BV -> BV -> BV
bv@BV{size=n} <<. (BV _ k)
  | k >= fromIntegral n  = BV n 0
  | otherwise            = bv `shiftL` (fromIntegral k)
{-# INLINE (<<.) #-}

shl = (<<.)
{-# INLINE shl #-}

-- | Logical right shift.
(>>.), shr :: BV -> BV -> BV
bv@BV{size=n} >>. (BV _ k)
  | k >= fromIntegral n  = BV n 0
  | otherwise            = bv `shiftR` (fromIntegral k)
{-# INLINE (>>.) #-}

shr = (>>.)
{-# INLINE shr #-}

-- | Arithmetic right shift
ashr :: BV -> BV -> BV
ashr u v | msb u     = not ((not u) >>. v)
         | otherwise = u >>. v

-- | Rotate left.
(<<<.), rol :: BV -> BV -> BV

bv@BV{size=n} <<<. (BV _ k)
  | k >= n'   = bv `rotateL` (fromIntegral $ k `mod` n')
  | otherwise = bv `rotateL` (fromIntegral k)
  where n' = fromIntegral n
{-# INLINE (<<<.) #-}

rol = (<<<.)
{-# INLINE rol #-}

-- | Rotate right.
(>>>.), ror :: BV -> BV -> BV

bv@BV{size=n} >>>. (BV _ k)
  | k >= n'   = bv `rotateR` (fromIntegral $ k `mod` n')
  | otherwise = bv `rotateR` (fromIntegral k)
  where n' = fromIntegral n
{-# INLINE (>>>.) #-}

ror = (>>>.)
{-# INLINE ror #-}

----------------------------------------------------------------------
--- Conversion

-- | Create a bit-vector from a single bit.
fromBool :: Bool -> BV
fromBool False = BV 1 0
fromBool True  = BV 1 1
{-# INLINE fromBool #-}

-- | Create a bit-vector from a big-endian list of bits.
--
-- >>> fromBits [False, False, True]
-- [3]1
fromBits :: [Bool] -> BV
fromBits bs =
  let (n,k) = List.foldr go (0,0) bs in
  BV n k
  -- NB: 'setBit' is a GMP function, faster than regular addition.
  where go b (!i,!v) | b         = (i+1,setBit v i)
                     | otherwise = (i+1,v)
{-# INLINE fromBits #-}

-- | Create a big-endian list of bits from a bit-vector.
--
-- >>> toBits [4]11
-- [True, False, True, True]
toBits :: BV -> [Bool]
toBits (BV n a) = List.map (testBit a) [n-1,n-2..0]
{-# INLINE toBits #-}

----------------------------------------------------------------------
--- Pretty-printing

-- | Show a bit-vector in binary form.
showBin :: BV -> String
showBin = ("0b" ++) . List.map showBit . toBits
  where showBit True  = '1'
        showBit False = '0'

hexChar :: Integral a => a -> Char
hexChar 0 = '0'
hexChar 1 = '1'
hexChar 2 = '2'
hexChar 3 = '3'
hexChar 4 = '4'
hexChar 5 = '5'
hexChar 6 = '6'
hexChar 7 = '7'
hexChar 8 = '8'
hexChar 9 = '9'
hexChar 10 = 'a'
hexChar 11 = 'b'
hexChar 12 = 'c'
hexChar 13 = 'd'
hexChar 14 = 'e'
hexChar 15 = 'f'
hexChar _  = error "Data.BitVector.hexChar: invalid input"

-- | Show a bit-vector in octal form.
showOct :: BV -> String
showOct = ("0o" ++) . List.map (hexChar . nat) . group (3::Int)

-- | Show a bit-vector in hexadecimal form.
showHex :: BV -> String
showHex = ("0x" ++) . List.map (hexChar . nat) . group (4::Int)

----------------------------------------------------------------------
--- Utilities

-- | Greatest natural number representable with /n/ bits.
maxNat :: Integral size => size -> Integer
maxNat n = 2^n - 1
{-# INLINE maxNat #-}

-- | Minimum width of a bit-vector to represent a given integer number.
--
-- >>> integerWidth 4
-- 3
--
-- >>> integerWidth (-4)
-- 4
integerWidth :: Integer -> Int
#if defined(MIN_VERSION_integer_gmp)
integerWidth !n
  | n >= 0    = I# (I.integerLog2# n +# 1#)
  | otherwise = I# (I.integerLog2# (-n) +# 2#)
#else
integerWidth !n
  | n >= 0    = go 1 1
  | otherwise = 1 + integerWidth (abs n)
  where go !k !k_max | k_max >= n = k
                     | otherwise  = go (k+1) (2*k_max+1)
#endif
{-# INLINE integerWidth #-}
