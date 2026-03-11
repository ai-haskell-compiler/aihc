module Combinatorics.Battleship.Count.Frontier (
   T,
   Position,
   maxShipSize,

   Use(Blocked, Free, Vertical),
   blockBounded,
   empty,
   insertNew,
   isFree,
   dilate,
   lookup,
   reverse, Reverse,
   foldMap,
   toList,
   mapToVector,

   fromList,
   fromString,

   propDilate,
   propReverse4,
   propReverse5,
   propReverse6,
   propReverse7,
   propReverse8,
   propReverse9,
   propReverse10,
   ) where

import qualified Combinatorics.Battleship.Size as Size
import Combinatorics.Battleship.Size
         (Nat, Size(Size), N4, N5, N6, N7, N8, N9, N10, N11)

import qualified Foreign.Storable.Newtype as Store
import qualified Foreign.Storable as St
import Foreign.Storable (Storable, alignment, poke, peek, )

import Control.Applicative ((<$>), )

import qualified Data.StorableVector.Lazy.Builder as SVBuilder
import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import qualified Data.Monoid.HT as Mn
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR, complement, )
import Data.Word (Word32, Word64, )
import Data.Char (ord, chr, )

import Data.Monoid (Monoid, mempty, mappend, )
import Data.Function.HT (nest, )
import Data.Bool.HT (if', )

import qualified Test.QuickCheck as QC

import Prelude2010 hiding (lookup, reverse, )
import Prelude ()


type Position = Int

data Use =
     Free
   | Blocked
   | Vertical Int
   deriving (Eq, Show)

{- |
Efficient representation of a (Map Position Use).

We need it for description of a cut of a board with ships.

>    ....#.....
>    ....#.....
>    ..........
>    ####......
>    ..........
> ___...#...#..___
>    ##.#...#..
>    ...#......
>    ...#..###.
>    ..........

Say, we are constructing the board with ships beginning from the bottom.
Using information from the cut,
we want to know at what positions in the upper plane are ships allowed.
In the example the frontier at the cut would be

> fromString "xxx3x.x1x."

The numbers 3 and 1 denotes lengths of the parts of vertical ships below the cut.
The @x@'s denote blocked columns,
i.e. in these columns it is not allowed to place ships
immediately above the cut.
-}
newtype T w = Cons {decons :: Word32}
   deriving (Eq, Ord) -- for use as key in a Map


instance (Nat w) => Show (T w) where
   showsPrec prec x =
      showParen (prec>10) $
         showString "Frontier.fromString " .
         shows (toString x)

instance Storable (T w) where
   sizeOf = Store.sizeOf decons
   alignment = Store.alignment decons
   poke = Store.poke decons
   peek = Store.peek Cons


debug :: Bool
debug = False

{-# INLINE checkPos #-}
checkPos :: String -> Size w -> Position -> a -> a
checkPos name (Size size) pos =
   if' (debug && (pos<0 || size<=pos)) $
      error $ name ++ ": position " ++ show pos ++ " out of range"

{-# INLINE validUse #-}
validUse :: Use -> Bool
validUse use =
   case use of
      Vertical k -> 0<k && k<=maxShipSize
      _ -> True

{-# INLINE checkUse #-}
checkUse :: String -> Use -> a -> a
checkUse name use =
   if' (debug && not (validUse use)) $
      error $ name ++ ": invalid use " ++ show use

{-# INLINE checkFree #-}
checkFree :: String -> Bool -> a -> a
checkFree name free =
   if' (debug && not free) $
      error $ name ++ ": position not free"

bitsPerNumber :: Int
bitsPerNumber = 3

mask :: Word32
mask = shiftL 1 bitsPerNumber - 1

maxShipSize :: Int
maxShipSize = fromIntegral mask - 1

bitsFromUse :: Use -> Word32
bitsFromUse use =
   case use of
      Free -> 0
      Blocked -> mask
      Vertical n -> fromIntegral n

useFromBits :: Word32 -> Use
useFromBits bits =
   if' (bits==0) Free $
   if' (bits==mask) Blocked $
   Vertical $ fromIntegral bits

useFromChar :: Char -> Use
useFromChar c =
   case c of
      '.' -> Free
      'x' -> Blocked
      _ ->
         if' ('1'<=c && c<='9')
            (Vertical (ord c - ord '0'))
            (error $ "useFromChar: illegal charactor '" ++ c : "'")

empty :: T w
empty = Cons 0

fromList :: [Use] -> T w
fromList =
   Cons . foldl (.|.) 0 .
   zipWith (\pos x -> shiftL x (pos*bitsPerNumber)) [0..] .
   map bitsFromUse

fromString :: String -> T w
fromString =
   fromList . map useFromChar

sizeOf :: (Nat w) => T w -> Size w
sizeOf _ = Size.size

lookup :: (Nat w) => T w -> Position -> Use
lookup frontier@(Cons bits) pos =
   checkPos "Frontier.lookup" (sizeOf frontier) pos $ useFromBits $
   shiftR bits (pos*bitsPerNumber) .&. mask

isFree :: (Nat w) => T w -> Position -> Bool
isFree frontier pos =
   lookup frontier pos == Free

{- |
Only allowed at positions containing 'Free'.
-}
insertNew :: (Nat w) => Position -> Use -> T w -> T w
insertNew pos use frontier@(Cons bits) =
   let name = "Frontier.insertNew"
   in  checkPos name (sizeOf frontier) pos $ checkUse name use $
       checkFree name (isFree frontier pos) $
       Cons $ bits .|. shiftL (bitsFromUse use) (pos*bitsPerNumber)

{- |
Inserts at positions outside of the bounds are ignored.
You may overwrite Free or Blocked fields, but not Vertical ones.
-}
blockBounded :: (Nat w) => Size w -> Position -> T w -> T w
blockBounded (Size size) pos frontier@(Cons bits) =
   if' (pos<0 || size<=pos) frontier $
   if' (debug && case lookup frontier pos of Vertical _ -> True; _ -> False)
      (error $ "Frontier.insertBounded: tried to overwrite Vertical at position " ++ show pos)
      (Cons $ bits .|. shiftL (bitsFromUse Blocked) (pos*bitsPerNumber))


dilate :: Size w -> T w -> T w
dilate = dilateComb

dilateComb :: Size w -> T w -> T w
dilateComb (Size size) =
   let comb = replicateOne size 1
   in  \(Cons bits) ->
         let occupied = bits .|. shiftR bits 1 .|. shiftR bits 2
             additional =
               (shiftL occupied bitsPerNumber .|. shiftR occupied bitsPerNumber)
               .&.
               complement occupied
               .&.
               comb
         in  Cons $ bits .|.
               additional .|. shiftL additional 1 .|. shiftL additional 2

dilateGen :: (Nat w) => Size w -> T w -> T w
dilateGen width@(Size size) frontier =
   foldl (flip $ blockBounded width) frontier $
   filter (isFree frontier) $
   concatMap (\k -> Mn.when (k>0) [k-1] ++ Mn.when (k<size-1) [k+1]) $
   filter (not . isFree frontier) $ take size [0..]

propDilate :: QC.Property
propDilate =
   QC.forAll (QC.choose (0,10)) $ \n ->
   Size.reifyInt n
      (\size -> QC.forAllShrink QC.arbitrary QC.shrink $ propDilateTyped size)

propDilateTyped :: (Nat w) => Size w -> T w -> Bool
propDilateTyped size frontier =
   dilateComb size frontier == dilateGen size frontier

mapToVector ::
   (Nat w, Storable a) => Size w -> (Use -> a) -> T w -> SV.Vector a
mapToVector (Size size) f frontier =
   SV.sample size $ f . lookup frontier

_mapToVector ::
   (Nat w, Storable a) => Size w -> (Use -> a) -> T w -> SV.Vector a
_mapToVector (Size size) f =
   SV.concat . SVL.chunks .
   SVBuilder.toLazyStorableVector (SVL.chunkSize size) .
   foldMap (SVBuilder.put . f)


{- |
Can be faster than 'toList' since it does not build a list.
It ignores 'Free' squares at the end of the frontier.
-}
{-# INLINE foldMap #-}
foldMap :: (Monoid m) => (Use -> m) -> T w -> m
foldMap f =
   let go m bits =
         if bits==0
           then m
           else go (mappend m (f $ useFromBits $ bits .&. mask)) $
                shiftR bits bitsPerNumber
   in  go mempty . decons

toListWithSize :: (Nat w) => Size w -> T w -> [Use]
toListWithSize (Size width) frontier = map (lookup frontier) $ take width [0 ..]

toList :: (Nat w) => T w -> [Use]
toList = toListWithSize Size.size

charFromUse :: Use -> Char
charFromUse u =
   case u of
      Free -> '.'
      Blocked -> 'x'
      Vertical n ->
         if' (1<=n && n<=9)
            (chr (n + ord '0'))
            (error $ "charFromUse: illegal vertical number " ++ show n)

toString :: (Nat w) => T w -> String
toString = map charFromUse . toList


newtype Reverse w = Reverse {runReverse :: T w -> T w}
newtype Reverse1 w = Reverse1 {runReverse1 :: T (Size.P1 w) -> T (Size.P1 w)}
newtype Reverse2 w = Reverse2 {runReverse2 :: T (Size.P2 w) -> T (Size.P2 w)}
newtype Reverse3 w = Reverse3 {runReverse3 :: T (Size.P3 w) -> T (Size.P3 w)}
newtype Reverse4 w = Reverse4 {runReverse4 :: T (Size.P4 w) -> T (Size.P4 w)}
newtype Reverse5 w = Reverse5 {runReverse5 :: T (Size.P5 w) -> T (Size.P5 w)}
newtype Reverse6 w = Reverse6 {runReverse6 :: T (Size.P6 w) -> T (Size.P6 w)}
newtype Reverse7 w = Reverse7 {runReverse7 :: T (Size.P7 w) -> T (Size.P7 w)}
newtype Reverse8 w = Reverse8 {runReverse8 :: T (Size.P8 w) -> T (Size.P8 w)}
newtype Reverse9 w = Reverse9 {runReverse9 :: T (Size.P9 w) -> T (Size.P9 w)}
newtype Reverse10 w = Reverse10 {runReverse10 :: T (Size.P10 w) -> T (Size.P10 w)}

reverse :: Nat w => T w -> T w
reverse =
   runReverse $ Size.switch (Reverse id) $ Reverse $
   runReverse1 $ Size.switch (Reverse1 id) $ Reverse1 $
   runReverse2 $ Size.switch (Reverse2 $ reverseGen Size.size) $ Reverse2 $
   runReverse3 $ Size.switch (Reverse3 $ reverseGen Size.size) $ Reverse3 $
   runReverse4 $ Size.switch (Reverse4 reverse4spread) $ Reverse4 $
   runReverse5 $ Size.switch (Reverse5 reverse5spread) $ Reverse5 $
   runReverse6 $ Size.switch (Reverse6 reverse6up) $ Reverse6 $
   runReverse7 $ Size.switch (Reverse7 reverse7up) $ Reverse7 $
   runReverse8 $ Size.switch (Reverse8 reverse8up) $ Reverse8 $
   runReverse9 $ Size.switch (Reverse9 reverse9up) $ Reverse9 $
   runReverse10 $ Size.switch (Reverse10 reverse10up) $ Reverse10 $
   reverseGen Size.size


reverseGen :: Size w -> T w -> T w
reverseGen (Size size) (Cons bits) =
   Cons $ snd $
   nest size
      (\(src, dst) ->
         (shiftR src bitsPerNumber,
          shiftL dst bitsPerNumber .|. (mask .&. src)))
      (bits, 0)

{-# INLINE swap #-}
swap :: Int -> Word32 -> Word32 -> Word32
swap n m bits =
   shiftL (bits .&. m) (n*bitsPerNumber) .|.
   (shiftR bits (n*bitsPerNumber) .&. m)

reverse6up :: T N6 -> T N6
reverse6up (Cons bits0) =
   let bits1 = swap 1 0o070707 bits0
   in  Cons $  swap 4 0o000077 bits1  .|.  bits1 .&. 0o007700

reverse7up :: T N7 -> T N7
reverse7up (Cons bits0) =
   let bits1 = swap 2 0o0070007 bits0  .|.  bits0 .&. 0o0700070
   in  Cons $  swap 4 0o0000777 bits1  .|.  bits0 .&. 0o0007000

reverse8up :: T N8 -> T N8
reverse8up (Cons bits0) =
   let bits1 = swap 1 0o07070707 bits0
       bits2 = swap 2 0o00770077 bits1
   in  Cons $  swap 4 0o00007777 bits2

reverse9up :: T N9 -> T N9
reverse9up (Cons bits0) =
   let bits1 = swap 1 0o070700707 bits0
       bits2 = swap 2 0o007700077 bits1
   in  Cons $  swap 5 0o000007777 bits2  .|.  bits0 .&. 0o000070000

reverse10up :: T N10 -> T N10
reverse10up (Cons bits0) =
   let bits1 = swap 1 0o0707070707 bits0
       bits2 = swap 2 0o0077000077 bits1
   in  Cons $  swap 6 0o0000007777 bits2  .|.  bits1 .&. 0o0000770000

reverse10down :: T N10 -> T N10
reverse10down (Cons bits0) =
   let bits1 = swap 5 0o0000077777 bits0
       bits2 = swap 3 0o0007700077 bits1
   in  Cons $  swap 1 0o0700707007 bits2  .|.  bits1 .&. 0o0070000700

reverse11up :: T N11 -> T N11
reverse11up (Cons bits0) =
   let bits1 = swap 1 0o07007007007 bits0
       bits2 = swap 3 0o00077000077 bits1  .|.  bits0 .&. 0o00700000700
   in  Cons $  swap 6 0o00000077777 bits2  .|.  bits0 .&. 0o00000700000


reverse4spread :: T N4 -> T N4
reverse4spread (Cons bits) =
   Cons $
   let full = 0o7777
   in  if bits == full
         then full
         else fromIntegral $ mod ((bits * 0o10000010) .&. 0o7070070700) full
{-
dcba

0abc d00a bcd0
 |^  ^  ^  ^
-}

reverse5spread :: T N5 -> T N5
reverse5spread (Cons bits) =
   Cons $ fromIntegral $
   mod
      ((fromIntegral bits * 0o10000000100000001)
       .&. 0o70070007007000000700)
      (0o777777 :: Word64)
{-
      ((fromIntegral bits * 0o_010000_000100_000001)
       .&. 0o70 070007 007000 000700)

 edcba

000abc de000a bcde00 0abcde
   |^   ^   ^   ^       ^
-}

reverse10spread :: T N10 -> T N10
reverse10spread =
   let full = multiMask 10
       spread = shiftL (replicateOne 5 12) bitsPerNumber
       revMask = replicateOne 5 11 * 0o70000700000
   in  \(Cons bits) ->
       Cons $
       if bits == full
         then full
         else
            fromInteger $
            mod ((toInteger bits * spread) .&. revMask) (toInteger full)
{-
         jihgfedcba

...abcde fghij00abc defghij00a bcdefghij0
     ^     ^     ^     ^     ^     ^
-}

reverse10splitSpread :: T N10 -> T N10
reverse10splitSpread (Cons bits) =
   Cons $
      shiftL
         (decons $ reverse5spread $ Cons $ bits .&. 0o77777) (5*bitsPerNumber)
      .|.
      (decons $ reverse5spread $ Cons $ shiftR bits (5*bitsPerNumber))


{-# INLINE multiMask #-}
multiMask :: (Bits a, Integral a) => Int -> a
multiMask n = shiftL 1 (n*bitsPerNumber) - 1

{-# INLINE replicateOne #-}
replicateOne :: (Bits a, Integral a) => Int -> Int -> a
replicateOne n k = multiMask (n*k) `div` multiMask k



cons :: Size w -> Word32 -> T w
cons (Size width) bits = Cons $ multiMask width .&. bits

instance (Nat w) => QC.Arbitrary (T w) where
   arbitrary = cons Size.size <$> QC.choose (minBound, maxBound)
   shrink = map (cons Size.size) . QC.shrink . decons

propReverse4 :: T N4 -> Bool
propReverse4 frontier =
   reverseGen Size.size frontier == reverse4spread frontier

propReverse5 :: T N5 -> Bool
propReverse5 frontier =
   reverseGen Size.size frontier == reverse5spread frontier

propReverse6 :: T N6 -> Bool
propReverse6 frontier =
   reverseGen Size.size frontier == reverse6up frontier

propReverse7 :: T N7 -> Bool
propReverse7 frontier =
   reverseGen Size.size frontier == reverse7up frontier

propReverse8 :: T N8 -> Bool
propReverse8 frontier =
   reverseGen Size.size frontier == reverse8up frontier

propReverse9 :: T N9 -> Bool
propReverse9 frontier =
   reverseGen Size.size frontier == reverse9up frontier

propReverse10 :: T N10 -> Bool
propReverse10 frontier =
   reverseGen Size.size frontier == reverse10up frontier &&
   reverseGen Size.size frontier == reverse10down frontier &&
   reverseGen Size.size frontier == reverse10spread frontier &&
   reverseGen Size.size frontier == reverse10splitSpread frontier

-- too big for Word32
_propReverse11 :: T N11 -> Bool
_propReverse11 frontier =
   reverseGen Size.size frontier == reverse11up frontier
