{-
We could add checks for overflows
by checking whether the most significant bit of every part is zero.
-}
module Combinatorics.Battleship.Fleet (
   -- * basics
   T,
   ShipSize,
   NumberOfShips,

   cumulate,
   dec, inc,
   empty,
   fromList, toList,
   fromSizes, toSizes,
   lookup,
   maxSize,
   singleton,
   subset,

   -- * configurations for some established versions
   german,
   english,

   -- * tests
   propList,
   propSizes,
   propCumulate,
   propSubset,

   propInc,
   propDec,
   propIncDec,
   ) where

import qualified Foreign.Storable.Newtype as Store
import Foreign.Storable (Storable, sizeOf, alignment, poke, peek, )

import Data.Foldable (foldMap, )
import Data.Bool.HT (if', )

import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )
import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR, )
import Data.Word (Word32, )

import Prelude hiding (lookup)

import qualified Test.QuickCheck as QC


type ShipSize = Int
type NumberOfShips = Int

{- |
Efficient representation of a (Map ShipSize NumberOfShips).

This is known as SIMD within a register <https://en.wikipedia.org/wiki/SWAR>.
-}
newtype T = Cons {decons :: Word32}
   deriving (Eq, Ord) -- for use as key in a Map


instance Show T where
   showsPrec prec x =
      showParen (prec>10) $
         showString "Fleet.fromList " .
         shows (toList x)

instance Semigroup T where
   Cons x <> Cons y = Cons (x+y)

instance Monoid T where
   mempty = Cons 0
   mappend = (<>)

instance Storable T where
   sizeOf = Store.sizeOf decons
   alignment = Store.alignment decons
   poke = Store.poke decons
   peek = Store.peek Cons


debug :: Bool
debug = False

{-# INLINE checkSize #-}
checkSize :: String -> ShipSize -> a -> a
checkSize name size =
   if' (debug && (size<=0 || maxSize<size)) $
      error $ name ++ ": ship size " ++ show size ++ " out of range"

{-
The number of bits must be large enough
in order to also hold a cumulative fleet.
-}
bitsPerNumber :: Int
bitsPerNumber = 4

digitMask :: Word32
digitMask = shiftL 1 bitsPerNumber - 1

maxSize :: Int
maxSize = 8

bitPosFromSize :: Int -> Int
bitPosFromSize size =
   (size-1)*bitsPerNumber

empty :: T
empty = mempty

singleton :: ShipSize -> NumberOfShips -> T
singleton size n =
   checkSize "Fleet.singleton" size
   Cons $ shiftL (fromIntegral n) (bitPosFromSize size)

fromList :: [(ShipSize, NumberOfShips)] -> T
fromList = foldMap (uncurry singleton)

fromSizes :: [ShipSize] -> T
fromSizes = fromList . map (flip (,) 1)


lookup :: T -> ShipSize -> NumberOfShips
lookup (Cons bits) size =
   checkSize "Fleet.lookup" size $
   fromIntegral $
      shiftR bits (bitPosFromSize size)
      .&.
      digitMask

toList :: T -> [(ShipSize, NumberOfShips)]
toList fleet =
   filter ((0/=) . snd) $
   map (\size -> (size, lookup fleet size)) [1..maxSize]

toSizes :: T -> [ShipSize]
toSizes = concatMap (\(size,n) -> replicate n size) . toList


propList :: T -> Bool
propList fleet  =  fleet == fromList (toList fleet)

propSizes :: T -> Bool
propSizes fleet  =  fleet == fromSizes (toSizes fleet)


{- |
@lookup (cumulate fleet) size@
returns the number of all ships that are at least @size@ squares big.
-}
cumulate :: T -> T
cumulate = cumulateDiv

cumulateCascade :: T -> T
cumulateCascade (Cons x) =
   Cons $ foldl (\y n -> y + shiftR y n) x $
   takeWhile (< maxSize * bitsPerNumber) $ iterate (2*) bitsPerNumber

{- |
The total number ships must be strictly smaller than 15.
-}
cumulateDiv :: T -> T
cumulateDiv (Cons x) =
   Cons $
   case divMod x digitMask of
      (q,r) -> shiftL q bitsPerNumber .|. r

genBounded :: QC.Gen T
genBounded = do
   n <- QC.choose (0, fromIntegral digitMask - 1)
   fmap fromSizes $ QC.vectorOf n $ QC.choose (1, maxSize)

propCumulate :: QC.Property
propCumulate =
   QC.forAll genBounded $
      \x -> cumulateCascade x == cumulateDiv x


{-# INLINE subset #-}
subset :: T -> T -> Bool
subset = subsetParity

subsetLookup :: T -> T -> Bool
subsetLookup x y =
   all (\size -> lookup x size <= lookup y size) [1..maxSize]

{- |
This implementation checks whether unwanted borrows occurred.
@x<=y@ is required for the largest ship size.
-}
subsetParity :: T -> T -> Bool
subsetParity =
   let sizesPos =
         div (shiftL 1 (maxSize*bitsPerNumber) - 1) digitMask
   in  \(Cons x) (Cons y) ->
         x<=y  &&  xor (xor x y) (y-x) .&. sizesPos == 0

propSubset :: T -> T -> Bool
propSubset x y  =  subsetLookup x y == subsetParity x y


inc :: ShipSize -> T -> T
inc size (Cons fleet) =
   checkSize "Fleet.inc" size $
   Cons $ fleet + shiftL 1 (bitPosFromSize size)

dec :: ShipSize -> T -> T
dec size (Cons fleet) =
   checkSize "Fleet.inc" size $
   Cons $ fleet - shiftL 1 (bitPosFromSize size)


{- |
The main configuration given
in <https://de.wikipedia.org/wiki/Schiffe_versenken>.
-}
german :: T
german = fromList [(5,1), (4,2), (3,3), (2,4)]

{- |
The main configuration given
in <https://en.wikipedia.org/wiki/Battleship_(game)>.
-}
english :: T
english = fromList [(2,1), (3,2), (4,1), (5,1)]


genShipSize :: QC.Gen ShipSize
genShipSize = QC.choose (1, maxSize)

propInc :: T -> QC.Property
propInc fleet =
   QC.forAll genShipSize $ \size ->
   QC.forAll genShipSize $ \pos ->
      lookup fleet size < fromIntegral digitMask
      QC.==>
      lookup (inc size fleet) pos == lookup fleet pos + fromEnum (pos==size)

propDec :: T -> QC.Property
propDec fleet =
   QC.forAll genShipSize $ \size ->
   QC.forAll genShipSize $ \pos ->
      lookup fleet size > 0
      QC.==>
      lookup (dec size fleet) pos == lookup fleet pos - fromEnum (pos==size)

propIncDec :: T -> QC.Property
propIncDec fleet =
   QC.forAll genShipSize $ \size ->
      lookup fleet size < fromIntegral digitMask
      QC.==>
      dec size (inc size fleet) == fleet


instance QC.Arbitrary T where
   arbitrary = fmap Cons $ QC.choose (minBound, maxBound)
   shrink = map (fromSizes . filter (>0)) . QC.shrink . toSizes
