{- |
In this module we generate for every line all possible layouts of bricks.
This leads to a big number of sets,
but still allows for the fastest solution
and a minimum number of solution steps.
The solver tends to need very few guesses.
-}
module Nonogram.Encoding.Combinatoric
         (Item, assigns, assignsBW, bitAssigns, intSetAssigns, bitVectorAssigns) where

import qualified Nonogram.Base as Base
import Nonogram.Base
         (Strip(Strip), strip, Orientation(Horizontal, Vertical),
          Color(White, Black), ColorMap, noAssign)

import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Exact as ESC
import Data.Bits (bit, setBit)

import Control.Monad (guard)
import Control.Applicative ((<$>))

import qualified Data.IntSet as IntSet; import Data.IntSet (IntSet)
import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.NonEmpty as NonEmpty
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid)
import Data.Word (Word64)
import Data.Set (Set)


data Item = Line | Position Int Color
   deriving (Eq, Ord, Show)

instance Base.Position Item where position = Position


type Assign map = ESC.Assign map (Map Strip (Set Item))

{-
quickCheck $ \n0 ns0 -> let n = abs n0; ns = map abs ns0 in spread n ns == spreadNaive n ns
-}
spread :: Int -> [Int] -> [(Int, [(Int, Int)])]
spread width0 sizes0 =
   case NonEmpty.init $ NonEmpty.scanr (+) (-1) (map succ sizes0) of
      [] -> return (width0, [])
      minWidth0:sums -> do
         remWidth0 <- reverse [minWidth0 .. width0]
         let go width [] = guard (width == 0) >> return []
             go width [(_0,size)] =
               guard (width>=size) >> return [(size, width-size)]
             go width ((minWidth,size):sizes) = do
               remWidth <- reverse [minWidth .. width-1]
               ((size, width-remWidth):) <$> go (remWidth-size) sizes
         chain <- go remWidth0 $ zip (sums++[0]) sizes0
         return (width0-remWidth0, chain)

_spreadNaive :: Int -> [Int] -> [(Int, [(Int, Int)])]
_spreadNaive width0 sizes0 = do
   start <- [0..width0]
   let go width [] = guard (width == 0) >> return []
       go width [size] = guard (width>=size) >> return [(size, width-size)]
       go width (size:sizes) = do
         space <- [1 .. width-size]
         ((size,space):) <$> go (width-size-space) sizes
   chain <- go (width0-start) sizes0
   return (start, chain)

assignsFromLine ::
   (Monoid map) => Orientation -> Int -> Int -> [Int] -> [Assign map]
assignsFromLine orient width line xs =
   map
      (noAssign . strip orient line . (Line :) .
       zipWith Position [0..] .
       (\(start,bricks) ->
         replicate start Black ++
         concatMap
            (\(size,space) -> replicate size White ++ replicate space Black)
            bricks)) $
   spread width xs

assignsGen ::
   (Monoid map) =>
   (Int -> Int -> Color -> map) -> [[Int]] -> [[Int]] -> [Assign map]
assignsGen square rows columns =
   concat (zipWith (assignsFromLine Horizontal (length columns)) [0..] rows)
   ++
   concat (zipWith (assignsFromLine Vertical (length rows)) [0..] columns)
   ++
   Base.assignsFromPositions square rows columns

assigns :: [[Int]] -> [[Int]] -> [Assign (Set (Int,Int))]
assigns = assignsGen Base.square

assignsBW :: [[Int]] -> [[Int]] -> [Assign ColorMap]
assignsBW = assignsGen Base.squareBW


type Mask = BitSet.Set Word64

bitAssigns ::
   [ESC.Assign map (Map Strip (Set Item))] -> [ESC.Assign map (Map Strip Mask)]
bitAssigns = map (fmap (fmap (foldMap (BitSet.Set . bitFromItem))))

bitFromItem :: Item -> Word64
bitFromItem x =
   case x of
      Line -> bit 63
      Position n color ->
         if n<31
           then bit (n + 31 * fromEnum color)
           else error "bitFromItem: position too big"


intSetAssigns ::
   Int -> Int ->
   [ESC.Assign map (Map Strip (Set Item))] -> [ESC.Assign map IntSet]
intSetAssigns nr nc =
   map (fmap (fold . Map.mapWithKey (intSetFromItems nr nc)))

intSetFromItems :: Int -> Int -> Strip -> Set Item -> IntSet
intSetFromItems nr nc (Strip orient k) items =
   case orient of
      Horizontal ->
         flip foldMap items $ \item ->
            IntSet.singleton $ intFromItem nr nc k item
      Vertical ->
         flip foldMap items $ \item ->
            IntSet.singleton $ nr + 2*nr*nc + intFromItem nc nr k item

intFromItem :: Int -> Int -> Int -> Item -> Int
intFromItem nr nc k item =
   case item of
      Line -> k
      Position j color -> nr + 2*(nc*k+j) + fromEnum color


type BitVector = BitSet.Set Integer

bitVectorAssigns ::
   Int -> Int ->
   [ESC.Assign map (Map Strip (Set Item))] -> [ESC.Assign map BitVector]
bitVectorAssigns nr nc =
   map (fmap (fold . Map.mapWithKey (bitVectorFromItems nr nc)))

bitVectorFromItems :: Int -> Int -> Strip -> Set Item -> BitVector
bitVectorFromItems nr nc x =
   BitSet.Set . foldl setBit 0 . IntSet.toList . intSetFromItems nr nc x
