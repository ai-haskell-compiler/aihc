{- |
Lonpos pyramid 101 puzzle

Like Soma cube but with exclusively flat bricks made from balls
that allow to stack the bricks in a diagonal fashion.

There are two problems to solve:

* arrange all bricks in a flat 5x11 rectangle

* arrange all bricks in a square pyramid with a 5x5 base.

<https://www.youtube.com/watch?v=5lwryXvqXBU>
-}
module Main where

import qualified Math.SetCover.Exact as ESC
import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Bit as Bit
import qualified Math.SetCover.Cuboid as Cuboid
import Math.SetCover.Cuboid (PackedCoords(PackedCoords), Coords(Coords), Size)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Foldable (forM_, foldMap)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Char (ord, chr)
import Data.Word (Word16, Word64)

import qualified System.IO as IO
import Utility (hPutStrLnImmediate)
import Text.Printf (printf)


shapes :: [[String]]
shapes =
   (
   ".." :
   " ." :
   [])
   :
   (
   "...." :
   [])
   :
   (
   "..." :
   "  ." :
   [])
   :
   (
   ".." :
   ".." :
   [])
   :
   (
   ".. " :
   " .." :
   "  ." :
   [])
   :
   (
   "... " :
   "  .." :
   [])
   :
   (
   "...." :
   "   ." :
   [])
   :
   (
   "...." :
   "  . " :
   [])
   :
   (
   "..." :
   ". ." :
   [])
   :
   (
   ".. " :
   "..." :
   [])
   :
   (
   "..." :
   "  ." :
   "  ." :
   [])
   :
   (
   " . " :
   "..." :
   " . " :
   [])
   :
   []


propNumberOfAtoms :: Bool
propNumberOfAtoms = Cuboid.numberOf2LayerAtoms shapes == 5*11


targetBase, targetPyramid :: [[String]]
targetBase =
   let line = replicate 11 '.'
   in  [replicate 5 line]

targetPyramid =
   map (\n -> replicate n $ replicate n '.') [5,4,3,2,1]



newtype Brick = Brick Int deriving (Eq, Ord)

showBall :: Brick -> String
showBall (Brick n) =
   if n<10 then show n else [chr $ ord 'A' + n-10]

type Mask = Set.Set (Either Brick PackedCoords)

type Assign = ESC.Assign (Map.Map PackedCoords Brick) Mask

{-
a = sqrt 2

A^T -> B^T:
(1, 1, 0) -> ( 1,  1,  0)
(1,-1, 0) -> ( 0,  0,  a)
(0, 0, a) -> (-1,  1,  0)

B = M·A

rotation around vector (1,1,0) by 90°
M =
0.5 ·
 ( 1  1  a)
 ( 1  1 -a)
 (-a  a  0)

scale z such that roots vanish
S = diag (1,1,a)

shear
U = 0.5 ·
 (2    -1)
 (   2 -1)
 (      2)

U·S·M·S^-1·U^-1 =
 ( 1  0  1)
 ( 1  0  0)
 (-1  1  0)
-}
diagRot0 :: Num a => Coords a -> Coords a
diagRot0 (Coords z y x) = Coords (y-x) x (x+z)

{-
rotation around vector (1,-1,0) by 90°

M =
0.5 *
 ( 1 -1  a)
 (-1  1  a)
 (-a -a  0)

U·S·M·S^-1·U^-1 =
 ( 1  0  1)
 ( 0  1  1)
 (-1 -1 -1)
-}
diagRot1 :: Num a => Coords a -> Coords a
diagRot1 (Coords z y x) = Coords (-x-y-z) (y+z) (x+z)

{-
R =
 ( 0  1  0)
 (-1  0  0)
 ( 0  0  1)

U·R·U^-1 =
 ( 0  1  0)
 (-1  0 -1)
 ( 0  0  1)
-}
vertRot :: Num a => Coords a -> Coords a
vertRot (Coords z y x) = Coords z (-x-z) y

{-
Q = 0.5 ·
 (a -a  0)
 (a  a  0)
 (0  0  2)

U·S·Q = 0.5 · a ·
 (1 -1 -1)
 (1  1 -1)
 (0  0  2)

With this matrix we could transform the coordinates
such that we could use 'Cuboid.allOrientations'.
However, this would require a final division by 2.
-}

primRotations :: Coords (Coords Int -> Coords Int)
primRotations = Coords vertRot diagRot0 diagRot1

transformedBrickAssign :: Size -> Brick -> [String] -> [Assign]
transformedBrickAssign size k =
   map (brickAssign size k) . concatMap (Cuboid.allPositions size) .
   Cuboid.allOrientationsGen primRotations .
   map (\(Coords y x z) -> Coords z y x) .
   Cuboid.coordsFrom2LayerString

brickSize :: Size
brickSize = Coords 4 4 4

testRotations :: [String] -> [Map.Map PackedCoords Brick]
testRotations =
   map (Map.fromList . map (flip (,) (Brick 0) . Cuboid.packCoords brickSize)) .
   Cuboid.allOrientationsGen primRotations .
   map (\(Coords y x z) -> Coords z y x) .
   Cuboid.coordsFrom2LayerString

brickAssign :: Size -> Brick -> [Coords Int] -> Assign
brickAssign size k ts =
   let xs = map (Cuboid.packCoords size) ts
   in  ESC.assign (Map.fromList $ map (flip (,) k) xs) $
       Set.fromList $ Left k : map Right xs

allAssigns :: Size -> [Assign]
allAssigns size =
   concat $ zipWith (transformedBrickAssign size) (map Brick [0 ..]) shapes

fittingAssigns :: Size -> [Coords Int] -> [Assign]
fittingAssigns size target =
   let targetSet = Set.fromList $ map (Cuboid.packCoords size) target
       keepRights =
          Set.fromList . mapMaybe (either (const Nothing) Just) . Set.toList
   in  filter (flip Set.isSubsetOf targetSet . keepRights . ESC.labeledSet) $
       allAssigns size


format :: Size -> [Map.Map PackedCoords Brick] -> String
format size v =
   let filled = Map.unions v
       toppleSize (Coords x y z) = Coords z x y
       topple (Coords z x y) = Coords x y z
   in  Cuboid.forNestedCoords
          unlines (intercalate " | ") (intercalate " ")
          (\c ->
             maybe "." showBall $
             Map.lookup (Cuboid.packCoords size $ topple c) filled)
          (toppleSize size)

printMask :: Size -> [Map.Map PackedCoords Brick] -> IO ()
printMask size =
   hPutStrLnImmediate IO.stdout . format size


type BitMask = BitSet.Set (Bit.Sum Word16 Word64)

packMask :: (PackedCoords -> Int) -> Mask -> BitMask
packMask f =
   foldMap
      (\c ->
         BitSet.Set $
         case c of
            Left (Brick k) -> Bit.bitLeft k
            Right k -> Bit.bitRight $ f k)

packFlat :: Size -> PackedCoords -> Int
packFlat _size (PackedCoords k) = k

packPyramid :: Size -> PackedCoords -> Int
packPyramid size@(Coords sizez _ _) p =
   case Cuboid.unpackCoords size p of
      Cuboid.Coords nz y x ->
         let z = sizez-1-nz
         in  div (z*(z+1)*(2*z+1)) 6 + (z+1)*y + x


main, mainBase, mainBits :: IO ()

-- 14 min for pyramid solutions
mainBase =
   forM_ [targetPyramid, targetBase] $
   \targetString -> do
      let target = Cuboid.coordsFromString targetString
          size = Cuboid.size target
          sol = ESC.partitions $ fittingAssigns size target
      printMask size $ head sol

-- 2 min for pyramid solutions
mainBits =
   forM_ [(targetPyramid, packPyramid), (targetBase, packFlat)] $
   \(targetString, pack) -> do
      let target = Cuboid.coordsFromString targetString
          size = Cuboid.size target
          sol =
             ESC.partitions $ map (fmap (packMask (pack size))) $
             fittingAssigns size target
      if True
        then mapM_ (printMask size) sol
        else printMask size $ head sol
      printf "total number of solutions: %d\n\n" $ length sol

main = mainBits
