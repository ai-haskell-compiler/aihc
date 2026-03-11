{- |
Conway's puzzle:

Assemble a 5x5x5 cube from the following cuboids:
 3x 1x1x3
29x 1x2x2

https://en.wikipedia.org/wiki/Conway_puzzle
-}
module Main where

import qualified Math.SetCover.Cuboid as Cuboid
import qualified Math.SetCover.Exact as ESC
import Math.SetCover.Cuboid (Size, Coords(Coords))

import qualified Control.Concurrent.PooledIO.Independent as Pool
import qualified Control.Monad.Trans.State as MS
import Control.Applicative (pure, liftA2, liftA3)

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.IntSet (IntSet)
import Data.Foldable (foldMap)

import qualified System.IO as IO
import Text.Printf (printf)
import Utility (hPutStrLnImmediate)


size :: Size
size = pure 5


stick :: [Int]
stick = [0..2]

type Mask = Set.Set (Maybe (Coords Int))
type FormatMask = Set.Set (Coords Int)

sticksX, sticksY, sticksZ :: [FormatMask]
sticksX =
   map Set.fromList $ Cuboid.allPositions size $ map (\x -> Coords x 0 0) stick
sticksY =
   map Set.fromList $ Cuboid.allPositions size $ map (\x -> Coords 0 x 0) stick
sticksZ =
   map Set.fromList $ Cuboid.allPositions size $ map (\x -> Coords 0 0 x) stick

data Coord = X | Y | Z deriving (Eq, Ord, Show)

count :: FormatMask -> Map.Map (Coord, Int, Int) Int
count =
   Map.fromListWith (+) .
   concatMap (\(Coords x y z) -> [((X,y,z),1), ((Y,x,z),1), ((Z,x,y),1)]) .
   Set.toList

minimal :: FormatMask -> Bool
minimal =
   let center = pure 2
       transforms =
         map (\trans -> liftA2 (+) (liftA2 (-) center (trans center)) . trans) $
         liftA2 (.) [id, fmap negate] Cuboid.rotations
   in  \mask -> all (\trans -> mask <= Set.map trans mask) transforms

assignsFromSticks ::
   [(FormatMask, FormatMask, FormatMask)] ->
   [ESC.Assign [FormatMask] FormatMask]
assignsFromSticks =
   map fst .
   filter (minimal . snd) .
   filter (Fold.all odd . count . snd) .
   map
      (\(x,y,z) ->
         let u = x `Set.union` y `Set.union` z
         in  (ESC.assign [x,y,z] u, u))

{-
Make use of the fact
that all slices must contain an odd number of elements from 1x1x3 cuboids.
-}
threeSticksOrtho :: [ESC.Assign [FormatMask] FormatMask]
threeSticksOrtho =
   assignsFromSticks $
   filter
      (\(x,y,z) -> ESC.disjoint x y && ESC.disjoint y z && ESC.disjoint z x) $
   liftA3 (,,) sticksX sticksY sticksZ


selectDisjoint :: MS.StateT [FormatMask] [] FormatMask
selectDisjoint =
   MS.StateT $ \masks -> do
      m:ms <- List.tails masks
      return (m, filter (ESC.disjoint m) ms)

threeSticks :: [ESC.Assign [FormatMask] FormatMask]
threeSticks =
   assignsFromSticks $
   MS.evalStateT
      (liftA3 (,,) selectDisjoint selectDisjoint selectDisjoint)
      (sticksX ++ sticksY ++ sticksZ)


square :: [(Int,Int)]
square = liftA2 (,) [0,1] [0,1]

squares :: [FormatMask]
squares =
   (map Set.fromList $
    concatMap (Cuboid.allPositions size)
      [map (\(x,y) -> Coords 0 x y) square,
       map (\(x,y) -> Coords x 0 y) square,
       map (\(x,y) -> Coords x y 0) square])

allAssigns :: [ESC.Assign [FormatMask] Mask]
allAssigns =
   map (\mask -> ESC.assign [mask] (Set.map Just mask)) squares
   ++
   map (fmap (Set.insert Nothing . Set.map Just)) threeSticks


threeSticksCanonical :: ESC.Assign [FormatMask] FormatMask
threeSticksCanonical =
   let x = Set.fromList [Coords 0 0 0, Coords 1 0 0, Coords 2 0 0]
       y = Set.fromList [Coords 3 1 1, Coords 3 2 1, Coords 3 3 1]
       z = Set.fromList [Coords 4 4 2, Coords 4 4 3, Coords 4 4 4]
   in  ESC.assign [x,y,z] (x `Set.union` y `Set.union` z)

initState ::
   ESC.Assign [FormatMask] FormatMask ->
   ESC.State [FormatMask] IntSet
initState s3asn =
   case ESC.intSetFromSetAssigns $
         s3asn : map (\mask -> ESC.assign [mask] mask) squares of
      asns@(s3:_) -> ESC.updateState s3 $ ESC.initState asns
      [] -> error "ESC.bitVectorFromSetAssigns lost first assignment"


formatIdent :: Int -> Char
formatIdent n =
   toEnum $
   if n<10
     then n + fromEnum '0'
     else n-10 + fromEnum 'A'

format :: [FormatMask] -> String
format v =
   let cubex =
          Map.unions $
          zipWith (\n -> foldMap (flip Map.singleton n)) [0..] $
          reverse v
   in  Cuboid.forNestedCoords
          unlines (List.intercalate " | ") (List.intercalate " ")
          (\c -> maybe "." (\n -> [formatIdent n]) $ Map.lookup c cubex)
          size

printMask :: [FormatMask] -> IO ()
printMask = hPutStrLnImmediate IO.stdout . format


mainSimple, mainCanonical, mainParallel :: IO ()
mainSimple = do
   let sol =
         map concat $ ESC.partitions $ ESC.bitVectorFromSetAssigns allAssigns
   mapM_ printMask sol
   print $ length sol

mainCanonical = do
   let sol = map concat $ ESC.search $ initState threeSticksCanonical
   mapM_ printMask sol
   print $ length sol

mainParallel =
   Pool.run $
      (\f -> zipWith f [0..] threeSticks) $ \n s3 ->
         IO.withFile (printf "conway%04d.txt" (n::Int)) IO.WriteMode $ \h ->
            mapM_ (hPutStrLnImmediate h . format . concat) $
            ESC.search $ initState s3


main :: IO ()
main = mainCanonical
