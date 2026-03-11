module Combinatorics.Battleship.Count.InclusionExclusion where

import qualified Combinatorics.Battleship.Allocation as Alloc
import qualified Combinatorics.Battleship.Enumeration as BS
import Combinatorics.Battleship (Board(Board), Ship(Ship), Orientation(..), )

import Data.List (tails, )
import Data.Set (Set, )
import qualified Data.Set as Set
import qualified Data.Ix as Ix

import Control.Monad (guard, liftM2, liftM3, )


allOverlaps :: Int -> Orientation -> Alloc.T -> [Ship]
allOverlaps size orient set = do
   let fatset = Alloc.fatten set
   let ((minx,miny),maxc) = Alloc.boundingBox fatset
   pos <-
      Ix.range $
      (case orient of
          Horizontal -> (minx-size+1, miny)
          Vertical   -> (minx, miny-size+1),
       maxc)
   let ship = Ship size orient pos
       area = BS.shipArea ship
   guard $ not $ Set.null $ Set.intersection fatset area
   return ship

allOverlapsHV :: Int -> Alloc.T -> [Ship]
allOverlapsHV size set =
   allOverlaps size Horizontal set ++
   allOverlaps size Vertical set

allOverlapsHVShip :: Int -> Ship -> [Set Ship]
allOverlapsHVShip size ship =
   let set = BS.shipArea ship
   in  map (flip Set.insert (Set.singleton ship)) $
       allOverlaps size Horizontal set ++
       allOverlaps size Vertical set

allOverlapsHVShips :: Int -> Set Ship -> [Set Ship]
allOverlapsHVShips size ships =
   let set = Set.unions $ map BS.shipArea $ Set.toList ships
   in  map (flip Set.insert ships) $
       allOverlaps size Horizontal set ++
       allOverlaps size Vertical set

countOverlaps :: (Int, Int) -> Int -> Int -> Orientation -> Integer
countOverlaps bnds size0 size1 orient1 =
   sum $
   countMovedOverlaps bnds =<<
   (allOverlapsHVShip size0 $ Ship size1 orient1 (0,0))

shipsBoxSizes :: Set Ship -> (Int, Int)
shipsBoxSizes =
   BS.boxSizes . foldl1 BS.mergeBox . map BS.shipBounds . Set.toList

countMovedOverlaps :: (Int, Int) -> Set Ship -> [Integer]
countMovedOverlaps (width, height) ovl = do
   let (w,h) = shipsBoxSizes ovl
       wc = width  + 1 - w
       hc = height + 1 - h
   guard $ wc>=0 && hc>=0
   return $ fromIntegral wc * fromIntegral hc

count2_3 :: Integer
count2_3 =
   let bnds@(w,h) = (6,6)
       wh = fromIntegral $ w+h
   in  (8*wh) * (9*wh)
         - countOverlaps bnds 2 3 Horizontal
         - countOverlaps bnds 2 3 Vertical

count4_5 :: Integer
count4_5 =
   let bnds = (10, 10)
       count size =
          sum $
          countMovedOverlaps bnds
             =<< map Set.singleton
                    [Ship size Horizontal (0,0),
                     Ship size Vertical (0,0)]
   in  count 4 * count 5
         - countOverlaps bnds 4 5 Horizontal
         - countOverlaps bnds 4 5 Vertical

count4_4 :: Integer
count4_4 =
   let w = 10
       h = 10
       bnds = (fromInteger w, fromInteger h)
   in  div ((w*(h+1-4) + (w+1-4)*h)^(2::Int)
        - countOverlaps bnds 4 4 Horizontal
        - countOverlaps bnds 4 4 Vertical) 2


overlap :: Ship -> Ship -> Bool
overlap a b =
   let (BS.Box a0 (a1x,a1y)) = BS.shipBounds a
       (BS.Box b0 (b1x,b1y)) = BS.shipBounds b
       (BS.Box (c0x,c0y) (c1x,c1y)) =
          BS.intersectBox (BS.Box a0 (a1x+1,a1y+1)) (BS.Box b0 (b1x+1,b1y+1))
   in  c0x<=c1x && c0y<=c1y


enumerate3_4_5 :: (Int, Int) -> [(Ship, Ship, Ship)]
enumerate3_4_5 (w,h) =
   let ships size =
          map (Ship size Horizontal) (liftM2 (,) [0..w-size] [0..h-1])
          ++
          map (Ship size Vertical) (liftM2 (,) [0..w-1] [0..h-size])
   in  liftM3 (,,) (ships 3) (ships 4) (ships 5)

enumerateOverlaps3_4_5_pairs :: (Integer, Integer, Integer)
enumerateOverlaps3_4_5_pairs =
   let bnds = (6,6)
   in  (fromIntegral $ length $
        filter (\(_s3,s4,s5) -> overlap s4 s5) $
        enumerate3_4_5 bnds,
        fromIntegral $ length $
        filter (\(s3,_s4,s5) -> overlap s3 s5) $
        enumerate3_4_5 bnds,
        fromIntegral $ length $
        filter (\(s3,s4,_s5) -> overlap s3 s4) $
        enumerate3_4_5 bnds)

enumerateOverlaps3_4_5_pairs2 :: (Integer, Integer, Integer)
enumerateOverlaps3_4_5_pairs2 =
   let bnds = (6,6)
   in  (fromIntegral $ length $
        filter (\(s3,s4,s5) ->
           overlap s3 s4 && overlap s3 s5) $
        enumerate3_4_5 bnds,
        fromIntegral $ length $
        filter (\(s3,s4,s5) ->
           overlap s3 s4 && overlap s4 s5) $
        enumerate3_4_5 bnds,
        fromIntegral $ length $
        filter (\(s3,s4,s5) ->
           overlap s3 s5 && overlap s4 s5) $
        enumerate3_4_5 bnds)

enumerateOverlaps3_4_5_pairs3 :: Integer
enumerateOverlaps3_4_5_pairs3 =
   let bnds = (6,6)
   in  fromIntegral $ length $
       filter (\(s3,s4,s5) ->
          overlap s3 s4 && overlap s4 s5 && overlap s3 s5) $
       enumerate3_4_5 bnds

enumerateOverlaps3_4_5_noOverlap :: Integer
enumerateOverlaps3_4_5_noOverlap =
   let bnds = (6,6)
   in  fromIntegral $ length $
       filter (\(s3,s4,s5) ->
          not (overlap s3 s4 || overlap s4 s5 || overlap s3 s5)) $
       enumerate3_4_5 bnds

enumerateOverlaps3_4_5_noOverlapTest :: Bool
enumerateOverlaps3_4_5_noOverlapTest =
   enumerateOverlaps3_4_5_noOverlap
   ==
   let sum3 (x,y,z) = x+y+z
   in  fromIntegral (length (enumerate3_4_5 (6,6)))
       -
       sum3 enumerateOverlaps3_4_5_pairs
       +
       sum3 enumerateOverlaps3_4_5_pairs2
       -
       enumerateOverlaps3_4_5_pairs3

enumerateOverlaps3_4_5_triples :: Integer
enumerateOverlaps3_4_5_triples =
   let bnds = (6,6)
   in  (fromIntegral $ length $
        filter (\(s3,s4,s5) ->
           overlap s3 s4 && overlap s3 s5  ||
           overlap s3 s4 && overlap s4 s5  ||
           overlap s3 s5 && overlap s4 s5) $
        enumerate3_4_5 bnds)



enumerate4_4 :: (Int, Int) -> [(Ship, Ship)]
enumerate4_4 (w,h) =
   let ships size =
          map (Ship size Horizontal) (liftM2 (,) [0..w-size] [0..h-1])
          ++
          map (Ship size Vertical) (liftM2 (,) [0..w-1] [0..h-size])
   in  liftM2 (,) (ships 4) (ships 4)

enumerateOverlaps4_4_noOverlap :: Integer
enumerateOverlaps4_4_noOverlap =
   let bnds = (6,7)
   in  fromIntegral $ length $
       filter (\(s4a,s4b) -> not (overlap s4a s4b)) $
       enumerate4_4 bnds



{-
This configuration can only be reached in one order:

<--->
 A
 |
 V
 <-->

In contrast to that,
the following configuration can be reached in more than one order:

<--->
 A<-->
 |
 V

Thus we have to check for duplicates manually.
-}
count3_4_5test :: (Integer, Integer, Integer)
count3_4_5test =
   let bnds = (6,6)
       ov3_4 = allOverlapsHVShip 3 $ Ship 4 Horizontal (0,0)
       ov3_5 = allOverlapsHVShip 3 $ Ship 5 Horizontal (0,0)
       ov4_5 = allOverlapsHVShip 4 $ Ship 5 Horizontal (0,0)
   in  (sum (countMovedOverlaps bnds =<< allOverlapsHVShips 3 =<< ov4_5),
        sum (countMovedOverlaps bnds =<< allOverlapsHVShips 4 =<< ov3_5),
        sum (countMovedOverlaps bnds =<< allOverlapsHVShips 5 =<< ov3_4))

count3_4_5overlaps :: (Int, Int) -> Integer
count3_4_5overlaps bnds =
   let ov3_5 = allOverlapsHVShip 3 $ Ship 5 Horizontal (0,0)
       ov4_5 = allOverlapsHVShip 4 $ Ship 5 Horizontal (0,0)
   in  sum $ concatMap (countMovedOverlaps bnds) $
       Set.toList $ Set.fromList $
          (allOverlapsHVShips 3 =<< ov4_5) ++
          (allOverlapsHVShips 4 =<< ov3_5)

normalize :: Set Ship -> Set Ship
normalize ss =
   let (BS.Box (dx,dy) _) =
          foldl1 BS.mergeBox . map BS.shipBounds . Set.toList $ ss
   in  Set.map (BS.moveShip (-dx,-dy)) ss

count3_4_5overlaps2 :: (Int, Int) -> Integer
count3_4_5overlaps2 bnds =
   let ships size =
          map (\orient -> Ship size orient (0,0)) [Horizontal, Vertical]
       ov3_4 = allOverlapsHVShip 3 =<< ships 4
       ov3_5 = allOverlapsHVShip 3 =<< ships 5
       ov4_5 = allOverlapsHVShip 4 =<< ships 5
   in  sum $ concatMap (countMovedOverlaps bnds) $
       Set.toList $ Set.fromList $ map normalize $ concat $
          [allOverlapsHVShips 3 =<< ov4_5,
           allOverlapsHVShips 4 =<< ov3_5,
           allOverlapsHVShips 5 =<< ov3_4]

count3_4_5overlapsTestA :: Bool
count3_4_5overlapsTestA =
   count3_4_5overlaps2 (6,6) == 2 * count3_4_5overlaps (6,6)

count3_4_5overlapsTestB :: Bool
count3_4_5overlapsTestB =
   count3_4_5overlaps2 (6,6) == enumerateOverlaps3_4_5_triples

count3_4_5_ov2 :: (Integer, Integer, Integer)
count3_4_5_ov2 =
   let bnds = (6,6)
       ov4_5 = allOverlapsHVShip 4 $ Ship 5 Horizontal (0,0)
       ov3_5 = allOverlapsHVShip 3 $ Ship 5 Horizontal (0,0)
       ov3_4 = allOverlapsHVShip 3 $ Ship 4 Horizontal (0,0)
   in  (sum (countMovedOverlaps bnds =<< ov4_5),
        sum (countMovedOverlaps bnds =<< ov3_5),
        sum (countMovedOverlaps bnds =<< ov3_4))

count3_4_5_ov2mult :: (Integer, Integer, Integer)
count3_4_5_ov2mult =
   let n = 6
       bnds = (fromIntegral n, fromIntegral n)
       ov4_5 = allOverlapsHVShip 4 $ Ship 5 Horizontal (0,0)
       ov3_5 = allOverlapsHVShip 3 $ Ship 5 Horizontal (0,0)
       ov3_4 = allOverlapsHVShip 3 $ Ship 4 Horizontal (0,0)
   in  (sum (countMovedOverlaps bnds =<< ov4_5) * 2*n*(n+1-3) * 2,
        sum (countMovedOverlaps bnds =<< ov3_5) * 2*n*(n+1-4) * 2,
        sum (countMovedOverlaps bnds =<< ov3_4) * 2*n*(n+1-5) * 2)

count3_4_5_ov2multTest :: Bool
count3_4_5_ov2multTest =
   count3_4_5_ov2mult == enumerateOverlaps3_4_5_pairs

count3_4_5overlaps3 :: (Int, Int) -> Integer
count3_4_5overlaps3 bnds =
   let makeShips size =
          map (\orient -> Ship size orient (0,0)) [Horizontal, Vertical]
       ov3_4 = allOverlapsHVShip 3 =<< makeShips 4
       ov3_5 = allOverlapsHVShip 3 =<< makeShips 5
       ov4_5 = allOverlapsHVShip 4 =<< makeShips 5
       coeff ships =
          (\n ->
             case length n of
                2 -> 1
                3 -> 2
                _ -> error "impossible length of list") $
          do (s0:ss) <- tails ships
             s1 <- ss
             guard $ overlap s0 s1
             return ()
   in  sum $
       concatMap
          (\ships ->
             fmap (coeff (Set.toList ships) *) $
             countMovedOverlaps bnds ships) $
       Set.toList $ Set.fromList $ map normalize $ concat $
          [allOverlapsHVShips 3 =<< ov4_5,
           allOverlapsHVShips 4 =<< ov3_5,
           allOverlapsHVShips 5 =<< ov3_4]

count3_4_5 :: Integer
count3_4_5 =
   let n = 10
       bnds = (fromIntegral n, fromIntegral n)
       ov3_4 = allOverlapsHVShip 3 $ Ship 4 Horizontal (0,0)
       ov3_5 = allOverlapsHVShip 3 $ Ship 5 Horizontal (0,0)
       ov4_5 = allOverlapsHVShip 4 $ Ship 5 Horizontal (0,0)
   in  (2*n*(n+1-5)) * (2*n*(n+1-4)) * (2*n*(n+1-3))
        - 2 * (sum (countMovedOverlaps bnds =<< ov3_4) * (2*n*(n+1-5))
             + sum (countMovedOverlaps bnds =<< ov3_5) * (2*n*(n+1-4))
             + sum (countMovedOverlaps bnds =<< ov4_5) * (2*n*(n+1-3)))
        + count3_4_5overlaps3 bnds


main :: IO ()
main =
   mapM_ (putStrLn . BS.formatBoard .
          (\(Board bnds field) -> Board bnds $ Alloc.normalize field) .
          BS.boardFromShips (10,10) . Set.toList) $
   allOverlapsHVShip 4 $ Ship 5 Vertical (0,0)
