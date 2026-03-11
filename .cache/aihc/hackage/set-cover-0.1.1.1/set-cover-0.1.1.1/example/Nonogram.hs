{-
* <https://en.wikipedia.org/wiki/Nonogram>
* <https://de.wikipedia.org/wiki/Datei:Paint_by_numbers_Animation.gif>

With the Combinatoric encoding and the priority queue based solver
the performance is appropriate but specialized solvers are still faster.
-}
module Main where

import qualified Nonogram.Example as Example
import qualified Nonogram.Encoding.Combinatoric as Combinatoric
import qualified Nonogram.Encoding.BlackWhite as BlackWhite
import qualified Nonogram.Encoding.Plug as Plug
import qualified Nonogram.Encoding.Naive as Naive
import Nonogram.Base (Strip, Color(White, Black), ColorMap)

import qualified Math.SetCover.Exact.Priority as ESC
import qualified Math.SetCover.Exact as ESCS
import qualified Math.SetCover.BitPosition as BitPos
import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.BitPriorityQueue as BitPQ
import qualified Math.SetCover.Queue as Queue

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Data.EnumMap as EnumMap
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as ListHT
import Data.OrdPSQ (OrdPSQ)
import Data.IntPSQ (IntPSQ)
import Data.EnumSet (EnumSet)
import Data.IntSet (IntSet)
import Data.Foldable (foldMap)
import Data.NonEmpty ((!:))
import Data.Word (Word64)


decode :: [[Int]] -> [[Int]] -> [Set (Int, Int)]
decode rows columns =
   map Set.unions $
   case 01::Int of
      00 -> ESC.partitions ESC.queueBit $
               Combinatoric.bitVectorAssigns (length rows) (length columns) $
               Combinatoric.assigns rows columns
      01 -> ESC.partitions queueMapBit $
               Combinatoric.bitAssigns $ Combinatoric.assigns rows columns
      02 -> ESCS.partitions $
               Combinatoric.intSetAssigns (length rows) (length columns) $
               Combinatoric.assigns rows columns
      03 -> partitionsSet $ Combinatoric.assigns rows columns
      10 -> ESC.partitions queueMapBit $
               BlackWhite.bitAssigns rows columns $
               BlackWhite.assigns rows columns
      11 -> partitionsSet $ BlackWhite.assigns rows columns
      20 -> partitionsSet $ Plug.assigns rows columns
      _  -> partitionsSet $ Naive.assigns rows columns

partitionsSet :: (Ord a) => [ESC.Assign label (Map Strip (Set a))] -> [[label]]
partitionsSet = ESC.partitions queueMap

queueMap ::
   (Ord a) =>
   Queue.Methods
      (OrdPSQ Strip Int (OrdPSQ a Int (EnumSet Queue.SetId)))
      (Map Strip (Set a))
queueMap = ESC.queueMap ESC.queueSet

queueMapBit ::
   (BitPos.C bits) =>
   Queue.Methods
      (OrdPSQ Strip Int (IntPSQ Int (EnumSet Queue.SetId)))
      (Map Strip (BitSet.Set bits))
queueMapBit = ESC.queueMap ESC.queueBit


format :: Int -> Int -> Set (Int, Int) -> String
format rows columns set =
   unlines $
   ListHT.outerProduct
      (\r c -> if Set.member (r,c) set then 'X' else '.')
      (take rows [0..])
      (take columns [0..])

formatBW :: Int -> Int -> ColorMap -> String
formatBW rows columns set =
   unlines $
   ListHT.outerProduct
      (\r c ->
         case Map.lookup (r,c) set of
            Nothing -> '_'
            Just Black -> 'X'
            Just White -> '.')
      (take rows [0..])
      (take columns [0..])

besidesMany :: Int -> [String] -> String
besidesMany space =
   let besides blockL blockR =
         let width = NonEmpty.maximum (0 !: map length blockL) + space
         in  zipWith (\l r -> ListHT.padRight ' ' width l ++ r) blockL blockR
   in  unlines . foldr1 besides . map lines

testSimple :: ([[Int]], [[Int]]) -> IO ()
testSimple (rows, columns) = do
   let assigns = Naive.assigns rows columns
   mapM_ (print . ESC.labeledSet) assigns
   putStrLn "set union:"
   print $ foldMap ESC.labeledSet assigns
   mapM_ (putStrLn . format (length rows) (length columns) . Set.unions) $
      partitionsSet assigns

decodeImage :: ([[Int]], [[Int]]) -> IO ()
decodeImage (rows, columns) =
   mapM_ (putStrLn . format (length rows) (length columns)) $
   decode rows columns

testImage :: IO ()
testImage = decodeImage $ Example.encodeStrings Example.letterP


type Evolve queue set = ([[Int]], [[Int]]) -> [[ESC.State queue ColorMap set]]

evolveGen ::
   Queue.Methods queue set ->
   [ESCS.Assign ColorMap set] -> [[ESC.State queue ColorMap set]]
evolveGen methods assigns =
   takeWhile (not . null) $
   iterate
      (concatMap (ESC.step methods) .
       filter (not . Queue.null methods . ESC.queue))
      [ESC.initState methods assigns]

evolveQueueMapBW ::
   Evolve
      (OrdPSQ Strip Int (OrdPSQ BlackWhite.Item Int (EnumSet Queue.SetId)))
      (Map Strip (Set BlackWhite.Item))
evolveQueueMapBW = evolveGen queueMap . uncurry BlackWhite.assignsBW

evolveQueueMap ::
   Evolve
      (OrdPSQ Strip Int (OrdPSQ Combinatoric.Item Int (EnumSet Queue.SetId)))
      (Map Strip (Set Combinatoric.Item))
evolveQueueMap = evolveGen queueMap . uncurry Combinatoric.assignsBW

evolveQueueMapBit ::
   Evolve
      (OrdPSQ Strip Int (IntPSQ Int (EnumSet Queue.SetId)))
      (Map Strip (BitSet.Set Word64))
evolveQueueMapBit =
   evolveGen queueMapBit .
   Combinatoric.bitAssigns . uncurry Combinatoric.assignsBW

evolveQueueBitPQ ::
   Evolve (BitPQ.Queue Integer Queue.SetId) (BitSet.Set Integer)
evolveQueueBitPQ (rows, columns) =
   evolveGen ESC.queueBitPQ $
   Combinatoric.bitVectorAssigns (length rows) (length columns) $
   Combinatoric.assignsBW rows columns

evolveQueueBit ::
   Evolve (IntPSQ Int (EnumSet Queue.SetId)) (BitSet.Set Integer)
evolveQueueBit (rows, columns) =
   evolveGen ESC.queueBit $
   Combinatoric.bitVectorAssigns (length rows) (length columns) $
   Combinatoric.assignsBW rows columns

evolveQueueIntSet :: Evolve (IntPSQ Int (EnumSet Queue.SetId)) IntSet
evolveQueueIntSet (rows, columns) =
   evolveGen ESC.queueIntSet $
   Combinatoric.intSetAssigns (length rows) (length columns) $
   Combinatoric.assignsBW rows columns

evolve :: ([[Int]], [[Int]]) -> IO ()
evolve (rows, columns) =
   let formatIntermediate state =
          (show $ EnumMap.size $ ESC.availableSubsets state) ++ '\n' :
          (formatBW (length rows) (length columns) .
           Map.unionsWith (error "conflicting colors") .
           ESC.usedSubsets $ state)
   in  mapM_ (putStrLn . besidesMany 2 . map formatIntermediate) $
       evolveQueueMap (rows, columns)

main :: IO ()
main = evolve Example.soccerEnc
