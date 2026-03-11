module Combinatorics.Battleship.SetCover where

import qualified Combinatorics.Battleship.Fleet as Fleet
import Combinatorics.Battleship (Ship(Ship), ShipSize, Orientation(..), )

import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Exact as ESC
import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Set as Set; import Data.Set (Set)

import System.Random (RandomGen, randomR, mkStdGen)

import Text.Printf (printf)

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS
import qualified Control.Functor.HT as FuncHT
import Control.DeepSeq (force)
import Control.Monad (liftM, liftM2, when, mplus)

import qualified Data.StorableVector as SV
import qualified Data.Foldable as Fold
import qualified Data.List as List
import Data.Foldable (foldMap, forM_)
import Data.Maybe.HT (toMaybe)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Tuple.HT (mapFst)
import Data.Word (Word64)


shipShape :: Ship -> Map (Int, Int) Bool
shipShape (Ship size orient (x,y)) =
   Map.fromAscList $ map (flip (,) True) $
   case orient of
      Horizontal -> map (flip (,) y) [x .. x+size-1]
      Vertical -> map ((,) x) [y .. y+size-1]

shipReserve :: Ship -> Set (Int, Int)
shipReserve (Ship size orient (x,y)) =
   let lx = max 0 (x-1)
       ly = max 0 (y-1)
   in  Set.fromAscList $
       case orient of
         Horizontal -> liftM2 (,) [lx .. x+size-1] [ly .. y]
         Vertical -> liftM2 (,) [lx .. x] [ly .. y+size-1]


type AssignShip = ESC.Assign (ShipSize, Map (Int, Int) Bool) (Set (Int, Int))

assignsShip :: [ShipSize] -> (Int, Int) -> [AssignShip]
assignsShip sizes (width, height) = do
   size <- sizes
   mplus
      (do
         x <- [0 .. width-size]
         y <- [0 .. height-1]
         let horizShip = Ship size Horizontal (x,y)
         [ESC.assign (size, shipShape horizShip) (shipReserve horizShip)])
      (do
         x <- [0 .. width-1]
         y <- [0 .. height-size]
         let vertShip = Ship size Vertical (x,y)
         [ESC.assign (size, shipShape vertShip) (shipReserve vertShip)])

boardCoords :: (Int, Int) -> [(Int, Int)]
boardCoords (width, height) =
   liftM2 (,) (take width [0..]) (take height [0..])

assignsSquare ::
   (Int, Int) ->
   [ESC.Assign (Maybe ShipSize, Map (Int, Int) Bool) (Set (Int, Int))]
assignsSquare (width, height) = do
   p <- boardCoords (width, height)
   [ESC.assign (Nothing, Map.singleton p False) (Set.singleton p)]

assigns ::
   [ShipSize] -> (Int, Int) ->
   [ESC.Assign (Maybe ShipSize, Map (Int, Int) Bool) (Set (Int, Int))]
assigns sizes boardSize =
   map
      (\asn -> asn{ESC.label = mapFst Just (ESC.label asn)})
      (assignsShip sizes boardSize) ++
   assignsSquare boardSize


formatBoard :: (Int, Int) -> Map (Int, Int) Bool -> String
formatBoard (width, height) set =
   unlines $
   FuncHT.outerProduct
      (\y x ->
         case Map.lookup (x,y) set of
            Nothing -> '_'
            Just False -> '.'
            Just True -> 'x')
      [0 .. height-1] [0 .. width-1]


printState :: (Int, Int) -> ESC.State (ship, Map (Int, Int) Bool) set -> IO ()
printState boardSize =
   printBoard boardSize . foldMap snd . ESC.usedSubsets

printBoard :: (Int, Int) -> Map (Int, Int) Bool -> IO ()
printBoard boardSize = putStr . ('\n':) . formatBoard boardSize


standardBoardSize :: (Int, Int)
standardBoardSize = (10, 10)

standardFleetList :: [(ShipSize, Fleet.NumberOfShips)]
standardFleetList = [(5,1), (4,2), (3,3), (2,4)]

enumerateFirst :: IO ()
enumerateFirst = do
   let boardSize = standardBoardSize
   mapM_
      (printState boardSize)
      (ESC.step $ ESC.initState $ assigns (map fst standardFleetList) boardSize)

enumerateMixed :: IO ()
enumerateMixed = do
   let boardSize = standardBoardSize
   let fleetList = standardFleetList
   let fleet = Fleet.fromList fleetList
   let loop state =
         let usedFleet =
               Fleet.fromList $ map (flip (,) 1) $
               mapMaybe fst $ ESC.usedSubsets state
         in  when (Fleet.subset usedFleet fleet) $
             if usedFleet == fleet
               then printState boardSize state
               else mapM_ loop (ESC.step state)
   loop $ ESC.initState $ assigns (map fst fleetList) boardSize


type AssignShipBitSet =
      ESC.Assign (ShipSize, Map (Int, Int) Bool) (BitSet.Set Integer)

enumerateGen ::
   (Monad m) =>
   ([AssignShipBitSet] -> m AssignShipBitSet) ->
   (Int, Int) -> [(ShipSize, Int)] -> m (Map (Int, Int) Bool)
enumerateGen sel boardSize fleetList = do
   let layoutShip shipSize = do
         state <- MS.get
         place <-
            MT.lift $ sel $ filter ((shipSize==) . fst . ESC.label) $
            ESC.availableSubsets state
         MS.put $ ESC.updateState place state
   liftM (foldMap snd . ESC.usedSubsets) $
      MS.execStateT
         (mapM_ layoutShip $ concatMap (uncurry $ flip replicate) fleetList) $
      ESC.initState $
      ESC.bitVectorFromSetAssigns $ assignsShip (map fst fleetList) boardSize


enumerateShip :: IO ()
enumerateShip = do
   let boardSize = standardBoardSize
   let fleetList = standardFleetList
   mapM_ (printBoard boardSize) $ enumerateGen id boardSize fleetList


select :: (RandomGen g) => [a] -> MS.StateT g Maybe a
select xs = MS.StateT $ \g ->
   toMaybe (not $ null xs) $ mapFst (xs!!) $ randomR (0, length xs - 1) g

enumerateRandom :: IO ()
enumerateRandom = do
   let boardSize = standardBoardSize
   let fleetList = standardFleetList
   forM_ [0..] $ \seed ->
      Fold.mapM_ (printBoard boardSize) $
      MS.evalStateT
         (enumerateGen select boardSize fleetList)
         (mkStdGen seed)


listsFromBoard :: (Num a) => (a -> b) -> (Int, Int) -> Map (Int, Int) a -> [[b]]
listsFromBoard f (width, height) set =
   FuncHT.outerProduct
      (\y x -> f $ Map.findWithDefault 0 (x,y) set)
      (take height [0..]) (take width [0..])

formatDistr :: (Int, Int) -> Map (Int, Int) Float -> String
formatDistr boardSize set =
   unlines $ map unwords $ listsFromBoard (printf "%.3f") boardSize set

formatAbsDistr :: (Int, Int) -> Map (Int, Int) Word64 -> String
formatAbsDistr boardSize set =
   unlines $ map unwords $ listsFromBoard (printf "%d") boardSize set

sumMaps :: [Map (Int, Int) Int] -> Map (Int, Int) Int
sumMaps = List.foldl' ((force .) . Map.unionWith (+)) Map.empty

sumMapsStorable ::
   (Int, Int) -> [Map (Int, Int) Word64] -> Map (Int, Int) Word64
sumMapsStorable boardSize =
   Map.fromList . zip (boardCoords boardSize) . SV.unpack .
   let zeroBoard = Map.fromList $ map (flip (,) 0) (boardCoords boardSize)
       numSquares = uncurry (*) boardSize
       checkLength x =
         if SV.length x == numSquares
           then x
           else error "invalid keys in counter board"
   in List.foldl' ((force .) . SV.zipWith (+)) (SV.replicate numSquares 0) .
      map (checkLength . SV.pack . Map.elems . flip Map.union zeroBoard)

estimateDistribution :: IO ()
estimateDistribution = do
   let boardSize = standardBoardSize
   let fleetList = standardFleetList
   let num = 100000
   putStr $ ('\n':) $ formatDistr boardSize $
      Map.map (\n -> fromIntegral n / fromIntegral num) $
      sumMapsStorable boardSize $
      map (Map.map (\b -> if b then 1 else 0)) $
      take num $ catMaybes $
      flip map [0..] $ \seed ->
      MS.evalStateT
         (enumerateGen select boardSize fleetList)
         (mkStdGen seed)

exactDistribution :: IO ()
exactDistribution = do
   let boardSize = standardBoardSize
   let fleetList = [(2,1), (3,2)]
   putStr $ ('\n':) $ formatAbsDistr boardSize $
      sumMapsStorable boardSize $
      map (Map.map (\b -> if b then 1 else 0)) $
      enumerateGen id boardSize fleetList

{-
110984 157686 189232 183236 181578 181578 183236 189232 157686 110984
157686 190520 213246 203776 201766 201766 203776 213246 190520 157686
189232 213246 232008 221676 220274 220274 221676 232008 213246 189232
183236 203776 221676 211572 210458 210458 211572 221676 203776 183236
181578 201766 220274 210458 209428 209428 210458 220274 201766 181578
181578 201766 220274 210458 209428 209428 210458 220274 201766 181578
183236 203776 221676 211572 210458 210458 211572 221676 203776 183236
189232 213246 232008 221676 220274 220274 221676 232008 213246 189232
157686 190520 213246 203776 201766 201766 203776 213246 190520 157686
110984 157686 189232 183236 181578 181578 183236 189232 157686 110984

real    0m37.341s
user    0m37.162s
sys     0m0.128s
-}

tikzBrightnessField :: (Double,Double) -> [[Double]] -> String
tikzBrightnessField (lower,upper) xs =
   unlines $
   zipWith
      (\num row ->
         printf "\\brightnessrow{%d}{%s}" num $
         List.intercalate "," $ map (printf "%02d") $
         map (\val -> round (100*(val-lower)/(upper-lower)) :: Int) row)
      [0::Int ..] xs
