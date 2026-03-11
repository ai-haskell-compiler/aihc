module Game.Labyrinth where

import qualified System.Random as Rnd
import qualified System.IO as IO
import Game.Utility (randomSelect)

import qualified Graphics.Ascii.Haha.Terminal as ANSI
import qualified Text.PrettyPrint.Boxes as Box
import Text.PrettyPrint.Boxes (Box)
import Text.Printf (printf)

import qualified Test.QuickCheck as QC

import qualified Control.Monad.Exception.Synchronous as ME
import qualified Control.Monad.Trans.State as MS
import qualified Control.Applicative.HT as App
import qualified Control.Functor.HT as FuncHT
import Control.Monad (mfilter)
import Control.Applicative (Applicative, pure, (<*>), (<$>))

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.Monoid.HT as Mn
import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.EnumMap as EnumMap
import qualified Data.EnumSet as EnumSet
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Bits as Bits
import qualified Data.Char as Char
import Data.EnumMap (EnumMap)
import Data.EnumSet (EnumSet)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Bits ((.&.), (.|.))
import Data.Word (Word64)
import Data.Traversable (Traversable, sequenceA)
import Data.Foldable (Foldable, foldMap)
import Data.NonEmpty ((!:))
import Data.List (zipWith4)
import Data.Monoid (Monoid, mconcat, mappend, mempty)
import Data.Semigroup (Semigroup, (<>))
import Data.Tuple.HT (mapPair, mapFst, mapSnd)
import Data.Ord.HT (comparing)


{- $setup
>>> import qualified Game.Labyrinth as Labyrinth
-}

newtype Board = Board Word64
   deriving (Eq)

instance Show Board where
   showsPrec p (Board bits) =
      showParen (p>=10) $
         showString "Board " . showString (printf "0x%016x" bits)

instance Semigroup Board where
   Board x <> Board y = Board (x.|.y)

instance Monoid Board where
   mempty = Board 0
   mappend = (<>)

instance QC.Arbitrary Board where
   arbitrary = Board . (boardMask .&.) <$> QC.arbitrary


data Coord = C0 | C1 | C2 | C3 | C4 | C5 | C6
   deriving (Eq, Ord, Enum, Bounded, Show)

instance QC.Arbitrary Coord where
   arbitrary = QC.arbitraryBoundedEnum

boardIndex :: (Coord,Coord) -> Int
boardIndex (row,column) = fromEnum row * 8 + fromEnum column

isCellFixed :: (Coord,Coord) -> Bool
isCellFixed (row,column) = even (fromEnum row) && even (fromEnum column)

boardGet :: Board -> (Coord,Coord) -> Bool
boardGet (Board bits) pos = Bits.testBit bits $ boardIndex pos

boardSet :: Bool -> (Coord,Coord) -> Board
boardSet b pos = Board $ fromIntegral (fromEnum b) `Bits.shiftL` boardIndex pos

singleCell :: (Coord,Coord) -> Board
singleCell = Board . Bits.bit . boardIndex

listsFromBoard :: Board -> [[Bool]]
listsFromBoard board =
   ListHT.outerProduct (curry $ boardGet board) allEnums allEnums

allEnums :: (Enum a, Bounded a) => [a]
allEnums = [minBound .. maxBound]

rowMask, columnMask, boardMask :: Word64
rowMask = 0x7F
columnMask = 0x1010101010101
boardMask = rowMask*columnMask

rowKMask, columnKMask :: Coord -> Word64
rowKMask pos = rowMask `Bits.shiftL` (8 * fromEnum pos)
columnKMask pos = columnMask `Bits.shiftL` fromEnum pos

infixl 7 .-.

(.-.) :: Bits.Bits a => a -> a -> a
a.-.b = a .&. Bits.complement b

shiftBoardLeft :: Board -> Board
shiftBoardLeft (Board bits) =
   Board $ (bits .-. columnKMask C0) `Bits.shiftR` 1

shiftBoardRight :: Board -> Board
shiftBoardRight (Board bits) =
   Board $ (bits .-. columnKMask C6) `Bits.shiftL` 1

shiftBoardUp :: Board -> Board
shiftBoardUp (Board bits) =
   Board $ (bits .-. rowKMask C0) `Bits.shiftR` 8

shiftBoardDown :: Board -> Board
shiftBoardDown (Board bits) =
   Board $ (bits .-. rowKMask C6) `Bits.shiftL` 8


splitBoard :: Board -> Board -> (Board,Board)
splitBoard (Board mask) (Board board) =
   (Board (board.&.mask), Board (board.-.mask))

{- |
prop> \k b -> b == Labyrinth.shiftRowLeft k (Labyrinth.shiftRowRight k b)
prop> \k b -> b == Labyrinth.shiftRowRight k (Labyrinth.shiftRowLeft k b)
-}
shiftRowLeft :: Coord -> (Bool, Board) -> (Bool, Board)
shiftRowLeft r (extra,board) =
   let (row, remainder) = splitBoard (Board $ rowKMask r) board
   in (boardGet row (r,C0),
       remainder <> shiftBoardLeft row <> boardSet extra (r,C6))

shiftRowRight :: Coord -> (Bool, Board) -> (Bool, Board)
shiftRowRight r (extra,board) =
   let (row, remainder) = splitBoard (Board $ rowKMask r) board
   in (boardGet row (r,C6),
       remainder <> shiftBoardRight row <> boardSet extra (r,C0))

{- |
prop> \k b -> b == Labyrinth.shiftColumnUp k (Labyrinth.shiftColumnDown k b)
prop> \k b -> b == Labyrinth.shiftColumnDown k (Labyrinth.shiftColumnUp k b)
-}
shiftColumnUp :: Coord -> (Bool, Board) -> (Bool, Board)
shiftColumnUp c (extra,board) =
   let (column, remainder) = splitBoard (Board $ columnKMask c) board
   in (boardGet column (C0,c),
       remainder <> shiftBoardUp column <> boardSet extra (C6,c))

shiftColumnDown :: Coord -> (Bool, Board) -> (Bool, Board)
shiftColumnDown c (extra,board) =
   let (column, remainder) = splitBoard (Board $ columnKMask c) board
   in (boardGet column (C6,c),
       remainder <> shiftBoardDown column <> boardSet extra (C0,c))


cycleBoardLeft :: Board -> Board
cycleBoardLeft (Board bits) =
   Board $
      (bits .-. columnKMask C0) `Bits.shiftR` 1
      .|.
      (bits .&. columnKMask C0) `Bits.shiftL` 6

cycleBoardRight :: Board -> Board
cycleBoardRight (Board bits) =
   Board $
      (bits .-. columnKMask C6) `Bits.shiftL` 1
      .|.
      (bits .&. columnKMask C6) `Bits.shiftR` 6

cycleBoardUp :: Board -> Board
cycleBoardUp (Board bits) =
   Board $
      (bits .-. rowKMask C0) `Bits.shiftR` 8
      .|.
      (bits .&. rowKMask C0) `Bits.shiftL` (8*6)

cycleBoardDown :: Board -> Board
cycleBoardDown (Board bits) =
   Board $
      (bits .-. rowKMask C6) `Bits.shiftL` 8
      .|.
      (bits .&. rowKMask C6) `Bits.shiftR` (8*6)

cycleGen :: (Board -> Board) -> (Coord -> Word64) -> Coord -> Board -> Board
cycleGen cycleBoard mask coord =
   uncurry mappend . mapFst cycleBoard . splitBoard (Board $ mask coord)

{- |
prop> \k b -> b == Labyrinth.cycleRowLeft k (Labyrinth.cycleRowRight k b)
prop> \k b -> b == Labyrinth.cycleRowRight k (Labyrinth.cycleRowLeft k b)
-}
cycleRowLeft :: Coord -> Board -> Board
cycleRowLeft = cycleGen cycleBoardLeft rowKMask

cycleRowRight :: Coord -> Board -> Board
cycleRowRight = cycleGen cycleBoardRight rowKMask

{- |
prop> \k b -> b == Labyrinth.cycleColumnUp k (Labyrinth.cycleColumnDown k b)
prop> \k b -> b == Labyrinth.cycleColumnDown k (Labyrinth.cycleColumnUp k b)
-}
cycleColumnUp :: Coord -> Board -> Board
cycleColumnUp = cycleGen cycleBoardUp columnKMask

cycleColumnDown :: Coord -> Board -> Board
cycleColumnDown = cycleGen cycleBoardDown columnKMask

cycleStripe :: Border -> Coord -> Board -> Board
cycleStripe b =
   case b of
      West -> cycleRowRight
      East -> cycleRowLeft
      South -> cycleColumnUp
      North -> cycleColumnDown



rotateMasked ::
   (Directions Board -> Directions Board) ->
   Board -> Directions Board -> Directions Board
rotateMasked rotate mask board =
   case FuncHT.unzip (splitBoard mask <$> board) of
      (masked, remaining) -> rotate masked <> remaining

rotateDir90, rotateDir180 :: Directions a -> Directions a
rotateDir90 (Directions n w s e) = Directions e n w s
rotateDir180 (Directions n w s e) = Directions s e n w

randomRotate ::
   (Rnd.RandomGen g) => Directions Board -> MS.State g (Directions Board)
randomRotate board =
   ($ board) <$>
   App.lift2 (.)
      (rotateMasked rotateDir180 . Board <$> MS.state Rnd.random)
      (rotateMasked rotateDir90  . Board <$> MS.state Rnd.random)


data Directions a = Directions {north, west, south, east :: a}
   deriving (Eq)

instance Semigroup a => Semigroup (Directions a) where
   (<>) = App.lift2 (<>)

instance Monoid a => Monoid (Directions a) where
   mempty = pure mempty
   mappend = App.lift2 mappend

instance Functor Directions where
   fmap f (Directions n w s e) = Directions (f n) (f w) (f s) (f e)

instance Foldable Directions where
   foldMap f (Directions n w s e) =
      f n `mappend` f w `mappend` f s `mappend` f e

instance Traversable Directions where
   sequenceA (Directions n w s e) = App.lift4 Directions n w s e

instance Applicative Directions where
   pure a = Directions a a a a
   Directions fn fw fs fe <*> Directions n w s e =
      Directions (fn n) (fw w) (fs s) (fe e)

transposeDirections :: Directions [[a]] -> [[Directions a]]
transposeDirections (Directions n w s e) =
   zipWith4 (zipWith4 Directions) n w s e


boolChar :: Char -> Bool -> Char
boolChar c b = if b then c else ' '

formatBoard :: Board -> String
formatBoard = unlines . map (map (boolChar '*')) . listsFromBoard

wallChar :: Char
wallChar = '\x2588'

wayChar :: Char
wayChar = '.'

selectWallWayChar :: Bool -> Bool -> Char
selectWallWayChar wall connect =
   case (wall, connect) of
      (False, False) -> ' '
      (True,  False) -> wallChar
      (False, True)  -> wayChar
      (True,  True)  -> error "character cannot be both wall and way"

boxTable :: [[Box]] -> Box
boxTable = Box.vcat Box.left . map (Box.hcat Box.top)

formatCell ::
   EnumSet Player -> Directions Bool -> Char -> Directions Bool -> Box
formatCell players connects center walls =
   let plChar p =
         if EnumSet.member p players then playerChar True p else wallChar
   in case App.lift2 selectWallWayChar walls connects of
         Directions n w s e ->
            boxTable $ map (map Box.char) $
            [plChar Player0, n, plChar Player1] :
            [w, center, e] :
            [plChar Player2, s, plChar Player3] :
            []

format :: Directions Board -> Box
format =
   boxTable .
   map (map (formatCell EnumSet.empty (pure False) ' ')) .
   transposeDirections . fmap listsFromBoard

formatWays :: Directions Board -> Board -> Box
formatWays walls reachable =
   boxTable $
   zipWith3
      (zipWith3 (formatCell EnumSet.empty))
      (transposeDirections $ fmap listsFromBoard $
       connectReachable walls reachable)
      (map (map (boolChar wayChar)) $ listsFromBoard reachable)
      (transposeDirections $ fmap listsFromBoard walls)

connectReachable :: Directions Board -> Board -> Directions Board
connectReachable walls reachable =
   let blockWall (Board mask) (Board bits) = Board $ bits.-.mask
       shiftAndBlock block1 shift block0 =
         blockWall
            (block1 walls <> shift (block0 walls))
            (reachable <> shift reachable)
   in Directions {
         north = shiftAndBlock north shiftBoardDown south,
         south = shiftAndBlock south shiftBoardUp north,
         west = shiftAndBlock west shiftBoardRight east,
         east = shiftAndBlock east shiftBoardLeft west
      }

arrows :: Directions Char
arrows =
   Directions {
      north = '\x21D3', west = '\x21D2', south = '\x21D1', east = '\x21D0'
   }

arrowFrame :: Maybe (Border, Coord) -> Box -> Box
arrowFrame forbidden box =
   let rowBox arrow c = Box.text [' ', arrow, c]
       columnBox c0 c1 = Box.alignVert Box.center1 3 $ Box.text [c0, c1]
       makeBoxes =
         Directions {
            north = rowBox, west = flip columnBox,
            south = rowBox, east = columnBox
         }
       horizVert h v = Directions {north = h, south = h, west = v, east = v}
       labels =
         maybe id
            (\(b,c) -> singleBorder b $ Map.insert c)
            forbidden (pure $ const id)
         <*>
         horizVert (Box.emptyBox 1 3) (Box.emptyBox 3 1)
         <*>
         App.lift2 fmap (makeBoxes <*> arrows) shiftCharMap
       labelBoxes =
         horizVert (Box.hsep 3 Box.top) (Box.vsep 3 Box.center1)
         <*>
         ((Box.nullBox :) . Map.elems <$> labels)
       emptyBox = Box.emptyBox 1 2
   in boxTable $
      [emptyBox, north labelBoxes, emptyBox] :
      [west labelBoxes, box, east labelBoxes] :
      [emptyBox, south labelBoxes, emptyBox] :
      []

constMap :: (Ord k) => [k] -> a -> Map k a
constMap keys a = Map.fromList $ map (flip (,) a) keys

shiftCharMap :: Directions (Map Coord Char)
shiftCharMap =
   flip MS.evalState 'A' $ Trav.traverse Trav.sequence $
   pure $ constMap [C1,C3,C5] $ MS.state $ \c -> (c, succ c)

arrowMap :: Map Char (Border,Coord)
arrowMap =
   Fold.fold $
   App.lift2
      (\b -> Map.fromList . map (\(c,char) -> (char,(b,c))) . Map.toList)
      borderSet shiftCharMap


coordinateFrame :: Box -> Box
coordinateFrame box =
   let row =
         Box.emptyBox 1 1 Box.<>
         (Box.hsep 2 Box.top $ map Box.char $ Map.keys columnMap)
       column =
         Box.emptyBox 1 1 Box.//
         (Box.vsep 2 Box.top $ map Box.char $ Map.keys rowMap)
       emptyBox = Box.emptyBox 1 2
   in boxTable $
      [emptyBox, row, emptyBox] :
      [column Box.<> Box.emptyBox 1 1, box, Box.emptyBox 1 1 Box.<> column] :
      [emptyBox, row, emptyBox] :
      []

rowMap, columnMap :: Map Char Coord
rowMap = Map.fromList $ zip ['A'..] allEnums
columnMap = Map.fromList $ zip ['0'..] allEnums


data Shape = I | L | T
   deriving (Eq, Ord, Enum, Show)

type InitTile = (Shape, Maybe Char)

data Rotation = R0 | R1 | R2 | R3
   deriving (Eq, Ord, Enum, Bounded)


dirsFromShape :: Shape -> Directions Bool
dirsFromShape shape =
   case shape of
      I -> (pure True) {north = False, south = False}
      L -> (pure True) {north = False, east = False}
      T -> (pure True) {west = False, east = False, south = False}

defaultBoardDirs :: [[Directions Bool]]
defaultBoardDirs =
   let f :: Rotation -> Shape -> Directions Bool
       f rot =
         (if Bits.testBit (fromEnum rot) 1 then rotateDir180 else id) .
         (if Bits.testBit (fromEnum rot) 0 then rotateDir90 else id) .
         dirsFromShape
       r0 = f R0; r1 = f R1; r2 = f R2; r3 = f R3
   in [r3 L, r0 T, r0 T, r2 L] :
      [r1 T, r1 T, r0 T, r3 T] :
      [r1 T, r2 T, r3 T, r3 T] :
      [r0 L, r2 T, r2 T, r1 L] :
      []

defaultBoard :: Directions Board
defaultBoard =
   let evenCoords = [C0, C2 .. C6]
   in mconcat $
      zipWith
         (\row ->
            mconcat .
            zipWith (\col -> fmap (flip Mn.when (singleCell (row,col))))
               evenCoords)
         evenCoords defaultBoardDirs


randomChoose :: (Rnd.RandomGen g) => MS.StateT [a] (MS.State g) a
randomChoose = MS.StateT $ randomSelect . ListHT.removeEach

{-
	total	fixed	symbol
		position
I:	 13	  0	  0
T:	 18	 12	 18
L:	 19	  4	 10

symbols: 24	 12
-}
shuffle :: (Rnd.RandomGen g) => MS.State g (InitTile, [InitTile])
shuffle =
   let (symbolsL, symbolsT) = splitAt 6 moveableSymbols
       xs =
         replicate 13 (I, Nothing) ++
         map (\s -> (T, Just s)) symbolsT ++
         replicate 9 (L, Nothing) ++
         map (\s -> (L, Just s)) symbolsL
   in flip MS.evalStateT xs $ App.lift2 (,) randomChoose $
      sequence $ Match.replicate (drop 1 xs) randomChoose

singleCellShape :: (Coord, Coord) -> Shape -> Directions Board
singleCellShape pos shape =
   flip Mn.when (singleCell pos) <$> dirsFromShape shape

grid :: [a] -> [(a,a)]
grid xs = App.lift2 (,) xs xs

layoutMoveableCells :: [InitTile] -> (Directions Board, SymbolMap)
layoutMoveableCells =
   mapPair (Fold.fold . Map.mapWithKey singleCellShape, Map.mapMaybe id) .
   FuncHT.unzip . Map.fromList .
   zip (filter (not . isCellFixed) $ grid allEnums)

randomBoard ::
   (Rnd.RandomGen g) => MS.State g (InitTile, (Directions Board, SymbolMap))
randomBoard = do
   (extra,shapes) <- shuffle
   let (board,symbolMap) = layoutMoveableCells shapes
   moveable <- randomRotate board
   return (extra, (defaultBoard <> moveable, symbolMap))

shuffleTargets :: (Rnd.RandomGen g) => MS.State g [Char]
shuffleTargets =
   let xs = moveableSymbols ++ fixedSymbols
   in flip MS.evalStateT xs $ sequence $ Match.replicate xs randomChoose

associateTargets ::
   NonEmpty.T [] Player -> [Char] ->
   NonEmpty.T Seq (Player, NonEmpty.T [] Char)
associateTargets players =
   NonEmpty.mapTail Seq.fromList .
   NonEmptyC.zipWith
      (\player targets ->
         (player, NonEmpty.snoc targets $ playerChar False player))
      players .
   maybe (error "empty players list") id .
   NonEmpty.fetch . ListHT.sliceHorizontal (1 + length (NonEmpty.tail players))


bfsStep :: Directions Board -> Board -> Board
bfsStep walls board =
   let blockWall (Board mask) (Board bits) = Board $ bits.-.mask
       shiftAndBlock block1 shift block0 =
         blockWall (block1 walls) (shift (blockWall (block0 walls) board))
   in board
      <>
      shiftAndBlock east  shiftBoardLeft  west
      <>
      shiftAndBlock west  shiftBoardRight east
      <>
      shiftAndBlock north shiftBoardDown  south
      <>
      shiftAndBlock south shiftBoardUp    north

bfs :: Directions Board -> Board -> Board
bfs walls =
   snd . head . dropWhile (not . fst) .
   ListHT.mapAdjacent (\x y -> (x==y, x)) . iterate (bfsStep walls)


data Player = Player0 | Player1 | Player2 | Player3
   deriving (Eq, Ord, Enum, Bounded, Show)

playerChar :: Bool -> Player -> Char
playerChar b p =
   case p of
      Player0 -> if b then '\x2666' else '\x2662'
      Player1 -> if b then '\x2665' else '\x2661'
      Player2 -> if b then '\x2660' else '\x2664'
      Player3 -> if b then '\x2663' else '\x2667'

playerSymbols, fixedSymbols, moveableSymbols :: [Char]
playerSymbols = map (playerChar False) allEnums
(fixedSymbols, moveableSymbols) = splitAt 12 $ take 24 ['\x2600' ..]

type SymbolMap = Map (Coord,Coord) Char

fixedSymbolMap :: SymbolMap
fixedSymbolMap =
   let corners = Map.fromList $ zip (grid [C0,C6]) playerSymbols
   in Map.union corners $ Map.fromList $
      zip (filter (flip Map.notMember corners) $ grid [C0, C2 .. C6])
         fixedSymbols

randomSymbolMap :: (Rnd.RandomGen g) => MS.State g SymbolMap
randomSymbolMap =
   Map.fromList <$>
   MS.evalStateT
      (mapM (\symbol -> flip (,) symbol <$> randomChoose) moveableSymbols)
      (filter (flip Map.notMember fixedSymbolMap) $ grid [C0 .. C6])

removeFromMap :: (Ord k) => k -> Map k a -> (Maybe a, Map k a)
removeFromMap = Map.updateLookupWithKey (\ _k _a -> Nothing)

insertMaybeMap :: (Ord k) => k -> Maybe a -> Map k a -> Map k a
insertMaybeMap k = maybe id (Map.insert k)

shiftRowLeftMap :: Coord -> (Maybe Char, SymbolMap) -> (Maybe Char, SymbolMap)
shiftRowLeftMap rowPos (extra, m) =
   let (row, remainder) = Map.partitionWithKey (\(r,_c) _ -> r == rowPos) m
       (x,xs) = removeFromMap (rowPos,minBound) row
   in (x,
       remainder <>
         insertMaybeMap (rowPos,maxBound) extra
            (Map.mapKeysMonotonic (\(r,c) -> (r, pred c)) xs))

shiftRowRightMap :: Coord -> (Maybe Char, SymbolMap) -> (Maybe Char, SymbolMap)
shiftRowRightMap rowPos (extra, m) =
   let (row, remainder) = Map.partitionWithKey (\(r,_c) _ -> r == rowPos) m
       (x,xs) = removeFromMap (rowPos,maxBound) row
   in (x,
       remainder <>
         insertMaybeMap (rowPos,minBound) extra
            (Map.mapKeysMonotonic (\(r,c) -> (r, succ c)) xs))

shiftColumnUpMap ::
   Coord -> (Maybe Char, SymbolMap) -> (Maybe Char, SymbolMap)
shiftColumnUpMap columnPos (extra, m) =
   let (column, remainder) =
         Map.partitionWithKey (\(_r,c) _ -> c == columnPos) m
       (x,xs) = removeFromMap (minBound,columnPos) column
   in (x,
       remainder <>
         insertMaybeMap (maxBound,columnPos) extra
            (Map.mapKeysMonotonic (\(r,c) -> (pred r, c)) xs))

shiftColumnDownMap ::
   Coord -> (Maybe Char, SymbolMap) -> (Maybe Char, SymbolMap)
shiftColumnDownMap columnPos (extra, m) =
   let (column, remainder) =
         Map.partitionWithKey (\(_r,c) _ -> c == columnPos) m
       (x,xs) = removeFromMap (maxBound,columnPos) column
   in (x,
       remainder <>
         insertMaybeMap (minBound,columnPos) extra
            (Map.mapKeysMonotonic (\(r,c) -> (succ r, c)) xs))


formatWithSymbols :: BoardState -> Board -> Box
formatWithSymbols s reachable =
   boxTable $
   zipWith4
      (zipWith4
         (\pos connects reach ->
            formatCell
               (EnumMap.keysSet $ EnumMap.filter (pos==) $ statePlayerMap s)
               connects
               (Map.findWithDefault (boolChar wayChar reach) pos $
                stateSymbolMap s)))
      (ListHT.outerProduct (,) allEnums allEnums)
      (transposeDirections $ fmap listsFromBoard $
       connectReachable (stateBoard s) reachable)
      (listsFromBoard reachable)
      (transposeDirections $ fmap listsFromBoard $ stateBoard s)


type PlayerMap = EnumMap Player (Coord,Coord)

shiftPlayersGen ::
   Eq a => (b -> a) -> (b -> b) -> a -> EnumMap k b -> EnumMap k b
shiftPlayersGen select update coord =
   EnumMap.map (\pos -> if select pos == coord then update pos else pos)

shiftRowLeftPlayers, shiftRowRightPlayers,
   shiftColumnUpPlayers, shiftColumnDownPlayers ::
      Coord -> PlayerMap -> PlayerMap
shiftRowLeftPlayers    = shiftPlayersGen fst (mapSnd cyclicPred)
shiftRowRightPlayers   = shiftPlayersGen fst (mapSnd cyclicSucc)
shiftColumnUpPlayers   = shiftPlayersGen snd (mapFst cyclicPred)
shiftColumnDownPlayers = shiftPlayersGen snd (mapFst cyclicSucc)


data BoardState =
   BoardState {
      stateBoard :: Directions Board,
      stateSymbolMap :: SymbolMap,
      statePlayerMap :: PlayerMap
   }

type Tile = (Directions Bool, Maybe Char)

cyclicPred :: (Eq a, Enum a, Bounded a) => a -> a
cyclicPred x = if x==minBound then maxBound else pred x

cyclicSucc :: (Eq a, Enum a, Bounded a) => a -> a
cyclicSucc x = if x==maxBound then minBound else succ x

shiftStateGen ::
   (Coord -> (Bool, Board) -> (Bool, Board)) ->
   (Coord -> (Maybe Char, SymbolMap) -> (Maybe Char, SymbolMap)) ->
   (Coord -> PlayerMap -> PlayerMap) ->
   Coord -> (Tile, BoardState) -> (Tile, BoardState)
shiftStateGen shiftBoard shiftMap shiftPlayers rowPos ((tile,symbol), state) =
   let (newExtra, newSymbolMap) =
         shiftMap rowPos (symbol, stateSymbolMap state)
       (newTile, newBoard) =
         FuncHT.unzip $ fmap (shiftBoard rowPos) $
         App.lift2 (,) tile (stateBoard state)
       newPlayerMap = shiftPlayers rowPos $ statePlayerMap state
   in ((newTile, newExtra),
       BoardState {
         stateBoard = newBoard,
         stateSymbolMap = newSymbolMap,
         statePlayerMap = newPlayerMap
       })

data Border = North | West | South | East
   deriving (Eq, Ord, Enum, Bounded, Show)

borderSet :: Directions Border
borderSet = Directions {north = North, west = West, south = South, east = East}

singleBorder :: Border -> a -> Directions a -> Directions a
singleBorder b a m =
   case b of
      North -> m{north = a}
      South -> m{south = a}
      East  -> m{east = a}
      West  -> m{west = a}

lookupBorder :: Border -> Directions a -> a
lookupBorder b =
   case b of
      North -> north
      South -> south
      East  -> east
      West  -> west

flipBorder :: Border -> Border
flipBorder b =
   case b of
      North -> South
      South -> North
      East -> West
      West -> East

shiftState :: Border -> Coord -> (Tile, BoardState) -> (Tile, BoardState)
shiftState b =
   case b of
      West -> shiftStateGen shiftRowRight shiftRowRightMap shiftRowRightPlayers
      East -> shiftStateGen shiftRowLeft shiftRowLeftMap shiftRowLeftPlayers
      South -> shiftStateGen shiftColumnUp shiftColumnUpMap shiftColumnUpPlayers
      North ->
         shiftStateGen shiftColumnDown shiftColumnDownMap shiftColumnDownPlayers


reachableFromPlayer :: Player -> BoardState -> Board
reachableFromPlayer player state =
   bfs (stateBoard state) $ singleCell $
   EnumMap.findWithDefault (error "player not in playerMap") player $
   statePlayerMap state

shapeRotations :: Eq a => Directions a -> [(Rotation, Directions a)]
shapeRotations shape =
   zip (if rotateDir180 shape == shape then [R0,R1] else allEnums) $
   iterate rotateDir90 shape

reachable1Single :: Board -> (Border, Coord) -> Tile -> BoardState -> Bool
reachable1Single cloud0 (b,pos) tile state0 =
   let state1 = snd $ shiftState b pos (tile, state0)
       cloud1 = bfs (stateBoard state1) $ cycleStripe b pos cloud0
   in any (boardGet cloud1) $ Map.keys $ stateSymbolMap state1

reachable1 :: (Tile, BoardState) -> [((Border, Coord), ([Rotation], Bool))]
reachable1 ((shape,symbol),state) =
   filter (not . null . fst . snd) $
   flip map (App.lift2 (,) allEnums [C1,C3,C5]) $ \shift ->
      (shift,
       mapPair (map fst, null) $
       ListHT.partition
          (\(_rot,rotShape) ->
             reachable1Single
                (Fold.foldMap singleCell $ statePlayerMap state)
                shift (rotShape,symbol) state) $
       shapeRotations shape)

reachable2Single :: Board -> (Border, Coord) -> Tile -> BoardState -> Int
reachable2Single cloud0 (b,pos) tile state0 =
   let (tile1,state1) = shiftState b pos (tile, state0)
       cloud1 = bfs (stateBoard state1) $ cycleStripe b pos cloud0
   in length $
      filter
         (\shift ->
            reachable1Single cloud1 shift (pure True, snd tile1) state1) $
      filter ((flipBorder b, pos) /=) $
      App.lift2 (,) allEnums [C1,C3,C5]

reachable2 :: (Tile, BoardState) -> [(((Border, Coord), Rotation), Int)]
reachable2 ((shape,symbol),state) =
   map
      (\(shift, (rot,rotShape)) ->
         ((shift, rot),
          reachable2Single
            (Fold.foldMap singleCell $ statePlayerMap state)
            shift (rotShape,symbol) state)) $
   App.lift2 (,)
      (App.lift2 (,) allEnums [C1,C3,C5])
      (shapeRotations shape)


formatRotation :: Rotation -> String
formatRotation = show . fromEnum

formatShift :: ((Border, Coord), Rotation) -> String
formatShift ((b, pos), rot) =
   Map.findWithDefault (error "forbidden coordinate")
      pos (lookupBorder b shiftCharMap)
   :
   formatRotation rot

formatShifts :: ((Border, Coord), ([Rotation], Bool)) -> String
formatShifts ((b, pos), (rot,complete)) =
   Map.findWithDefault (error "forbidden coordinate")
      pos (lookupBorder b shiftCharMap)
   :
   if complete then "*" else concatMap formatRotation rot

parseShift ::
   Maybe (Border, Coord) -> String ->
   ME.Exceptional String ((Border, Coord), Rotation)
parseShift forbidden input =
   case input of
      [arrowChar, rotation] ->
         App.lift2 (,)
            (do
               pos <-
                  ME.fromMaybe "not an arrow letter" $
                  Map.lookup (Char.toUpper arrowChar) arrowMap
               if Just pos == forbidden
                  then ME.throw "reversing the last shift is forbidden"
                  else return pos)
            (case rotation of
               '0' -> return R0
               '1' -> return R1
               '2' -> return R2
               '3' -> return R3
               _ -> ME.throw "not a rotation number from 0-3")
      _ -> ME.throw "need two input characters"

parsePosition :: Board -> String -> ME.Exceptional String (Coord, Coord)
parsePosition reachable input =
   case input of
      [rowChar, columnChar] -> do
         pos <-
            App.lift2 (,)
               (ME.fromMaybe "row letter out of range" $
                Map.lookup (Char.toUpper rowChar) rowMap)
               (ME.fromMaybe "column number out of range" $
                Map.lookup columnChar columnMap)
         if boardGet reachable pos
            then return pos
            else ME.throw "position unreachable"
      _ -> ME.throw "need two input characters"

inputLoop :: (String -> ME.Exceptional String a) -> IO a
inputLoop parse =
   let go = ME.switch (\msg -> putStrLn msg >> go) return . parse =<< getLine
   in IO.hFlush IO.stdout >> go

playerCharSet :: EnumSet Char
playerCharSet = EnumSet.fromList $ map (playerChar True) allEnums

inversePlayerChar :: Char -> String
inversePlayerChar c =
   if EnumSet.member c playerCharSet
      then ANSI.esc "" (ANSI.reverse []) "m" ++ c : ANSI.reset
      else [c]

printBox :: Box -> IO ()
printBox = putStr . concatMap inversePlayerChar . Box.render

main :: IO ()
main = do
   let gameLoop forbiddenShift (shape0, msymbol0) state0
            (NonEmpty.Cons (player,targets) remPlayers) = do
         let rotatedShapes =
               EnumMap.fromList $ zip allEnums $ iterate rotateDir90 shape0
         let symbol0 = maybe ' ' id msymbol0
         printBox $ Box.hsep 5 Box.top $
            arrowFrame forbiddenShift (formatWithSymbols state0 mempty)
            :
            (Box.vcat Box.center1 $ Fold.fold $
             EnumMap.mapWithKey
               (\k sh ->
                  [Box.text $ formatRotation k,
                   formatCell EnumSet.empty (pure False) symbol0 sh,
                   Box.emptyBox 1 0])
               rotatedShapes)
            :
            []
         let NonEmpty.Cons target remTargets = targets
         let state0Target =
              state0{
                 stateSymbolMap =
                    Map.filter (target==) $ stateSymbolMap state0,
                 statePlayerMap =
                    EnumMap.intersectionWith const (statePlayerMap state0)
                       (EnumMap.singleton player ())
              }
         let msymbol0Target = mfilter (target==) msymbol0
         let isAllowed (shift,_) = forbiddenShift /= Just shift
         case splitAt 5 $ filter isAllowed $
              reachable1 ((shape0,msymbol0Target),state0Target) of
            ([], _) -> do
               let xs =
                    take 5 $ List.sortBy (flip $ comparing snd) $
                    filter (isAllowed . fst) $
                    reachable2 ((shape0,msymbol0Target),state0Target)
               putStrLn $ "promising: " ++
                  List.intercalate ", "
                     (map (\(x,l) -> printf "%s(%d)" (formatShift x) l) xs
                        ++ ["..."])
            (xs, ys) ->
               putStrLn $ "hint: " ++
               List.intercalate ", "
                  (map formatShifts xs ++ take 1 (map (const "...") ys))
         let goal = playerChar True player : '\x2192' : target : []
         putStr $ goal ++ " Enter position and rotation of inserted piece: "
         ((border,coord), rotation) <- inputLoop $ parseShift forbiddenShift
         let (shape1,state1) =
               shiftState border coord
                  ((rotatedShapes EnumMap.! rotation, msymbol0), state0)
         let reachable = reachableFromPlayer player state1
         printBox $ coordinateFrame $ formatWithSymbols state1 reachable
         putStrLn ""
         putStr $ goal ++ " Enter coordinates of new player position: "
         pos <- inputLoop $ parsePosition reachable
         mNewPlayers <-
            if Just target /= Map.lookup pos (stateSymbolMap state1)
               then return $ Just $ NonEmpty.snoc remPlayers (player,targets)
               else do
                  putStrLn $ "found target: " ++ target : ""
                  case NonEmpty.fetch remTargets of
                     Nothing -> do
                        putStrLn $ "All targets found!"
                        return $ NonEmpty.fetch remPlayers
                     Just remTargetsNE@(NonEmpty.Cons newTarget _) -> do
                        putStrLn $
                           "new target: " ++ newTarget :
                           ", remaining: " ++ show (length remTargets)
                        return $ Just $
                           NonEmpty.snoc remPlayers (player, remTargetsNE)
         case mNewPlayers of
            Nothing -> putStrLn "All players found all their targets!"
            Just newPlayers ->
               gameLoop (Just (flipBorder border, coord)) shape1
                  state1{statePlayerMap =
                     EnumMap.insert player pos $ statePlayerMap state1}
                  newPlayers

   let (((shape,msymbol),(board,symbolMap)), shuffledTargets) =
         MS.evalState (App.lift2 (,) randomBoard shuffleTargets) $
         Rnd.mkStdGen 42
   let targets = associateTargets (Player1 !: Player2 : []) shuffledTargets
   let playerMap =
         EnumMap.intersectionWith (const id)
            (EnumMap.fromList $ Fold.toList targets) $
         EnumMap.fromList $ zip allEnums $ grid [C0,C6]
   gameLoop Nothing (dirsFromShape shape, msymbol)
      (BoardState board (fixedSymbolMap <> symbolMap) playerMap)
      targets

{-
let board = snd $ MS.evalState randomBoard $ Rnd.mkStdGen 42
printBox $ formatWays board $ bfs board $ singleCell (C1,C6)
-}
