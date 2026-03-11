{- |
In this approach I construct the board row by row from the bottom to the top.
In every step I maintain the necessary information
in order to know, what ships and positions and orientations
are allowed in the next row.
This information is stored in the Frontier.

possible optimization:
   "meet in the middle"
   compute counts for 5x10 boards and put them together,
   problem:
      for a given frontier there are many other half boards that may match
-}
module Combinatorics.Battleship.Count.ShortenShip where

import qualified Combinatorics.Battleship.Count.CountMap as CountMap
import qualified Combinatorics.Battleship.Count.Counter as Counter
import qualified Combinatorics.Battleship.Count.Frontier as Frontier
import qualified Combinatorics.Battleship.Fleet as Fleet
import qualified Combinatorics.Battleship.Size as Size
import Combinatorics.Battleship.Size (Nat, Size(Size), n6, n8, n10, )

import qualified Control.Monad.Trans.State.Strict as MS
import Control.Monad (when, guard, zipWithM_, forM_, )
import Control.Applicative (Alternative, (<|>), )

import Foreign.Storable (Storable, )
import Data.Word (Word64, )

import qualified Data.Map as Map
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.Foldable as Fold
import Data.Map (Map, )
import Data.Monoid (mappend, )
import Data.Tuple.HT (mapFst, mapSnd, )

import Data.Function.HT (nest, )
import Data.List (intercalate, )
import Text.Printf (printf, )

import qualified Test.QuickCheck.Monadic as QCM
import qualified Test.QuickCheck as QC


type Count = Counter.Composed Word64 Word64
type CountMap w = CountMap.T w Count
type CountMapPath w = CountMap.Path w Count
-- type Count = Integer
-- type CountMap w = Map (CountMap.Key w) Count


-- * count all possible fleets on a board with given width

baseCase :: Size w -> CountMap w
baseCase _size =
   CountMap.singleton (Frontier.empty, Fleet.empty) Counter.one

asumTakeFrontier ::
   (Nat w, Alternative f) =>
   Frontier.T w -> Frontier.Position -> Size w -> [f a] -> f a
asumTakeFrontier frontier pos (Size size) =
   Fold.asum . Match.take (takeWhile (Frontier.isFree frontier) [pos .. size-1])

widthRange :: (Nat w) => Size w -> [Int]
widthRange (Size size) = take size [0 ..]

atEnd :: Size w -> Int -> Bool
atEnd (Size size) pos = pos>=size

maxShipSize :: Fleet.ShipSize
maxShipSize = min Fleet.maxSize Frontier.maxShipSize


guardCumulativeSubset :: Fleet.T -> MS.StateT (Frontier.T w, Fleet.T) [] ()
guardCumulativeSubset cumMaxFleet = do
   (frontier, fleet) <- MS.get
   guard $
      Fleet.subset
         (Fleet.cumulate $ addFrontierFleet frontier fleet)
         cumMaxFleet

newShip ::
   Fleet.T -> Fleet.T ->
   Fleet.ShipSize -> MS.StateT (Frontier.T w, Fleet.T) [] ()
newShip cumMaxFleet maxFleet shipSize = do
   MS.modify $ mapSnd $ Fleet.inc shipSize
   guard . flip Fleet.subset maxFleet =<< MS.gets snd
   guardCumulativeSubset cumMaxFleet

insertVertical ::
   (Nat w) =>
   Fleet.T -> Int ->
   Frontier.Position -> MS.StateT (Frontier.T w, Fleet.T) [] ()
insertVertical cumMaxFleet n pos = do
   MS.modify $ mapFst $ Frontier.insertNew pos (Frontier.Vertical n)
   guardCumulativeSubset cumMaxFleet


{- |
In this approach, the fleet contains all ships
also the ones at the frontier.
-}
nextFrontier :: (Nat w) => Size w -> CountMap w -> CountMap w
nextFrontier width =
   CountMap.mergeMany .
   map
      (\((frontier,fleet), cnt) ->
         CountMap.fromList $
         map (flip (,) cnt) $ mergeSymmetricFrontiers $
         map (mapFst (Frontier.dilate width)) $
         transitionFrontier width frontier fleet) .
   CountMap.toAscList

transitionFrontier ::
   (Nat w) => Size w -> Frontier.T w -> Fleet.T -> [(Frontier.T w, Fleet.T)]
transitionFrontier width oldFrontier =
   let go pos =
         when (not $ atEnd width pos) $ do
            let insertVert n =
                  MS.modify $ mapFst $
                  Frontier.insertNew pos (Frontier.Vertical n)
            let updateFleet = MS.modify . mapSnd
            (frontier,fleet) <- MS.get
            case Frontier.lookup oldFrontier pos of
               Frontier.Blocked -> go (pos+1)
               Frontier.Vertical n ->
                  go (pos+2)
                  <|>
                  (do guard (n < maxShipSize)
                      insertVert (n+1)
                      updateFleet (Fleet.inc (n+1) . Fleet.dec n)
                      go (pos+2))
               Frontier.Free ->
                  go (pos+1)
                  <|>
                  (do insertVert 1
                      updateFleet (Fleet.inc 1)
                      go (pos+2))
                  <|>
                  (asumTakeFrontier oldFrontier pos width $
                   zipWith3
                      (\newPos shipSize newFrontierUpdate -> do
                          MS.put (newFrontierUpdate, fleet)
                          updateFleet (Fleet.inc shipSize)
                          go newPos)
                      [pos+2 ..]
                      [1 .. Fleet.maxSize]
                      (tail $
                       scanl
                          (flip (Frontier.blockBounded width))
                          frontier [pos ..]))
   in  MS.execStateT (go 0) . (,) Frontier.empty


count :: (Nat w) => (Size w, Int) -> Fleet.T -> Count
count (width,height) reqFleet =
   Counter.sum $
   map snd $
   filter (\((_front,fleet), _) -> fleet == reqFleet) $
   CountMap.toAscList $
   nest height (nextFrontier width) $ baseCase width


-- * count fleets with an upper bound

{- |
Here we save memory and speed up the computation in the following way:
We stop searching deeper if

1. the fleet becomes larger than the requested fleet
    ("larger" means, that for at least one ship size
     the number of ships is larger than in the requested fleet)

2. the cumulated fleet becomes larger than the cumulated requested fleet
     This is necessary, since we do not know the final length
     of the vertical ships at the frontier.

In this approach,
the fleet does not contain the vertical ships at the frontier.
-}
nextFrontierBounded :: (Nat w) => Size w -> Fleet.T -> CountMap w -> CountMap w
nextFrontierBounded width maxFleet =
--   foldMap is not efficient enough
--   foldl mappend mempty .    -- not efficient enough
   CountMap.mergeMany .
   map
      (\((frontier,fleet), cnt) ->
         CountMap.fromList $
         map (flip (,) cnt) $ mergeSymmetricFrontiers $
         map (mapFst (Frontier.dilate width)) $
         transitionFrontierBounded width maxFleet frontier fleet) .
   CountMap.toAscList

nextFrontierBoundedExternal ::
   (Nat w) => Size w -> Fleet.T -> CountMapPath w -> CountMap w -> IO ()
nextFrontierBoundedExternal width maxFleet path =
   CountMap.writeSorted path .
   map
      (concatMap
         (\((frontier,fleet), cnt) ->
            map (flip (,) cnt) $ mergeSymmetricFrontiers $
            map (mapFst (Frontier.dilate width)) $
            transitionFrontierBounded width maxFleet frontier fleet)) .
   ListHT.sliceVertical bucketSize .
   CountMap.toAscList

transitionFrontierBounded ::
   (Nat w) =>
   Size w -> Fleet.T -> Frontier.T w -> Fleet.T ->
   [(Frontier.T w, Fleet.T)]
transitionFrontierBounded width maxFleet oldFrontier =
   let cumMaxFleet = Fleet.cumulate maxFleet
       go pos =
          when (not $ atEnd width pos) $ do
             (frontier,fleet) <- MS.get
             case Frontier.lookup oldFrontier pos of
                Frontier.Blocked -> go (pos+1)
                Frontier.Vertical n ->
                   (newShip cumMaxFleet maxFleet n
                    <|>
                    (guard (n < maxShipSize) >>
                     insertVertical cumMaxFleet (n+1) pos)
                   >>
                   go (pos+2))
                Frontier.Free ->
                   go (pos+1)
                   <|>
                   (insertVertical cumMaxFleet 1 pos >> go (pos+2))
                   <|>
                   (asumTakeFrontier oldFrontier pos width $
                    zipWith3
                       (\newPos shipSize frontierUpdate -> do
                          MS.put (frontierUpdate,fleet)
                          newShip cumMaxFleet maxFleet shipSize
                          go newPos)
                       [pos+2 ..]
                       [1 .. Fleet.maxSize]
                       (tail $
                        scanl
                           (flip (Frontier.blockBounded width))
                           frontier [pos ..]))
   in  MS.execStateT (go 0) . (,) Frontier.empty


countBounded :: (Nat w) => (Size w, Int) -> Fleet.T -> Count
countBounded (width,height) reqFleet =
   countBoundedFromMap reqFleet $
   nest height (nextFrontierBounded width reqFleet) $ baseCase width


{- |
This solves a different problem.
In this variant the ships are allowed to touch each other.
-}
nextFrontierTouching :: (Nat w) => Size w -> Fleet.T -> CountMap w -> CountMap w
nextFrontierTouching width maxFleet =
   CountMap.mergeMany .
   map
      (\((frontier,fleet), cnt) ->
         CountMap.fromList $
         map (flip (,) cnt) $ mergeSymmetricFrontiers $
         transitionFrontierTouching width maxFleet frontier fleet) .
   CountMap.toAscList

nextFrontierTouchingExternal ::
   (Nat w) => Size w -> Fleet.T -> CountMapPath w -> CountMap w -> IO ()
nextFrontierTouchingExternal width maxFleet path =
   CountMap.writeSorted path .
   map
      (concatMap
         (\((frontier,fleet), cnt) ->
            map (flip (,) cnt) $ mergeSymmetricFrontiers $
            transitionFrontierTouching width maxFleet frontier fleet)) .
   ListHT.sliceVertical bucketSize .
   CountMap.toAscList

transitionFrontierTouching ::
   (Nat w) =>
   Size w -> Fleet.T -> Frontier.T w -> Fleet.T -> [(Frontier.T w, Fleet.T)]
transitionFrontierTouching width maxFleet oldFrontier =
   let cumMaxFleet = Fleet.cumulate maxFleet
       finishVerticals pos =
          case Frontier.lookup oldFrontier pos of
             Frontier.Blocked ->
                error "in touching mode there must be no blocked fields"
             Frontier.Vertical n ->
                (guard (n < maxShipSize) >>
                 insertVertical cumMaxFleet (n+1) pos)
                <|>
                newShip cumMaxFleet maxFleet n
             Frontier.Free -> return ()

       startNewShips pos =
          when (not $ atEnd width pos) $ do
             frontier <- MS.gets fst
             case Frontier.lookup frontier pos of
                Frontier.Blocked ->
                   error "finishVerticals must not block fields"
                Frontier.Vertical _ ->
                   startNewShips (pos+1)
                Frontier.Free ->
                   startNewShips (pos+1)
                   <|>
                   (insertVertical cumMaxFleet 1 pos >> startNewShips (pos+1))
                   <|>
                   (asumTakeFrontier frontier pos width $
                    map
                       (\shipSize ->
                          newShip cumMaxFleet maxFleet shipSize >>
                          startNewShips (pos+shipSize)) $
                       [1 .. Fleet.maxSize])

   in  \fleet -> flip MS.execStateT (Frontier.empty, fleet) $ do
         mapM_ finishVerticals (widthRange width)
         startNewShips 0

countTouching :: (Nat w) => (Size w, Int) -> Fleet.T -> Count
countTouching (width,height) reqFleet =
   countBoundedFromMap reqFleet $
   nest height (nextFrontierTouching width reqFleet) $ baseCase width

canonicalFrontier :: (Nat w) => Frontier.T w -> Frontier.T w
canonicalFrontier fr = min fr (Frontier.reverse fr)

mergeSymmetricFrontiers ::
   (Nat w) => [(Frontier.T w, fleet)] -> [(Frontier.T w, fleet)]
mergeSymmetricFrontiers = map (mapFst canonicalFrontier)


fleetAtFrontier :: Frontier.T w -> Fleet.T
fleetAtFrontier =
   Frontier.foldMap
      (\use ->
         case use of
            Frontier.Vertical n -> Fleet.singleton n 1
            _ -> Fleet.empty)


addFrontierFleet :: Frontier.T w -> Fleet.T -> Fleet.T
addFrontierFleet frontier = mappend $ fleetAtFrontier frontier


-- * retrieve counts from count maps

{-# SPECIALISE countBoundedFromMap :: Fleet.T -> CountMap w -> Count #-}
countBoundedFromMap ::
   (Counter.C a, Storable a) => Fleet.T -> CountMap.T w a -> a
countBoundedFromMap reqFleet =
   Counter.sum .
   map snd .
   filter (\((front,fleet), _) ->
             addFrontierFleet front fleet == reqFleet) .
   CountMap.toAscList

countBoundedFleetsFromMap :: CountMap w -> Map Fleet.T Integer
countBoundedFleetsFromMap =
   Map.fromListWith (+) .
   map (\((front,fleet), cnt) ->
             (addFrontierFleet front fleet,
              Counter.toInteger cnt)) .
   CountMap.toAscList

{-
maybe this is not lazy enough and thus requires to much memory at once
-}
countBoundedFleetsFromMap_ :: CountMap w -> Map Fleet.T Integer
countBoundedFleetsFromMap_ =
   Map.mapKeysWith (+) (uncurry addFrontierFleet) .
   fmap Counter.toInteger .
   CountMap.toMap


{-
*ShortenShip> let height=3::Int; width=10::Int; reqFleet = Fleet.fromList [(2,3),(3,1)]
(0.01 secs, 524480 bytes)

*ShortenShip> let counts = nest height (nextFrontier width) $ baseCase width in (Map.size counts, Fold.sum counts, Fold.maximum counts)
(658486,37986080,16640)
(77.32 secs, 9147062872 bytes)

*ShortenShip> let counts = nest height (nextFrontierBounded width reqFleet) $ baseCase width in (Map.size counts, Fold.sum counts, Fold.maximum counts)
(59485,870317,2295)
(41.05 secs, 4961028184 bytes)

This was computed, where we marked horizontal ships
instead of blocked columns.

*ShortenShip> let width=10::Int; reqFleet = Fleet.german
*ShortenShip> map Map.size $ iterate (nextFrontierBounded width reqFleet) $ baseCase width
[1,976,9441,129247,727781,Interrupted.

Here we switched to blocked columns and thus could merge some cases.
*ShortenShip> map Map.size $ iterate (nextFrontierBounded width reqFleet) $ baseCase width
[1,762,8712,110276,671283,Heap exhausted

Now merge symmetric cases.
*ShortenShip> map Map.size $ iterate (nextFrontierBounded width reqFleet) $ baseCase width
[1,400,4209,53897,331185,Heap exhausted

Now correctly stop searching, when we exceed the requested fleet
in a cumulative way.
*ShortenShip> map Map.size $ iterate (nextFrontierBounded width reqFleet) $ baseCase width
[1,400,2780,33861,156962,596354,1078596,
-}


countSingleKind :: IO ()
countSingleKind =
   mapM_
      (print . countBounded (n10,10) . Fleet.fromList . (:[]))
      [(5,1), (4,2), (3,3), (2,4)]

{- | <http://math.stackexchange.com/questions/58769/how-many-ways-can-we-place-these-ships-on-this-board>
-}
count8x8 :: IO ()
count8x8 =
{-
   print $ countTouching (n8,8) Fleet.english
-}
   let reqFleet = Fleet.english
       width = n8
       height = 8
   in  reportCounts
         (baseCase width) (nextFrontierTouchingExternal width)
         height reqFleet

{-
0
0
0
24348
712180
8705828
50637316
193553688
571126760
-}

countTouchingExternalReturn ::
   Nat w => (Size w, Int) -> Fleet.T -> IO Count
countTouchingExternalReturn (width, height) =
   countExternalGen (baseCase width) (nextFrontierTouchingExternal width) height


{- |
http://mathoverflow.net/questions/8374/battleship-permutations
-}
count10x10 :: IO ()
count10x10 =
   print $ countBounded (n10,10) Fleet.english

{-
width = 10
reqFleet = Fleet.english

0 (height 0)
0
0
28
3216
665992
7459236
49267288
212572080
703662748
1925751392 (height 10)
4558265312
9655606528
-}


countStandard :: IO ()
countStandard =
   let -- reqFleet = Fleet.german
       reqFleet = Fleet.english
       -- reqFleet = Fleet.fromList [(5,3), (3,3), (2,4)]
       -- reqFleet = Fleet.fromList [(5,1), (4,5), (2,4)]
       -- reqFleet = Fleet.fromList [(5,1), (4,2), (3,7)]
       -- reqFleet = Fleet.fromList [(5,1), (4,2), (3,3)]
       width = n10
       height = 12
   in  mapM_ (print . countBoundedFromMap reqFleet) $
       take (height+1) $
       iterate (nextFrontierBounded width reqFleet) $
       baseCase width

{-
width = 8

0 (height 0)
0
0
0
0
0
0
0
0
41590204
7638426604 (height 10)
362492015926
7519320122520
-}

{-
width = 9

0 (height 0)
0
0
0
0
0
0
3436
41590204 (height 8)
14057667720
810429191552
19372254431062
259204457356150 (height 12)
-}

bucketSize :: Int
bucketSize = 2^(14::Int)

tmpPath :: Int -> CountMap.Path w a
tmpPath = CountMap.Path . printf "/tmp/battleship%02d"

writeTmpCountMap :: Int -> CountMap w -> IO ()
writeTmpCountMap = CountMap.writeFile . tmpPath

writeTmps :: IO ()
writeTmps =
   let width = n10
   in  zipWithM_ writeTmpCountMap [0 ..] $
       iterate (nextFrontierBounded width Fleet.german) $
       baseCase width


countExternalGen ::
   (Counter.C a, Storable a) =>
   CountMap w ->
   (Fleet.T -> CountMap.Path w a -> CountMap.T w a -> IO ()) ->
   Int -> Fleet.T -> IO a
countExternalGen base next height fleet = do
   writeTmpCountMap 0 base
   let pathPairs = ListHT.mapAdjacent (,) $ map tmpPath [0 .. height]
   forM_ pathPairs $ \(src,dst) ->
      next fleet dst =<< CountMap.readFile src
   fmap (countBoundedFromMap fleet) $ CountMap.readFile $ tmpPath height

countExternalReturn ::
   Nat w => (Size w, Int) -> Fleet.T -> IO Count
countExternalReturn (width, height) =
   countExternalGen (baseCase width) (nextFrontierBoundedExternal width) height

reportCounts ::
   (Counter.C a, Storable a, Show a) =>
   CountMap w ->
   (Fleet.T -> CountMap.Path w a -> CountMap.T w a -> IO ()) ->
   Int -> Fleet.T -> IO ()
reportCounts base next height fleet = do
   writeTmpCountMap 0 base
   let pathPairs = ListHT.mapAdjacent (,) $ map tmpPath [0 .. height]
   forM_ pathPairs $ \(src,dst) -> do
      next fleet dst =<< CountMap.readFile src
      print . countBoundedFromMap fleet =<< CountMap.readFile dst

countExternal :: IO ()
countExternal =
   let width = n10
       height = 10
   in  reportCounts
         (baseCase width) (nextFrontierBoundedExternal width)
         height Fleet.german

{-
width = 10

0 (height 6)
13662566
7638426604
810429191552
26509655816984 (height 10)
430299058359872
4354180199012068
31106813918568460
170879359784006832
764137344189527328 (height 15)
2898295265655126580
9610725684470910308
28507470306925125256
76991108526373642970
191979866440965078136 (height 20)
446937970915638578082
980266021942073496100
2040665261937921277448
4057034306861698428948
7742825845480094358032
14247628010376642047600
25372084886315737302592
43866177282362611934648
73835392689835032947938
121284466564264656560792 (height 30)
194834219987709759902080
306653595670979763499532
473656349424114922202508
719020031938684168649088
1074093940268573906015112
1580772674048252559547360
2294422842530289843193622
3287462379238476844672168
4653704875700525771264888
6513595388626319121164932 (height 40)
9020479350315319743053840
12368062564291338311417712
16799237841455675768629728
22616472670702007858720088
30193972466229549593717002
39991855436006321943166520
52572598016253033812617552
68620034159721482069184188
88961217595210753573463188
114591483536481178478783072 (height 50)
146703075254771226052685400
186717731484777645553381392
236323662853505427847380798
297517379449112891075247688
372650867327049668352192392
464484649227692889820652980
576247304097610697944846232
711702061209803706344169808
875221127811075401295007088
1071868454331343083880616712 (height 60)
1307491688314503052228073010
1588824117417688619931354072
1923597453119688551212343968
2320666360224207858208417388
2790145692883833943588101532
3343561455750300075076754240
3994016569020196440013951912
4756372578636947840355038528
5647448517773937744162740838
6686238193004392201202597352 (height 70)
7894147238305325350349872920
9295251352289772187727329252
10916577208857975219414387488
12788407608848430883790544688
14944612520298020488691084672
17423007737631130160152847800
20265742975534986235203786842
23519721301486677943493929848
27237051901920789404168888688
31475538270909499207841886492 (height 80)
36299204007013709451418121380
41778858503698518338457830432
47992704921433432642323359608
55026992935361409746132575088
62976718861265404915127762062
71946375874530859216057441160
82050757151941778367149983272
93415814884501004073824213268
106179578231076248372025054888
120493133407586729455583375632 (height 90)
136521669234705095698870234640
154445591598700034393838411112
174461710415130814393910088578
196784502823670288688670584088
221647456484421865215555733952
249304496991752923240266671820
280031503570936389699101953452
314127917375817976286354988288
351918446862353195080976223688
393754874873229999431436186272 (height 100)

real    80m24.600s
user    76m33.675s
sys     2m22.601s
-}

countFleets :: IO ()
countFleets =
   Fold.mapM_ putStrLn .
   Map.mapWithKey
      (\fleet cnt ->
         "|-\n| " ++
         intercalate " || "
            (map ((\n -> if n==0 then " " else show n) . Fleet.lookup fleet) [2..5]
              ++ [show cnt])) .
   Map.filterWithKey (\fleet _cnt -> Fleet.subset fleet Fleet.german) .
   countBoundedFleetsFromMap =<<
   CountMap.readFile (tmpPath 10)


printMapSizes :: IO ()
printMapSizes =
   mapM_ (print . CountMap.size) $
   iterate (nextFrontierBounded n10 Fleet.german) $
   baseCase n10



genShip :: QC.Gen Fleet.ShipSize
genShip = QC.choose (1, maxShipSize)

genFleet :: QC.Gen Fleet.T
genFleet = fmap Fleet.fromSizes $ flip QC.vectorOf genShip =<< QC.choose (0,4)

propCountSymmetry :: QC.Property
propCountSymmetry =
   QC.forAllShrink genFleet QC.shrink $ \fleet ->
      let d =
            {-
            A single square is moved by any rotation or reflection.
            Two squares can have one symmetry.
            -}
            case mod (sum $ map (uncurry (*)) $ Fleet.toList fleet) 4 of
               0 -> 1
               2 -> 2
               _ ->
                 {-
                 If there is an odd sized ship without a partner
                 then there are no symmetries.
                 -}
                 if any odd $ map (uncurry (*)) $ Fleet.toList fleet
                   then 8
                   else 4
      in  mod (Counter.toInteger $ countBounded (n6,6) fleet) d == 0

propCountTransposed :: QC.Property
propCountTransposed =
   QC.forAllShrink (QC.choose (0,4)) QC.shrink $ \width ->
   QC.forAllShrink (QC.choose (0,8)) QC.shrink $ \height ->
   QC.forAllShrink genFleet QC.shrink $ \fleet ->
   Size.reifyInt width $ \w ->
   Size.reifyInt height $ \h ->
      countBounded (w,height) fleet == countBounded (h,width) fleet

propCountBounded :: QC.Property
propCountBounded =
   QC.forAllShrink (QC.choose (0,4)) QC.shrink $ \width ->
   QC.forAllShrink (QC.choose (0,10)) QC.shrink $ \height ->
   QC.forAllShrink genFleet QC.shrink $ \fleet ->
   Size.reifyInt width $ \w ->
      count (w,height) fleet == countBounded (w,height) fleet

propCountTouchingTransposed :: QC.Property
propCountTouchingTransposed =
   QC.forAllShrink (QC.choose (0,4)) QC.shrink $ \width ->
   QC.forAllShrink (QC.choose (0,6)) QC.shrink $ \height ->
   Size.reifyInt width $ \w ->
   Size.reifyInt height $ \h ->
   QC.forAllShrink genFleet QC.shrink $ \fleet ->
      countTouching (w,height) fleet == countTouching (h,width) fleet

propCountMoreTouching :: QC.Property
propCountMoreTouching =
   QC.forAllShrink (QC.choose (0,6)) QC.shrink $ \width ->
   QC.forAllShrink (QC.choose (0,10)) QC.shrink $ \height ->
   QC.forAllShrink genFleet QC.shrink $ \fleet ->
   Size.reifyInt width $ \w ->
      countBounded (w,height) fleet <= countTouching (w,height) fleet


propCountExternal :: QC.Property
propCountExternal =
   QC.forAllShrink (QC.choose (0,4)) QC.shrink $ \width ->
   QC.forAllShrink (QC.choose (0,10)) QC.shrink $ \height ->
   QC.forAllShrink genFleet QC.shrink $ \fleet ->
   Size.reifyInt width $ \w -> QCM.monadicIO $ do
      c <- QCM.run $ countExternalReturn (w,height) fleet
      QCM.assert $ count (w,height) fleet == c

propCountTouchingExternal :: QC.Property
propCountTouchingExternal =
   QC.forAllShrink (QC.choose (0,4)) QC.shrink $ \width ->
   QC.forAllShrink (QC.choose (0,10)) QC.shrink $ \height ->
   QC.forAllShrink genFleet QC.shrink $ \fleet ->
   Size.reifyInt width $ \w -> QCM.monadicIO $ do
      c <- QCM.run $ countTouchingExternalReturn (w,height) fleet
      QCM.assert $ countTouching (w,height) fleet == c
