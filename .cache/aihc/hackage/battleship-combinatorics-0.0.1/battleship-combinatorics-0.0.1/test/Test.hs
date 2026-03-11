module Main where

import qualified Combinatorics.Battleship.Count.ShortenShip.Distribution as
                                                                   Distribution
import qualified Combinatorics.Battleship.Count.ShortenShip as ShortenShip
import qualified Combinatorics.Battleship.Count.CountMap as CountMap
import qualified Combinatorics.Battleship.Count.Counter as Counter
import qualified Combinatorics.Battleship.Count.Frontier as Frontier
import qualified Combinatorics.Battleship.Enumeration as Enumerate
import qualified Combinatorics.Battleship.SetCover as SetCover
import qualified Combinatorics.Battleship.Fleet as Fleet
import qualified Combinatorics.Battleship.Size as Size


import qualified Test.QuickCheck.Monadic as QCM
import qualified Test.QuickCheck as QC
import Test.QuickCheck (quickCheck)


factorial :: (Integral a) => a -> a
factorial n = product [1..n]

multiplicity :: (Integral a) => Fleet.T -> a
multiplicity = product . map (factorial . fromIntegral . snd) . Fleet.toList

propCountSetCover :: QC.Property
propCountSetCover =
   QC.forAllShrink (QC.choose (0,4)) QC.shrink $ \width ->
   QC.forAllShrink (QC.choose (0,10)) QC.shrink $ \height ->
   QC.forAllShrink ShortenShip.genFleet QC.shrink $ \fleet ->
   Size.reifyInt width $ \w ->
      Counter.toInteger (ShortenShip.countBounded (w, height) fleet)
       * multiplicity fleet
      ==
      fromIntegral
         (length $ SetCover.enumerateGen id (width,height) $ Fleet.toList fleet)

propDistributionSetCover :: QC.Property
propDistributionSetCover =
   QC.forAll (QC.choose (1,4)) $ \width ->
   QC.forAll (QC.choose (1,10)) $ \height ->
   QC.forAllShrink ShortenShip.genFleet QC.shrink $ \fleet ->
   Size.reifyInt width $ \w ->
   Size.reifyInt height $ \h -> QCM.monadicIO $ do
      (_c,cd) <- QCM.run $ Distribution.distributionExternalList w h fleet
      let boardSize = (width,height)
          board =
            SetCover.sumMapsStorable boardSize $
            map (fmap (\b -> if b then 1 else 0)) $
            SetCover.enumerateGen id boardSize $ Fleet.toList fleet
      QCM.assert $
         map (map (multiplicity fleet *)) cd
         ==
         SetCover.listsFromBoard id boardSize board

propCountEnumerate :: QC.Property
propCountEnumerate =
   QC.forAllShrink (QC.choose (0,4)) QC.shrink $ \width ->
   QC.forAllShrink (QC.choose (0,10)) QC.shrink $ \height ->
   QC.forAllShrink ShortenShip.genFleet QC.shrink $ \fleet ->
   Size.reifyInt width $ \w ->
      Counter.toInteger (ShortenShip.countBounded (w, height) fleet)
      ==
      (fromIntegral $ length $
         Enumerate.configurations (width, height) $
         Enumerate.fleetFromSizes $ Fleet.toSizes fleet)

quickCheckNum :: QC.Testable prop => Int -> prop -> IO ()
quickCheckNum n = QC.quickCheckWith (QC.stdArgs {QC.maxSuccess = n})

main :: IO ()
main =
   mapM_ (\(msg,io) -> putStr (msg++": ") >> io) $
   ("Counter.add",        quickCheck Counter.propAdd) :
   ("Frontier.dilate",    quickCheckNum 1000 Frontier.propDilate) :
   ("Frontier.reverse4",  quickCheck Frontier.propReverse4) :
   ("Frontier.reverse5",  quickCheck Frontier.propReverse5) :
   ("Frontier.reverse6",  quickCheck Frontier.propReverse6) :
   ("Frontier.reverse7",  quickCheck Frontier.propReverse7) :
   ("Frontier.reverse8",  quickCheck Frontier.propReverse8) :
   ("Frontier.reverse9",  quickCheck Frontier.propReverse9) :
   ("Frontier.reverse10", quickCheck Frontier.propReverse10) :
   ("Fleet.list",         quickCheck Fleet.propList) :
   ("Fleet.sizes",        quickCheck Fleet.propSizes) :
   ("Fleet.cumulate",     quickCheck Fleet.propCumulate) :
   ("Fleet.subset",       quickCheck Fleet.propSubset) :
   ("Fleet.inc",          quickCheck Fleet.propInc) :
   ("Fleet.dec",          quickCheck Fleet.propDec) :
   ("Fleet.incDec",       quickCheck Fleet.propIncDec) :
   ("CountMap.merge",     quickCheck CountMap.propMerge) :
   ("ShortenShip.countSymmetry",
                          quickCheck ShortenShip.propCountSymmetry) :
   ("ShortenShip.countTransposed",
                          quickCheck ShortenShip.propCountTransposed) :
   ("ShortenShip.countTouchingTransposed",
                          quickCheck ShortenShip.propCountTouchingTransposed) :
   ("ShortenShip.countMoreTouching",
                          quickCheck ShortenShip.propCountMoreTouching) :
   ("ShortenShip.countBounded",
                          quickCheck ShortenShip.propCountBounded) :
   ("ShortenShip.countExternal",
                          quickCheck ShortenShip.propCountExternal) :
   ("ShortenShip.countTouchingExternal",
                          quickCheck ShortenShip.propCountTouchingExternal) :
   ("countSetCover",      quickCheck propCountSetCover) :
   ("countEnumerate",     quickCheck propCountEnumerate) :
   ("distributionSetCover",
                          quickCheck propDistributionSetCover) :
   ("Distribution.countExternalTotal",
                          quickCheck Distribution.propCountExternalTotal) :
   ("Distribution.countExternalSimple",
                          quickCheck Distribution.propCountExternalSimple) :
   ("Distribution.countExternalSymmetric",
                          quickCheck Distribution.propCountExternalSymmetric) :
   ("Distribution.countExternalTransposed",
                          quickCheck Distribution.propCountExternalTransposed) :
   []
