module Combinatorics.Battleship.Count.ShortenShip.Distribution where

import qualified Combinatorics.Battleship.Count.ShortenShip as ShortenShip
import qualified Combinatorics.Battleship.Count.CountMap as CountMap
import qualified Combinatorics.Battleship.Count.Counter as Counter
import qualified Combinatorics.Battleship.Count.Frontier as Frontier
import qualified Combinatorics.Battleship.Fleet as Fleet
import qualified Combinatorics.Battleship.Size as Size
import Combinatorics.Battleship.Size (Nat, Zero, Succ, N10, Size(Size), size)
import Combinatorics.Battleship.Count.ShortenShip (countBoundedFromMap)

import Foreign.Storable (Storable, sizeOf, alignment, poke, peek)
import Foreign.Ptr (Ptr, castPtr)

import Control.Monad.HT (void)
import Control.Monad (when)
import Control.Applicative ((<$>))
import Control.DeepSeq (NFData, rnf, ($!!))

import qualified Data.StorableVector as SV
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Tuple.HT (mapFst)
import Data.Word (Word64)

import qualified Test.QuickCheck.Monadic as QCM
import qualified Test.QuickCheck as QC


{- |
We need to encode the height in the type
since the Storable instance requires that the size of the binary data
can be infered from the Distribution type.
-}
newtype Distr w h a = Distr {getDistr :: SV.Vector a}

instance (Storable a) => NFData (Distr w h a) where
   rnf = rnf . getDistr

countFromDistr :: (Storable a) => Distr w h a -> a
countFromDistr = SV.head . getDistr

rowsFromDistr :: (Storable a) => Size w -> Distr w h a -> [Row w a]
rowsFromDistr (Size width) =
   map Row . SV.sliceVertical width . SV.tail . getDistr

heightType :: Size h -> Distr w h a -> Distr w h a
heightType _ = id


newtype Size2 w h = Size2 Int

size2FromSizes :: Size w -> Size h -> Size2 w h
size2FromSizes (Size width) (Size height) = Size2 (1 + width*height)

size2 :: (Nat w, Nat h) => Size2 w h
size2 = size2FromSizes size size


instance (Nat w, Nat h, Storable a) => Storable (Distr w h a) where
   sizeOf = sizeOfWithSize size2
   alignment (Distr xs) = alignment (SV.head xs)
   poke ptr (Distr xs) = SV.poke (castPtr ptr) xs
   peek = peekWithSize size2

-- not correct if padding is needed
sizeOfWithSize :: (Storable a) => Size2 w h -> Distr w h a -> Int
sizeOfWithSize (Size2 n) (Distr xs) = n * sizeOf (SV.head xs)

peekWithSize ::
   (Storable a) => Size2 w h -> Ptr (Distr w h a) -> IO (Distr w h a)
peekWithSize (Size2 n) ptr = fmap Distr $ SV.peek n (castPtr ptr)

instance
      (Nat w, Nat h, Counter.C a, Storable a) => Counter.C (Distr w h a) where
   zero = constant size2 Counter.zero
   one = constant size2 Counter.one
   add (Distr x) (Distr y) = Distr $ SV.zipWith Counter.add x y

constant :: (Storable a) => Size2 w h -> a -> Distr w h a
constant (Size2 n) = Distr . SV.replicate n


newtype Row w a = Row {getRow :: SV.Vector a}

avg :: (Integral a) => a -> a -> a
avg x y =
   case divMod (x+y) 2 of
      (z,0) -> z
      _ -> error "avg: odd sum"

symmetric :: (Integral a, Storable a) => Row w a -> Row w a
symmetric (Row xs) = Row $ SV.zipWith avg xs (SV.reverse xs)


type Count = Word64
type CountMap = CountMap.T Count

{-# SPECIALISE
   CountMap.mergeMany :: [CountDistrMap N10 Zero] -> CountDistrMap N10 Zero
  #-}

type CountDistr w h = Distr w h Count
type CountDistrMap w h = CountMap.T w (CountDistr w h)
type CountDistrPath w h = CountMap.Path w (CountDistr w h)


rowFromFrontier :: (Nat w) => Size w -> Count -> Frontier.T w -> Row w Count
rowFromFrontier width cnt =
   Row .
   Frontier.mapToVector width (\x -> if x == Frontier.Free then 0 else cnt)

addRowToDistr :: Row w Count -> CountDistr w h -> CountDistr w (Succ h)
addRowToDistr (Row row) (Distr xs) =
   Distr $ SV.concat [SV.take 1 xs, row, SV.tail xs]

addFrontierToDistr ::
   (Nat w) => Frontier.T w -> CountDistr w h -> CountDistr w (Succ h)
addFrontierToDistr frontier cntDistr =
   addRowToDistr (rowFromFrontier size (countFromDistr cntDistr) frontier) cntDistr


baseCase :: (Nat w) => CountDistrMap w Zero
baseCase = CountMap.singleton (Frontier.empty, Fleet.empty) Counter.one

nextFrontierBoundedExternal ::
   (Nat w, Nat h) =>
   Size w -> Fleet.T -> CountDistrPath w (Succ h) -> CountDistrMap w h -> IO ()
nextFrontierBoundedExternal width maxFleet dst =
   CountMap.writeSorted dst .
   map
      (concatMap
         (\((frontier,fleet), cntDistr) ->
            map (\key ->
                  (mapFst
                     (ShortenShip.canonicalFrontier . Frontier.dilate width)
                     key,
                   addFrontierToDistr (fst key) cntDistr)) $
            ShortenShip.transitionFrontierBounded
               width maxFleet frontier fleet)) .
   ListHT.sliceVertical bucketSize .
   CountMap.toAscList

bucketSize :: Int
bucketSize = 2^(11::Int)

tmpPath :: Size h -> CountDistrPath w h
tmpPath (Size height) = ShortenShip.tmpPath height

reportCount :: (Nat w, Nat h) => Fleet.T -> CountDistrPath w h -> IO ()
reportCount fleet path = do
   putStrLn ""
   cd <- countBoundedFromMap fleet <$> CountMap.readFile path
   print $ countFromDistr cd
   putStr $ unlines $
      map (unwords . map show . SV.unpack . getRow . symmetric) $
      rowsFromDistr size cd

withReport ::
   (Nat w, Nat h) =>
   Bool -> Fleet.T -> (CountDistrPath w h -> IO ()) -> IOCountDistrPath w h
withReport report fleet act =
   IOCountDistrPath $
   case tmpPath size of
      path -> do
         act path
         when report $ reportCount fleet path
         return path

newtype
   IOCountDistrPath w h =
      IOCountDistrPath {runIOCountDistrPath :: IO (CountDistrPath w h)}

distributionBoundedExternal ::
   (Nat w, Nat h) => Bool -> Fleet.T -> IO (CountDistrPath w h)
distributionBoundedExternal report fleet =
   runIOCountDistrPath $
   Size.switch
      (withReport report fleet $ \path ->
         CountMap.writeFile path baseCase)
      (withReport report fleet $ \path ->
         nextFrontierBoundedExternal size fleet path
            =<< CountMap.readFile
            =<< distributionBoundedExternal report fleet)


countExternal :: IO ()
countExternal =
   void (distributionBoundedExternal True Fleet.german :: IO (CountDistrPath N10 N10))



distributionExternalList ::
   (Nat w, Nat h) => Size w -> Size h -> Fleet.T -> IO (Count, [[Count]])
distributionExternalList w h fleet = do
   cdm <-
      (return $!!) . countBoundedFromMap fleet =<<
      CountMap.readFile =<< distributionBoundedExternal False fleet
   return
      (countFromDistr cdm,
       map (SV.unpack . getRow . symmetric) $
       rowsFromDistr w $ heightType h cdm)

propCountExternalTotal :: QC.Property
propCountExternalTotal =
   QC.forAllShrink (QC.choose (0,6)) QC.shrink $ \width ->
   QC.forAllShrink (QC.choose (0,10)) QC.shrink $ \height ->
   QC.forAllShrink ShortenShip.genFleet QC.shrink $ \fleet ->
   Size.reifyInt width $ \w ->
   Size.reifyInt height $ \h -> QCM.monadicIO $ do
      (c,cd) <- QCM.run $ distributionExternalList w h fleet
      QCM.assert $
         Counter.toInteger c
          * fromIntegral (sum $ map (uncurry (*)) $ Fleet.toList fleet)
         ==
         (sum $ map (Counter.toInteger . Counter.sum) cd)

propCountExternalSimple :: QC.Property
propCountExternalSimple =
   QC.forAllShrink (QC.choose (0,6)) QC.shrink $ \width ->
   QC.forAllShrink (QC.choose (0,10)) QC.shrink $ \height ->
   QC.forAllShrink ShortenShip.genFleet QC.shrink $ \fleet ->
   Size.reifyInt width $ \w ->
   Size.reifyInt height $ \h -> QCM.monadicIO $ do
      (c,_cd) <- QCM.run $ distributionExternalList w h fleet
      ce <- QCM.run $ ShortenShip.countExternalReturn (w,height) fleet
      QCM.assert $ Counter.toInteger ce == Counter.toInteger c

propCountExternalSymmetric :: QC.Property
propCountExternalSymmetric =
   QC.forAllShrink (QC.choose (0,6)) QC.shrink $ \sz ->
   QC.forAllShrink ShortenShip.genFleet QC.shrink $ \fleet ->
   Size.reifyInt sz $ \n -> QCM.monadicIO $ do
      (_c,cd) <- QCM.run $ distributionExternalList n n fleet
      QCM.assert $ cd == List.transpose cd

propCountExternalTransposed :: QC.Property
propCountExternalTransposed =
   QC.forAllShrink (QC.choose (0,6)) QC.shrink $ \width ->
   QC.forAllShrink (QC.choose (0,6)) QC.shrink $ \height ->
   QC.forAllShrink ShortenShip.genFleet QC.shrink $ \fleet ->
   Size.reifyInt width $ \w ->
   Size.reifyInt height $ \h -> QCM.monadicIO $ do
      (c0,cd0) <- QCM.run $ distributionExternalList w h fleet
      (c1,cd1) <- QCM.run $ distributionExternalList h w fleet
      QCM.assert $ c0 == c1
      QCM.assert $ List.transpose cd0 == cd1
