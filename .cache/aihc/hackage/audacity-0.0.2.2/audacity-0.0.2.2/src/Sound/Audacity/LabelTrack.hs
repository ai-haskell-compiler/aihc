module Sound.Audacity.LabelTrack where

import Text.Read.HT (maybeRead)
import Text.Printf (printf)

import Control.DeepSeq (NFData, rnf)
import Control.Monad (zipWithM)

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.Monoid as Mn
import qualified Data.Semigroup as Sg
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.HT as ListHT
import Data.Tuple.HT (mapFst, mapSnd, mapPair)

import qualified Prelude as P
import Prelude hiding (readFile, writeFile, null)


newtype T time label = Cons {decons :: [Interval time label]}

instance (Show time, Show label) => Show (T time label) where
   showsPrec p (Cons xs) =
      showParen (p>10) $ showString "LabelTrack.Cons " . shows xs


type Interval time label = ((time, time), label)


instance Functor (T time) where
   fmap f = lift $ map (mapSnd f)

instance Fold.Foldable (T time) where
   foldMap f = Fold.foldMap (f . snd) . decons

instance Trav.Traversable (T time) where
   sequenceA =
      fmap Cons . Trav.traverse (\(bnd, label) -> fmap ((,) bnd) label) . decons

instance Sg.Semigroup (T time label) where
   Cons xs <> Cons ys = Cons $ xs ++ ys
   sconcat = Cons . concatMap decons . NonEmpty.toList

instance Mn.Monoid (T time label) where
   mempty = empty
   mappend (Cons xs) (Cons ys) = Cons $ xs ++ ys
   mconcat = Cons . concatMap decons

instance (NFData time, NFData label) => NFData (T time label) where
   rnf = rnf . decons


empty :: T time label
empty = Cons []

null :: T time label -> Bool
null = P.null . decons

singleton :: (time,time) -> label -> T time label
singleton bnds label = Cons [(bnds, label)]


fromAdjacentChunks ::
   (Num time) => [(time, label)] -> T time label
fromAdjacentChunks =
   Cons . snd .
   Trav.mapAccumL (\t0 (d, lab) -> let t1=t0+d in (t1, ((t0,t1), lab))) 0


lift ::
   ([Interval time0 label0] -> [Interval time1 label1]) ->
   T time0 label0 -> T time1 label1
lift f (Cons xs) = Cons $ f xs

lift2 ::
   ([Interval time0 label0] -> [Interval time1 label1] -> [Interval time2 label2]) ->
   T time0 label0 -> T time1 label1 -> T time2 label2
lift2 f (Cons xs) (Cons ys) = Cons $ f xs ys

{- |
Format the times using a comma,
which is certainly only correct in German locale.
-}
{-
ToDo: find out, how Audacity formats the labels.
In the project XML file format, the numbers are formatted with decimal points.
-}
formatTime :: (RealFrac time) => time -> String
formatTime t =
   let million = 10^(6::Int)
       (seconds,micros) = divMod (round (t * fromInteger million)) million
   in  printf "%d,%06d" seconds micros

{- |
You must make sure, that the time mapping function preserves the order.
This is not checked.
-}
mapTime :: (time0 -> time1) -> T time0 label -> T time1 label
mapTime f  =  lift $ map (mapFst $ mapPair (f, f))

mapWithTime ::
   ((time, time) -> label0 -> label1) -> T time label0 -> T time label1
mapWithTime f  =  lift $ map (\(bnd, lab) -> (bnd, f bnd lab))

realTimes ::
   (Fractional time) =>
   time -> T Int label -> T time label
realTimes sampleRate =
   mapTime (\t -> fromIntegral t / sampleRate)

mask :: (Ord time) => (time, time) -> T time label -> T time label
mask (from,to) =
   lift $
      filter (uncurry (<) . fst) .
      map (mapFst (mapPair (max from, min to)))


zipWithList ::
   (label0 -> label1 -> label2) -> [label0] -> T time label1 -> T time label2
zipWithList f xs = lift $ zipWith (\x (bnd, y) -> (bnd, f x y)) xs


writeFile :: (RealFrac time) => FilePath -> T time String -> IO ()
writeFile path intervals =
   P.writeFile path $ unlines $
   flip map (decons $ mapTime formatTime intervals) $ \((from,to),label) ->
      printf "%s\t%s\t%s" from to label

writeFileInt ::
   (RealFrac time) =>
   time -> FilePath -> T Int String -> IO ()
writeFileInt sampleRate path =
   writeFile path . realTimes sampleRate


parseTime :: (Fractional time) => String -> Maybe time
parseTime str =
   case break (','==) str of
      (intStr, ',':fracStr) -> do
         int <- maybeRead intStr
         frac <- maybeRead fracStr
         return $
            fromInteger int +
            fromInteger frac / fromInteger (10 ^ length fracStr)
      (intStr, []) -> fmap fromInteger $ maybeRead intStr
      (_, _:_) -> error "break seems to match other characters than comma"

{- |
Read label file in a strict way.
-}
readFile :: (Fractional time) => FilePath -> IO (T time String)
readFile name =
   let parseTimeIO n str =
          case parseTime str of
             Just t -> return t
             Nothing ->
                ioError $ userError $
                printf "%s:%d: \"%s\" is not a number" name n str
       parseLine n ln =
          case ListHT.chop ('\t'==) ln of
             [fromStr, toStr, label] -> do
                from <- parseTimeIO n fromStr
                to <- parseTimeIO n toStr
                return ((from, to), label)
             fields ->
                ioError $ userError $
                printf "%s:%d: expected 3 fields, but got %d"
                   name n (length fields)
   in  fmap Cons $ zipWithM parseLine [1::Int ..] . lines =<< P.readFile name
