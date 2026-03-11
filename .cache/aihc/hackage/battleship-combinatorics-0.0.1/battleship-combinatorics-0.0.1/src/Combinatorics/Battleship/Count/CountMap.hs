module Combinatorics.Battleship.Count.CountMap (
   T,
   KeyCount,

   Path(Path),
   readFile,
   writeFile,

   fromList,
   fromListStorable,
   fromListExternal,
   writeSorted,
   fromMap,
   singleton,
   size,
   toAscList,
   toMap,

   mergeMany,

   propMerge,
   ) where

import qualified Combinatorics.Battleship.Count.Frontier as Frontier
import qualified Combinatorics.Battleship.Count.Counter as Counter
import qualified Combinatorics.Battleship.Fleet as Fleet
import Combinatorics.Battleship.Count.Counter (add)
import Combinatorics.Battleship.Size (Nat, N10, )

import qualified System.IO.Temp as Temp
import System.Directory (removeFile, )
import System.FilePath ((</>), )

import qualified Data.StorableVector.Lazy.Pointer as SVP
import qualified Data.StorableVector.Lazy as SVL

import Data.Map (Map, )
import qualified Data.Map as Map

import qualified Control.Concurrent.PooledIO.Independent as Pool
import Control.DeepSeq (NFData, rnf, )
import Control.Monad (liftM2, zipWithM_, foldM, forM_, )
import Control.Applicative ((<$>), )
import Control.Functor.HT (void, )

import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.Match as Match
import Data.Monoid (Monoid, mempty, mappend, mconcat, )
import Data.Semigroup (Semigroup, (<>))
import Data.List.HT (sliceVertical, )
import Text.Printf (printf, )

import Data.Word (Word64, )

import Foreign.Storable
          (Storable, sizeOf, alignment,
           poke, peek, pokeByteOff, peekByteOff, )

import Prelude hiding (readFile, writeFile, )


type Count64 = Word64
type Count128 = Counter.Composed Word64 Word64

{- |
Represents a @Map Key Count@
by a lazy ByteString containing the (key,count) pairs in ascending order.
-}
newtype T w a = Cons (SVL.Vector (Element w a))
   deriving (Eq)

instance (Nat w, Show a, Storable a) => Show (T w a) where
   showsPrec prec (Cons x) =
      showParen (prec>10) $
         showString "CountMap.fromAscList " .
         shows (SVL.unpack x)

instance (Storable a) => NFData (T w a) where
   rnf (Cons x) = rnf x


data Element w a =
   Element {
      _elementKey :: Key w,
      _elementCount :: a
   } deriving (Eq, Show)

type Key w = (Frontier.T w, Fleet.T)
type KeyCount w a = (Key w, a)

instance (Storable a) => Storable (Element w a) where
   sizeOf ~(Element ~(front, fleet) cnt) =
      sizeOf front + sizeOf fleet + sizeOf cnt
   alignment ~(Element ~(front, fleet) cnt) =
      alignment front `lcm` alignment fleet `lcm` alignment cnt
   poke ptr (Element (front, fleet) cnt) = do
      pokeByteOff ptr 0 front
      pokeByteOff ptr (sizeOf front) fleet
      pokeByteOff ptr (sizeOf front + sizeOf fleet) cnt
   peek ptr = do
      front <- peekByteOff ptr 0
      fleet <- peekByteOff ptr (sizeOf front)
      cnt   <- peekByteOff ptr (sizeOf front + sizeOf fleet)
      return (Element (front, fleet) cnt)


defaultChunkSize :: SVL.ChunkSize
defaultChunkSize = SVL.chunkSize 512

fromAscList :: (Storable a) => [KeyCount w a] -> T w a
fromAscList =
   Cons . SVL.pack defaultChunkSize . map (uncurry Element)

fromMap :: (Storable a) => Map (Key w) a -> T w a
fromMap = fromAscList . Map.toAscList

fromList :: (Counter.C a, Storable a) => [KeyCount w a] -> T w a
fromList = fromMap . Map.fromListWith add

fromListStorable :: (Counter.C a, Storable a) => [KeyCount w a] -> T w a
fromListStorable = mconcat . map (uncurry singleton)


toAscList :: (Storable a) => T w a -> [KeyCount w a]
toAscList (Cons m) = map pairFromElement $ SVL.unpack m

toMap :: (Storable a) => T w a -> Map (Key w) a
toMap = Map.fromAscList . toAscList


singleton :: (Storable a) => Key w -> a -> T w a
singleton key cnt = Cons $ SVL.singleton $ Element key cnt

pairFromElement :: Element w a -> KeyCount w a
pairFromElement (Element key cnt) = (key, cnt)


size :: T w a -> Int
size (Cons x) = SVL.length x


newtype Path w a = Path {getPath :: FilePath}

writeFile :: (Storable a) => Path w a -> T w a -> IO ()
writeFile (Path path) (Cons xs) = SVL.writeFile path xs

{- |
It silently drops IO exceptions
and does not check whether the loaded data is valid.
-}
readFile :: (Storable a) => Path w a -> IO (T w a)
readFile (Path path) =
   Cons . snd <$> SVL.readFileAsync defaultChunkSize path

formatPath :: FilePath -> Int -> Path w a
formatPath dir = Path . (dir </>) . printf "extsort%04d"

{- |
It deletes the input files after the merge.
This saves a lot of disk space when running 'fromListExternal'.
-}
mergeFiles ::
   (Counter.C a, Storable a) => Path w a -> Path w a -> Path w a -> IO ()
mergeFiles input0 input1 output = do
   writeFile output =<< liftM2 merge (readFile input0) (readFile input1)
   removeFile $ getPath input0
   removeFile $ getPath input1

sequenceLast :: (Monad m) => a -> [m a] -> m a
sequenceLast deflt = foldM (\_ act -> act) deflt

{- |
Create a @CountMap@ from a large list of elements.
Neither the argument nor the result needs to fit in memory.
You only have to provide enough space on disk.
The result is lazily read from a temporary file.
That is, this file should neither be modified
nor deleted while processing the result.
Even more, 'fromListExternal' must not be called again
while processing the result.
You may better choose 'writeSorted'.
-}
fromListExternal ::
   (Counter.C a, Storable a) => Int -> [KeyCount w a] -> IO (T w a)
fromListExternal bucketSize xs = do
   let dir = "/tmp"
   lastN <-
      sequenceLast (-1) $
      zipWith
         (\n bucket -> writeFile (formatPath dir n) bucket >> return n)
         [0 ..] $
      map fromList $
      sliceVertical bucketSize xs
   case formatPath dir (2*lastN) of
      finalPath -> do
         forM_ (take lastN $ zip (iterate (2+) 0) [lastN+1 ..]) $
            \(srcN, dstN) ->
               mergeFiles
                  (formatPath dir srcN)
                  (formatPath dir (srcN+1))
                  (formatPath dir dstN `asTypeOf` finalPath)
         readFile finalPath

pairs :: [a] -> [(a,a)]
pairs (x0:x1:xs) = (x0,x1) : pairs xs
pairs (_:_) = []
pairs [] = error "pairs: even number of elements"

{-
The final external sort is bound by disk access time,
thus we only sort the buckets individually in parallel.
-}
writeSorted ::
   (Counter.C a, Storable a) => Path w a -> [[KeyCount w a]] -> IO ()
writeSorted dst xs =
   Temp.withSystemTempDirectory "battleship" $ \dir -> do
      let chunks = map fromList xs
      let unary = void chunks
      let paths =
            {-
            Matching with () makes sure
            that references from 'unary' to 'chunks' are removed
            as chunks are written to disk.
            They can then be reclaimed by the garbage collector.
            -}
            zipWith (\() -> formatPath dir) (init $ init $ unary ++ unary) [0..]
            ++
            [dst]
      Pool.run $ zipWith writeFile paths chunks
      zipWithM_ (uncurry mergeFiles) (pairs paths) (Match.drop unary paths)


empty :: (Storable a) => T w a
empty = Cons SVL.empty

merge :: (Counter.C a, Storable a) => T w a -> T w a -> T w a
merge (Cons xs0) (Cons ys0) =
   Cons $
   SVL.unfoldr defaultChunkSize
      (\(xt,yt) ->
         case (SVP.viewL xt, SVP.viewL yt) of
            (Nothing, Nothing) -> Nothing
            (Just (x,xs), Nothing) -> Just (x, (xs,yt))
            (Nothing, Just (y,ys)) -> Just (y, (xt,ys))
            (Just (Element xkey xcnt, xs),
             Just (Element ykey ycnt, ys)) -> Just $
               case compare xkey ykey of
                  EQ -> (Element xkey (add xcnt ycnt), (xs,ys))
                  LT -> (Element xkey xcnt, (xs,yt))
                  GT -> (Element ykey ycnt, (xt,ys)))
      (SVP.cons xs0, SVP.cons ys0)

propMerge :: [KeyCount N10 Count64] -> [KeyCount N10 Count64] -> Bool
propMerge xs ys =
   let xm = Map.fromListWith add xs
       ym = Map.fromListWith add ys
   in  merge (fromMap xm) (fromMap ym)
       ==
       fromMap (Map.unionWith add xm ym)


{-# SPECIALISE mergeMany :: [T w Count64] -> T w Count64 #-}
{-# SPECIALISE mergeMany :: [T w Count128] -> T w Count128 #-}
{-# INLINEABLE mergeMany #-}
mergeMany :: (Counter.C a, Storable a) => [T w a] -> T w a
mergeMany = maybe empty (NonEmpty.foldBalanced merge) . NonEmpty.fetch

instance (Counter.C a, Storable a) => Semigroup (T w a) where
   (<>) = merge

instance (Counter.C a, Storable a) => Monoid (T w a) where
   mempty = empty
   mappend = merge
   mconcat = mergeMany
