{- |
Like "Data.StorableVector.Lazy"
but the maximum chunk size is encoded in a type parameter.
This way, you do not need to pass a chunk size parameter at various places.
The API becomes more like the one for lists and 'ByteString's.
-}
module Data.StorableVector.Lazy.Typed (
   Vector,
   DefaultVector,
   Size,
   ChunkSize,
   chunkSize,
   lazyChunkSize,
   DefaultChunkSize,
   Size1024,
   empty,
   singleton,
   toVectorLazy,
   fromVectorLazy,
   chunks,
   fromChunks,
   pack,
   unpack,
   unfoldr,
   unfoldrResult,
   sample,
   sampleN,
   iterate,
   repeat,
   cycle,
   replicate,
   null,
   length,
   equal,
   index,
   cons,
   append,
   extendL,
   concat,
   sliceVertical,
   snoc,
   map,
   reverse,
   foldl,
   foldl',
   foldr,
   foldMap,
   any,
   all,
   maximum,
   minimum,
   viewL,
   viewR,
   switchL,
   switchR,
   scanl,
   mapAccumL,
   mapAccumR,
   crochetL,
   take,
   takeEnd,
   drop,
   splitAt,
   dropMarginRem,
   dropMargin,
   dropWhile,
   takeWhile,
   span,
   filter,
   zipWith,
   zipWith3,
   zipWith4,
   zipWithAppend,
   zipWithLastPattern,
   zipWithLastPattern3,
   zipWithLastPattern4,
   zipWithSize,
   zipWithSize3,
   zipWithSize4,
   sieve,
   deinterleave,
   interleaveFirstPattern,
   pad,
   hGetContentsAsync,
   hGetContentsSync,
   hPut,
   readFileAsync,
   writeFile,
   appendFile,
   interact,
   ) where

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as V
import qualified Data.List as List

import Foreign.Storable (Storable)
import System.IO (IO, FilePath, Handle)
import Test.QuickCheck (Arbitrary(arbitrary))

import Control.DeepSeq (NFData, rnf)
import Control.Monad (fmap)

import Data.Function.HT (compose2)
import Data.Tuple.HT (mapPair, mapFst, mapSnd)
import Data.Maybe.HT (toMaybe)
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Data.Semigroup (Semigroup, (<>), )
import Data.Either (Either)
import Data.Maybe (Maybe(Just))
import Data.Function (flip, ($), (.))
import Data.Tuple (fst)
import Data.Bool (Bool)
import Data.Ord (Ord, (<), (>=))
import Data.Eq (Eq, (==))
import Text.Show (Show, showsPrec, showParen, showString)
import Prelude (IOError, Int, succ)


{- |
A @Vector size a@ represents a chunky storable vector
with maximum chunk size expressed by type parameter @size@.
-}
newtype Vector size a = SV {plain :: SVL.Vector a}


newtype ChunkSize size = ChunkSize Int

lazyChunkSize :: ChunkSize size -> SVL.ChunkSize
lazyChunkSize (ChunkSize size) = SVL.chunkSize size

class Size size where
   chunkSize :: ChunkSize size

instance Size Size1024 where
   chunkSize = ChunkSize 1024

_dummySize :: Size1024
_dummySize = Size1024

type DefaultChunkSize = Size1024
data Size1024 = Size1024

type DefaultVector = Vector DefaultChunkSize


withChunkSize ::
   (Size size) => (ChunkSize size -> Vector size a) -> Vector size a
withChunkSize f = f chunkSize

withLazyChunkSize ::
   (Size size) => (SVL.ChunkSize -> SVL.Vector a) -> Vector size a
withLazyChunkSize f = withChunkSize $ lift0 . f . lazyChunkSize

getChunkSize :: (Size size) => Vector size a -> ChunkSize size
getChunkSize _ = chunkSize


lift0 :: SVL.Vector a -> Vector size a
lift0 = SV

lift1 ::
   (SVL.Vector a -> SVL.Vector b) ->
   Vector size a -> Vector size b
lift1 f (SV a) = SV (f a)

lift2 ::
   (SVL.Vector a -> SVL.Vector b -> SVL.Vector c) ->
   Vector size a -> Vector size b -> Vector size c
lift2 f (SV a) (SV b) = SV (f a b)

lift3 ::
   (SVL.Vector a -> SVL.Vector b -> SVL.Vector c -> SVL.Vector d) ->
   Vector size a -> Vector size b -> Vector size c -> Vector size d
lift3 f (SV a) (SV b) (SV c) = SV (f a b c)

lift4 ::
   (SVL.Vector a -> SVL.Vector b -> SVL.Vector c -> SVL.Vector d ->
    SVL.Vector e) ->
   Vector size a -> Vector size b -> Vector size c -> Vector size d ->
   Vector size e
lift4 f (SV a) (SV b) (SV c) (SV d) = SV (f a b c d)


instance (Size size, Storable a) => Semigroup (Vector size a) where
    (<>) = append

instance (Size size, Storable a) => Monoid (Vector size a) where
    mempty  = empty
    mappend = append
    mconcat = concat

instance (Size size, Storable a, Eq a) => Eq (Vector size a) where
   (==) = equal

instance (Size size, Storable a, Show a) => Show (Vector size a) where
   showsPrec p xs =
      showParen (p>=10)
         (showString "VectorLazyTyped.fromChunks " .
          showsPrec 10 (SVL.chunks $ plain xs))

{- |
This generates chunks of maximum size.
If you want to have chunks of varying size, use

> fromChunks <$> arbitrary

instead.
-}
instance (Size size, Storable a, Arbitrary a) => Arbitrary (Vector size a) where
   arbitrary = fmap pack arbitrary

instance (Size size, Storable a) => NFData (Vector size a) where
   rnf = rnf . plain


-- * Introducing and eliminating 'Vector's

{-# INLINE empty #-}
empty :: (Storable a) => Vector size a
empty = lift0 SVL.empty

{-# INLINE singleton #-}
singleton :: (Storable a) => a -> Vector size a
singleton = lift0 . SVL.singleton

toVectorLazy :: Vector size a -> SVL.Vector a
toVectorLazy = plain

{- |
This will maintain all laziness breaks,
but if chunks are too big, they will be split.
-}
fromVectorLazy :: (Size size, Storable a) => SVL.Vector a -> Vector size a
fromVectorLazy = fromChunks . SVL.chunks

chunks :: Vector size a -> [V.Vector a]
chunks = SVL.chunks . plain

fromChunks :: (Size size, Storable a) => [V.Vector a] -> Vector size a
fromChunks xs =
   withChunkSize $ \(ChunkSize size) ->
      fromChunksUnchecked $ List.concatMap (V.sliceVertical size) xs

fromChunksUnchecked :: (Storable a) => [V.Vector a] -> Vector size a
fromChunksUnchecked = lift0 . SVL.fromChunks

pack :: (Size size, Storable a) => [a] -> Vector size a
pack xs = withLazyChunkSize $ \size -> SVL.pack size xs

unpack :: (Storable a) => Vector size a -> [a]
unpack = SVL.unpack . plain


{-# INLINE unfoldr #-}
unfoldr :: (Size size, Storable b) =>
   (a -> Maybe (b,a)) ->
   a ->
   Vector size b
unfoldr f a =
   withLazyChunkSize $ \cs -> SVL.unfoldr cs f a

{-# INLINE unfoldrResult #-}
unfoldrResult :: (Size size, Storable b) =>
   (a -> Either c (b, a)) ->
   a ->
   (Vector size b, c)
unfoldrResult f a =
   let x =
         mapFst lift0 $
         SVL.unfoldrResult (lazyChunkSize $ getChunkSize $ fst x) f a
   in  x


{-# INLINE sample #-}
sample, _sample :: (Size size, Storable a) => (Int -> a) -> Vector size a
sample f = withLazyChunkSize $ \cs -> SVL.sample cs f

_sample f = unfoldr (\i -> Just (f i, succ i)) 0

{-# INLINE sampleN #-}
sampleN, _sampleN ::
   (Size size, Storable a) => Int -> (Int -> a) -> Vector size a
sampleN n f = withLazyChunkSize $ \cs -> SVL.sampleN cs n f

_sampleN n f = unfoldr (\i -> toMaybe (i<n) (f i, succ i)) 0


{-# INLINE iterate #-}
iterate :: (Size size, Storable a) => (a -> a) -> a -> Vector size a
iterate f a = withLazyChunkSize $ \cs -> SVL.iterate cs f a

repeat :: (Size size, Storable a) => a -> Vector size a
repeat a = withLazyChunkSize $ \cs -> SVL.repeat cs a

cycle :: (Size size, Storable a) => Vector size a -> Vector size a
cycle = lift1 SVL.cycle

replicate :: (Size size, Storable a) => Int -> a -> Vector size a
replicate n a = withLazyChunkSize $ \cs -> SVL.replicate cs n a



-- * Basic interface

{-# INLINE null #-}
null :: (Size size, Storable a) => Vector size a -> Bool
null = SVL.null . plain

length :: Vector size a -> Int
length = SVL.length . plain

equal :: (Size size, Storable a, Eq a) => Vector size a -> Vector size a -> Bool
equal = compose2 SVL.equal plain

index :: (Size size, Storable a) => Vector size a -> Int -> a
index (SV xs) = SVL.index xs


{-# INLINE cons #-}
cons :: (Size size, Storable a) => a -> Vector size a -> Vector size a
cons x = lift1 (SVL.cons x)

infixr 5 `append`

{-# INLINE append #-}
append ::
   (Size size, Storable a) => Vector size a -> Vector size a -> Vector size a
append = lift2 SVL.append


{- |
@extendL x y@
prepends the chunk @x@ and merges it with the first chunk of @y@
if the total size is at most @size@.
This way you can prepend small chunks
while asserting a reasonable average size for chunks.
The prepended chunk must be smaller than the maximum chunk size in the Vector.
This is not checked.
-}
extendL ::
   (Size size, Storable a) => V.Vector a -> Vector size a -> Vector size a
extendL x ys = withLazyChunkSize $ \cs -> SVL.extendL cs x (plain ys)


concat :: (Size size, Storable a) => [Vector size a] -> Vector size a
concat = lift0 . SVL.concat . List.map plain

sliceVertical ::
   (Size size, Storable a) => Int -> Vector size a -> [Vector size a]
sliceVertical n = List.map lift0 . SVL.sliceVertical n . plain

{-# INLINE snoc #-}
snoc :: (Size size, Storable a) => Vector size a -> a -> Vector size a
snoc = flip $ \x -> lift1 (flip SVL.snoc x)


-- * Transformations

{-# INLINE map #-}
map :: (Size size, Storable x, Storable y) =>
      (x -> y)
   -> Vector size x
   -> Vector size y
map f = lift1 (SVL.map f)


reverse :: (Size size, Storable a) => Vector size a -> Vector size a
reverse = lift1 SVL.reverse


-- * Reducing 'Vector's

{-# INLINE foldl #-}
foldl :: (Size size, Storable b) => (a -> b -> a) -> a -> Vector size b -> a
foldl f x0 = SVL.foldl f x0 . plain

{-# INLINE foldl' #-}
foldl' :: (Size size, Storable b) => (a -> b -> a) -> a -> Vector size b -> a
foldl' f x0 = SVL.foldl' f x0 . plain

{-# INLINE foldr #-}
foldr :: (Size size, Storable b) => (b -> a -> a) -> a -> Vector size b -> a
foldr f x0 = SVL.foldr f x0 . plain


{-# INLINE foldMap #-}
foldMap ::
   (Size size, Storable a, Monoid m) => (a -> m) -> Vector size a -> m
foldMap f = SVL.foldMap f . plain

{-# INLINE any #-}
any :: (Size size, Storable a) => (a -> Bool) -> Vector size a -> Bool
any p = SVL.any p . plain

{-# INLINE all #-}
all :: (Size size, Storable a) => (a -> Bool) -> Vector size a -> Bool
all p = SVL.all p . plain

maximum :: (Size size, Storable a, Ord a) => Vector size a -> a
maximum = SVL.maximum . plain

minimum :: (Size size, Storable a, Ord a) => Vector size a -> a
minimum = SVL.minimum . plain


-- * inspecting a vector

{-# INLINE viewL #-}
viewL :: (Size size, Storable a) => Vector size a -> Maybe (a, Vector size a)
viewL = fmap (mapSnd lift0) . SVL.viewL . plain

{-# INLINE viewR #-}
viewR :: (Size size, Storable a) => Vector size a -> Maybe (Vector size a, a)
viewR = fmap (mapFst lift0) . SVL.viewR . plain

{-# INLINE switchL #-}
switchL ::
   (Size size, Storable a) =>
   b -> (a -> Vector size a -> b) -> Vector size a -> b
switchL n j = SVL.switchL n (\a -> j a . lift0) . plain

{-# INLINE switchR #-}
switchR ::
   (Size size, Storable a) =>
   b -> (Vector size a -> a -> b) -> Vector size a -> b
switchR n j = SVL.switchR n (j . lift0) . plain


{-# INLINE scanl #-}
scanl :: (Size size, Storable a, Storable b) =>
   (a -> b -> a) -> a -> Vector size b -> Vector size a
scanl f start = lift1 $ SVL.scanl f start

{-# INLINE mapAccumL #-}
mapAccumL :: (Size size, Storable a, Storable b) =>
   (acc -> a -> (acc, b)) -> acc -> Vector size a -> (acc, Vector size b)
mapAccumL f start = mapSnd lift0 . SVL.mapAccumL f start . plain

{-# INLINE mapAccumR #-}
mapAccumR :: (Size size, Storable a, Storable b) =>
   (acc -> a -> (acc, b)) -> acc -> Vector size a -> (acc, Vector size b)
mapAccumR f start = mapSnd lift0 . SVL.mapAccumR f start . plain

{-# INLINE crochetL #-}
crochetL ::
   (Size size, Storable x, Storable y) =>
      (x -> acc -> Maybe (y, acc))
   -> acc
   -> Vector size x
   -> Vector size y
crochetL f acc0 = lift1 $ SVL.crochetL f acc0



-- * sub-vectors

{-# INLINE take #-}
take :: (Size size, Storable a) => Int -> Vector size a -> Vector size a
take n = lift1 $ SVL.take n

{-# INLINE takeEnd #-}
takeEnd :: (Size size, Storable a) => Int -> Vector size a -> Vector size a
takeEnd n = lift1 $ SVL.takeEnd n

{-# INLINE drop #-}
drop :: (Size size, Storable a) => Int -> Vector size a -> Vector size a
drop n = lift1 $ SVL.drop n

{-# INLINE splitAt #-}
splitAt ::
   (Size size, Storable a) =>
   Int -> Vector size a -> (Vector size a, Vector size a)
splitAt n =
   mapPair (lift0, lift0) . SVL.splitAt n . plain



{-# INLINE dropMarginRem #-}
dropMarginRem ::
   (Size size, Storable a) =>
   Int -> Int -> Vector size a -> (Int, Vector size a)
dropMarginRem n m = mapSnd lift0 . SVL.dropMarginRem n m . plain

{-# INLINE dropMargin #-}
dropMargin ::
   (Size size, Storable a) => Int -> Int -> Vector size a -> Vector size a
dropMargin n m = lift1 $ SVL.dropMargin n m



{-# INLINE dropWhile #-}
dropWhile ::
   (Size size, Storable a) => (a -> Bool) -> Vector size a -> Vector size a
dropWhile p = lift1 $ SVL.dropWhile p

{-# INLINE takeWhile #-}
takeWhile ::
   (Size size, Storable a) => (a -> Bool) -> Vector size a -> Vector size a
takeWhile p = lift1 $ SVL.takeWhile p


{-# INLINE span #-}
span ::
   (Size size, Storable a) =>
   (a -> Bool) -> Vector size a -> (Vector size a, Vector size a)
span p = mapPair (lift0, lift0) . SVL.span p . plain



-- * other functions


{-# INLINE filter #-}
filter ::
   (Size size, Storable a) => (a -> Bool) -> Vector size a -> Vector size a
filter p = lift1 $ SVL.filter p


{- |
Generates laziness breaks
wherever one of the input signals has a chunk boundary.
-}
{-# INLINE zipWith #-}
zipWith :: (Size size, Storable a, Storable b, Storable c) =>
      (a -> b -> c)
   -> Vector size a
   -> Vector size b
   -> Vector size c
zipWith f = lift2 $ SVL.zipWith f

{-# INLINE zipWith3 #-}
zipWith3 :: (Size size, Storable a, Storable b, Storable c, Storable d) =>
      (a -> b -> c -> d)
   -> Vector size a
   -> Vector size b
   -> Vector size c
   -> Vector size d
zipWith3 f = lift3 $ SVL.zipWith3 f

{-# INLINE zipWith4 #-}
zipWith4 ::
   (Size size, Storable a, Storable b, Storable c, Storable d, Storable e) =>
      (a -> b -> c -> d -> e)
   -> Vector size a
   -> Vector size b
   -> Vector size c
   -> Vector size d
   -> Vector size e
zipWith4 f = lift4 $ SVL.zipWith4 f


{-# INLINE zipWithAppend #-}
zipWithAppend :: (Size size, Storable a) =>
      (a -> a -> a)
   -> Vector size a
   -> Vector size a
   -> Vector size a
zipWithAppend f = lift2 $ SVL.zipWithAppend f



{- |
Preserves chunk pattern of the last argument.
-}
{-# INLINE zipWithLastPattern #-}
zipWithLastPattern :: (Size size, Storable a, Storable b, Storable c) =>
      (a -> b -> c)
   -> Vector size a
   -> Vector size b
   -> Vector size c
zipWithLastPattern f = lift2 $ SVL.zipWithLastPattern f

{- |
Preserves chunk pattern of the last argument.
-}
{-# INLINE zipWithLastPattern3 #-}
zipWithLastPattern3 ::
   (Size size, Storable a, Storable b, Storable c, Storable d) =>
   (a -> b -> c -> d) ->
   (Vector size a -> Vector size b -> Vector size c -> Vector size d)
zipWithLastPattern3 f = lift3 $ SVL.zipWithLastPattern3 f

{- |
Preserves chunk pattern of the last argument.
-}
{-# INLINE zipWithLastPattern4 #-}
zipWithLastPattern4 ::
   (Size size, Storable a, Storable b, Storable c, Storable d, Storable e) =>
   (a -> b -> c -> d -> e) ->
   (Vector size a -> Vector size b -> Vector size c -> Vector size d -> Vector size e)
zipWithLastPattern4 f = lift4 $ SVL.zipWithLastPattern4 f


{-# INLINE zipWithSize #-}
zipWithSize :: (Size size, Storable a, Storable b, Storable c) =>
      (a -> b -> c)
   -> Vector size a
   -> Vector size b
   -> Vector size c
zipWithSize f a b =
   withLazyChunkSize $ \cs -> SVL.zipWithSize cs f (plain a) (plain b)

{-# INLINE zipWithSize3 #-}
zipWithSize3 ::
   (Size size, Storable a, Storable b, Storable c, Storable d) =>
   (a -> b -> c -> d) ->
   (Vector size a -> Vector size b -> Vector size c -> Vector size d)
zipWithSize3 f a b c =
   withLazyChunkSize $ \cs ->
      SVL.zipWithSize3 cs f (plain a) (plain b) (plain c)

{-# INLINE zipWithSize4 #-}
zipWithSize4 ::
   (Size size, Storable a, Storable b, Storable c, Storable d, Storable e) =>
   (a -> b -> c -> d -> e) ->
   (Vector size a -> Vector size b -> Vector size c -> Vector size d -> Vector size e)
zipWithSize4 f a b c d =
   withLazyChunkSize $ \cs ->
      SVL.zipWithSize4 cs f (plain a) (plain b) (plain c) (plain d)


-- * interleaved vectors

{-# INLINE sieve #-}
sieve :: (Size size, Storable a) => Int -> Vector size a -> Vector size a
sieve n = lift1 $ SVL.sieve n

{-# INLINE deinterleave #-}
deinterleave ::
   (Size size, Storable a) => Int -> Vector size a -> [Vector size a]
deinterleave n =
   List.map lift0 . SVL.deinterleave n . plain

{- |
Interleave lazy vectors
while maintaining the chunk pattern of the first vector.
All input vectors must have the same length.
-}
{-# INLINE interleaveFirstPattern #-}
interleaveFirstPattern ::
   (Size size, Storable a) => [Vector size a] -> Vector size a
interleaveFirstPattern = lift0 . SVL.interleaveFirstPattern . List.map plain



{- |
Ensure a minimal length of the list by appending pad values.
-}
{- disabled INLINE pad -}
pad ::
   (Size size, Storable a) =>
   a -> Int -> Vector size a -> Vector size a
pad y n xs = withLazyChunkSize $ \cs -> SVL.pad cs y n $ plain xs





-- * IO

withIOErrorChunkSize ::
   (Size size) =>
   (ChunkSize size -> IO (IOError, Vector size a)) ->
   IO (IOError, Vector size a)
withIOErrorChunkSize act = act chunkSize

hGetContentsAsync :: (Size size, Storable a) =>
   Handle -> IO (IOError, Vector size a)
hGetContentsAsync h =
   withIOErrorChunkSize $ \cs ->
      fmap (mapSnd lift0) $ SVL.hGetContentsAsync (lazyChunkSize cs) h


withIOChunkSize ::
   (Size size) =>
   (ChunkSize size -> IO (Vector size a)) ->
   IO (Vector size a)
withIOChunkSize act = act chunkSize

hGetContentsSync ::
   (Size size, Storable a) =>
   Handle -> IO (Vector size a)
hGetContentsSync h =
   withIOChunkSize $ \cs ->
      fmap lift0 $ SVL.hGetContentsSync (lazyChunkSize cs) h

hPut :: (Size size, Storable a) => Handle -> Vector size a -> IO ()
hPut h = SVL.hPut h . plain

readFileAsync ::
   (Size size, Storable a) => FilePath -> IO (IOError, Vector size a)
readFileAsync path =
   withIOErrorChunkSize $ \cs ->
      fmap (mapSnd lift0) $ SVL.readFileAsync (lazyChunkSize cs) path

writeFile :: (Size size, Storable a) => FilePath -> Vector size a -> IO ()
writeFile path = SVL.writeFile path . plain

appendFile :: (Size size, Storable a) => FilePath -> Vector size a -> IO ()
appendFile path = SVL.appendFile path . plain

interact ::
   (Size size, Storable a) =>
   (Vector size a -> Vector size a) -> IO ()
interact = interactAux chunkSize

interactAux ::
   (Size size, Storable a) =>
   ChunkSize size -> (Vector size a -> Vector size a) -> IO ()
interactAux cs f = SVL.interact (lazyChunkSize cs) (plain . f . lift0)
