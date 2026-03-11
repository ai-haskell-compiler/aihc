{- |
Chunky signal stream built on StorableVector.

Hints for fusion:
 - Higher order functions should always be inlined in the end
   in order to turn them into machine loops
   instead of calling a function in an inner loop.
-}
module Data.StorableVector.Lazy (
   -- constructors should not be exported
   Vector(SV, chunks),
   ChunkSize(ChunkSize),
   chunkSize,
   defaultChunkSize,
   empty,
   singleton,
   fromChunks,
   pack,
   unpack,
   packWith,
   unpackWith,
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
   monoidConcatMap,
   any,
   all,
   maximum,
   minimum,
   pointer,
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
   compact,
   fromChunk,
   hGetContentsAsync,
   hGetContentsSync,
   hPut,
   readFileAsync,
   writeFile,
   appendFile,
   interact,
   -- should not be exported or should be exported from plain StorableVector
   crochetLChunk,
   -- should not be exported
   padAlt,
   cancelNullVector,
   moduleError,
   ) where

import qualified Data.List as List
import qualified Data.StorableVector as V
import qualified Data.StorableVector.Base as VB
import qualified Data.StorableVector.Lazy.PointerPrivate as Ptr

import qualified Numeric.NonNegative.Class as NonNeg

import qualified Data.List.HT as ListHT
import Data.Tuple.HT (mapPair, mapFst, mapSnd, swap, )
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (fromMaybe, )

import Foreign.Storable (Storable)

import Data.Monoid (Monoid, mempty, mappend, mconcat, )
import Data.Semigroup (Semigroup, (<>), )
-- import Control.Arrow ((***))
import Control.Monad (liftM, liftM2, liftM3, liftM4, mfilter, )


import qualified System.IO as IO
import System.IO (openBinaryFile, IOMode(WriteMode, ReadMode, AppendMode),
                  hClose, Handle)

import Control.DeepSeq (NFData, rnf)
import Control.Exception (bracket, catch, )

import qualified System.IO.Error as Exc
import qualified System.Unsafe as Unsafe

import qualified Test.QuickCheck as QC


{-
import Prelude hiding
   (length, (++), concat, iterate, foldl, map, repeat, replicate, null,
    zip, zipWith, zipWith3, drop, take, splitAt, takeWhile, dropWhile, reverse)
-}

import qualified Prelude as P

import Data.Either (Either(Left, Right), either, )
import Data.Maybe (Maybe(Just, Nothing), maybe, )
import Data.Function (const, flip, id, ($), (.), )
import Data.Tuple (fst, snd, uncurry, )
import Data.Bool (Bool(True,False), not, (&&), )
import Data.Ord (Ord, (<), (>), (<=), (>=), min, max, )
import Data.Eq (Eq, (==), )
import Control.Monad (mapM_, fmap, (=<<), (>>=), (>>), return, )
import Text.Show (Show, showsPrec, showParen, showString, show, )
import Prelude
   (IO, error, IOError,
    FilePath, String, succ,
    Num, Int, sum, (+), (-), divMod, mod, fromInteger, )



newtype Vector a = SV {chunks :: [V.Vector a]}


instance (Storable a) => Semigroup (Vector a) where
    (<>) = append

instance (Storable a) => Monoid (Vector a) where
    mempty  = empty
    mappend = append
    mconcat = concat

instance (Storable a, Eq a) => Eq (Vector a) where
   (==) = equal

instance (Storable a, Show a) => Show (Vector a) where
   showsPrec p xs =
      showParen (p>=10)
         (showString "VectorLazy.fromChunks " .
          showsPrec 10 (chunks xs))

instance (Storable a, QC.Arbitrary a) => QC.Arbitrary (Vector a) where
   arbitrary = liftM2 pack QC.arbitrary QC.arbitrary

instance (Storable a) => NFData (Vector a) where
   rnf = rnf . List.map rnf . chunks


-- for a list of chunk sizes see "Data.StorableVector.LazySize".
newtype ChunkSize = ChunkSize Int
   deriving (Eq, Ord, Show)

instance QC.Arbitrary ChunkSize where
   arbitrary = fmap ChunkSize $ QC.choose (1,2048)

{-
ToDo:
Since non-negative-0.1 we have the Monoid superclass for NonNeg.
Maybe we do not need the Num instance anymore.
-}
instance Num ChunkSize where
   (ChunkSize x) + (ChunkSize y)  =  ChunkSize (x+y)
   (-)  =  moduleError "ChunkSize.-" "intentionally unimplemented"
   (*)  =  moduleError "ChunkSize.*" "intentionally unimplemented"
   abs  =  moduleError "ChunkSize.abs" "intentionally unimplemented"
   signum  =  moduleError "ChunkSize.signum" "intentionally unimplemented"
   fromInteger = ChunkSize . fromInteger

instance Semigroup ChunkSize where
   ChunkSize x <> ChunkSize y = ChunkSize (x+y)

instance Monoid ChunkSize where
   mempty = ChunkSize 0
   mappend (ChunkSize x) (ChunkSize y) = ChunkSize (x+y)
   mconcat = ChunkSize . sum . List.map (\(ChunkSize c) -> c)

instance NonNeg.C ChunkSize where
   split = NonNeg.splitDefault (\(ChunkSize c) -> c) ChunkSize

chunkSize :: Int -> ChunkSize
chunkSize x =
   ChunkSize $
      if x>0
        then x
        else moduleError "chunkSize" ("no positive number: " List.++ show x)

defaultChunkSize :: ChunkSize
defaultChunkSize =
   ChunkSize 1024



-- * Introducing and eliminating 'Vector's

{-# INLINE empty #-}
empty :: (Storable a) => Vector a
empty = SV []

{-# INLINE singleton #-}
singleton :: (Storable a) => a -> Vector a
singleton x = SV [V.singleton x]

fromChunks :: (Storable a) => [V.Vector a] -> Vector a
fromChunks = SV

pack :: (Storable a) => ChunkSize -> [a] -> Vector a
pack size = unfoldr size ListHT.viewL

unpack :: (Storable a) => Vector a -> [a]
unpack = List.concatMap V.unpack . chunks


{-# WARNING packWith "It seems to be used nowhere and might be removed." #-}
{-# INLINE packWith #-}
packWith :: (Storable b) => ChunkSize -> (a -> b) -> [a] -> Vector b
packWith size f = unfoldr size (fmap (mapFst f) . ListHT.viewL)

{-# WARNING unpackWith "It seems to be used nowhere and might be removed." #-}
{-# INLINE unpackWith #-}
unpackWith :: (Storable a) => (a -> b) -> Vector a -> [b]
unpackWith f = List.concatMap (V.unpackWith f) . chunks


{-# INLINE unfoldr #-}
unfoldr :: (Storable b) =>
   ChunkSize ->
   (a -> Maybe (b,a)) ->
   a ->
   Vector b
unfoldr (ChunkSize size) f =
   SV .
   List.unfoldr (cancelNullVector . V.unfoldrN size f =<<) .
   Just

{- |
Example:

> *Data.StorableVector.Lazy> unfoldrResult (ChunkSize 5) (\c -> if c>'z' then Left (Char.ord c) else Right (c, succ c)) 'a'
> (VectorLazy.fromChunks [Vector.pack "abcde",Vector.pack "fghij",Vector.pack "klmno",Vector.pack "pqrst",Vector.pack "uvwxy",Vector.pack "z"],123)
-}
{-# INLINE unfoldrResult #-}
unfoldrResult :: (Storable b) =>
   ChunkSize ->
   (a -> Either c (b, a)) ->
   a ->
   (Vector b, c)
unfoldrResult (ChunkSize size) f =
   let recourse a0 =
          let (chunk, a1) =
                 V.unfoldrResultN size Right (either (Left . Left) Right . f) a0
          in  either
                 ((,) (if V.null chunk then [] else [chunk]))
                 (mapFst (chunk :) . recourse) a1
   in  mapFst SV . recourse


{-# INLINE sample #-}
sample :: (Storable a) => ChunkSize -> (Int -> a) -> Vector a
sample size f =
   unfoldr size (\i -> Just (f i, succ i)) 0

{-# INLINE sampleN #-}
sampleN :: (Storable a) => ChunkSize -> Int -> (Int -> a) -> Vector a
sampleN size n f =
   unfoldr size (\i -> toMaybe (i<n) (f i, succ i)) 0


{-# INLINE iterate #-}
iterate :: Storable a => ChunkSize -> (a -> a) -> a -> Vector a
iterate size f = unfoldr size (\x -> Just (x, f x))

repeat :: Storable a => ChunkSize -> a -> Vector a
repeat (ChunkSize size) =
   SV . List.repeat . V.replicate size

cycle :: Storable a => Vector a -> Vector a
cycle =
   SV . List.cycle . chunks

replicate :: Storable a => ChunkSize -> Int -> a -> Vector a
replicate (ChunkSize size) n x =
   let (numChunks, rest) = divMod n size
   in  append
          (SV (List.replicate numChunks (V.replicate size x)))
          (fromChunk (V.replicate rest x))




-- * Basic interface

{-# INLINE null #-}
null :: (Storable a) => Vector a -> Bool
null = List.null . chunks

length :: Vector a -> Int
length = sum . List.map V.length . chunks

equal :: (Storable a, Eq a) => Vector a -> Vector a -> Bool
equal (SV xs0) (SV ys0) =
   let recourse (x:xs) (y:ys) =
          let l = min (V.length x) (V.length y)
              (xPrefix, xSuffix) = V.splitAt l x
              (yPrefix, ySuffix) = V.splitAt l y
              build z zs =
                 if V.null z then zs else z:zs
          in  xPrefix == yPrefix &&
              recourse (build xSuffix xs) (build ySuffix ys)
       recourse [] [] = True
       -- this requires that chunks will always be non-empty
       recourse _ _ = False
   in  recourse xs0 ys0

index :: (Storable a) => Vector a -> Int -> a
index (SV xs) n =
   if n < 0
     then
        moduleError "index"
           ("negative index: " List.++ show n)
     else
        List.foldr
           (\x k m0 ->
              let m1 = m0 - V.length x
              in  if m1 < 0
                    then VB.unsafeIndex x m0
                    else k m1)
           (\m -> moduleError "index"
                     ("index too large: " List.++ show n
                      List.++ ", length = " List.++ show (n-m)))
           xs n


{-# NOINLINE [0] cons #-}
cons :: Storable a => a -> Vector a -> Vector a
cons x = SV . (V.singleton x :) . chunks

infixr 5 `append`

{-# NOINLINE [0] append #-}
append :: Storable a => Vector a -> Vector a -> Vector a
append (SV xs) (SV ys)  =  SV (xs List.++ ys)


{- |
@extendL size x y@
prepends the chunk @x@ and merges it with the first chunk of @y@
if the total size is at most @size@.
This way you can prepend small chunks
while asserting a reasonable average size for chunks.
-}
extendL :: Storable a => ChunkSize -> V.Vector a -> Vector a -> Vector a
extendL (ChunkSize size) x (SV yt) =
   SV $
   maybe
      [x]
      (\(y,ys) ->
          if V.length x + V.length y <= size
            then V.append x y : ys
            else x:yt)
      (ListHT.viewL yt)


concat :: (Storable a) => [Vector a] -> Vector a
concat = SV . List.concat . List.map chunks

sliceVertical :: (Storable a) => Int -> Vector a -> [Vector a]
sliceVertical n =
   List.unfoldr (\x -> toMaybe (not (null x)) (splitAt n x))

{-# NOINLINE [0] snoc #-}
snoc :: Storable a => Vector a -> a -> Vector a
snoc xs x = append xs $ singleton x


-- * Transformations

{-# INLINE map #-}
map :: (Storable x, Storable y) =>
      (x -> y)
   -> Vector x
   -> Vector y
map f = SV . List.map (V.map f) . chunks


reverse :: Storable a => Vector a -> Vector a
reverse =
   SV . List.reverse . List.map V.reverse . chunks


-- * Reducing 'Vector's

{-# INLINE foldl #-}
foldl :: Storable b => (a -> b -> a) -> a -> Vector b -> a
foldl f x0 = List.foldl (V.foldl f) x0 . chunks

{-# INLINE foldl' #-}
foldl' :: Storable b => (a -> b -> a) -> a -> Vector b -> a
foldl' f x0 = List.foldl' (V.foldl f) x0 . chunks

{-# INLINE foldr #-}
foldr :: Storable b => (b -> a -> a) -> a -> Vector b -> a
foldr f x0 = List.foldr (flip (V.foldr f)) x0 . chunks


{-# INLINE foldMap #-}
foldMap :: (Storable a, Monoid m) => (a -> m) -> Vector a -> m
foldMap f = List.foldr (mappend . V.foldMap f) mempty . chunks

{-# DEPRECATED monoidConcatMap "Use foldMap instead." #-}
{-# INLINE monoidConcatMap #-}
monoidConcatMap :: (Storable a, Monoid m) => (a -> m) -> Vector a -> m
monoidConcatMap = foldMap

{-# INLINE any #-}
any :: (Storable a) => (a -> Bool) -> Vector a -> Bool
any p = List.any (V.any p) . chunks

{-# INLINE all #-}
all :: (Storable a) => (a -> Bool) -> Vector a -> Bool
all p = List.all (V.all p) . chunks

maximum, _maximum :: (Storable a, Ord a) => Vector a -> a
maximum = List.maximum . List.map V.maximum . chunks
_maximum = List.foldl1' max . List.map V.maximum . chunks

minimum, _minimum :: (Storable a, Ord a) => Vector a -> a
minimum = List.minimum . List.map V.minimum . chunks
_minimum = List.foldl1' min . List.map V.minimum . chunks

{-
It is not clear whether this implementation is good.
Associativity depends on the chunk structure,
but in principle chunks could be summed in parallel.

sum :: (Storable a, Num a) => Vector a -> a
sum =
   List.sum . List.map V.sum . chunks

product :: (Storable a, Num a) => Vector a -> a
product =
   List.product . List.map V.product . chunks
-}


-- * inspecting a vector

{-# INLINE pointer #-}
pointer :: Storable a => Vector a -> Ptr.Pointer a
pointer = Ptr.cons . chunks

{-# INLINE viewL #-}
viewL :: Storable a => Vector a -> Maybe (a, Vector a)
viewL (SV xs0) =
   do (x,xs) <- ListHT.viewL xs0
      (y,ys) <- V.viewL x
      return (y, append (fromChunk ys) (SV xs))

{-# INLINE viewR #-}
viewR :: Storable a => Vector a -> Maybe (Vector a, a)
viewR (SV xs0) =
   do xsp <- ListHT.viewR xs0
      let (xs,x) = xsp
{-
   do ~(xs,x) <- ListHT.viewR xs0
-}
      let (ys,y) = fromMaybe (moduleError "viewR" "last chunk empty") (V.viewR x)
      return (append (SV xs) (fromChunk ys), y)

{-# INLINE switchL #-}
switchL :: Storable a => b -> (a -> Vector a -> b) -> Vector a -> b
switchL n j =
   maybe n (uncurry j) . viewL

{-# INLINE switchR #-}
switchR :: Storable a => b -> (Vector a -> a -> b) -> Vector a -> b
switchR n j =
   maybe n (uncurry j) . viewR


{-
viewLSafe :: Storable a => Vector a -> Maybe (a, Vector a)
viewLSafe (SV xs0) =
   -- dropWhile would be unnecessary if we require that all chunks are non-empty
   do (x,xs) <- ListHT.viewL (List.dropWhile V.null xs0)
      (y,ys) <- viewLVector x
      return (y, append (fromChunk ys) (SV xs))

viewRSafe :: Storable a => Vector a -> Maybe (Vector a, a)
viewRSafe (SV xs0) =
   -- dropWhile would be unnecessary if we require that all chunks are non-empty
   do (xs,x) <- ListHT.viewR (dropWhileRev V.null xs0)
      (ys,y) <- V.viewR x
      return (append (SV xs) (fromChunk ys), y)
-}


{-# INLINE scanl #-}
scanl :: (Storable a, Storable b) =>
   (a -> b -> a) -> a -> Vector b -> Vector a
scanl f start =
   cons start . snd .
   mapAccumL (\acc -> (\b -> (b,b)) . f acc) start

{-# INLINE mapAccumL #-}
mapAccumL :: (Storable a, Storable b) =>
   (acc -> a -> (acc, b)) -> acc -> Vector a -> (acc, Vector b)
mapAccumL f start =
   mapSnd SV .
   List.mapAccumL (V.mapAccumL f) start .
   chunks

{-# INLINE mapAccumR #-}
mapAccumR :: (Storable a, Storable b) =>
   (acc -> a -> (acc, b)) -> acc -> Vector a -> (acc, Vector b)
mapAccumR f start =
   mapSnd SV .
   List.mapAccumR (V.mapAccumR f) start .
   chunks

{-# DEPRECATED crochetLChunk "Use Storable.Vector.crochetLResult" #-}
{-# INLINE crochetLChunk #-}
crochetLChunk :: (Storable x, Storable y) =>
      (x -> acc -> Maybe (y, acc))
   -> acc
   -> V.Vector x
   -> (V.Vector y, Maybe acc)
crochetLChunk = V.crochetLResult

{-# INLINE crochetL #-}
crochetL :: (Storable x, Storable y) =>
      (x -> acc -> Maybe (y, acc))
   -> acc
   -> Vector x
   -> Vector y
crochetL f acc0 =
   SV . List.unfoldr (\(xt,acc) ->
       do (x,xs) <- ListHT.viewL xt
          acc' <- acc
          return $ mapSnd ((,) xs) $ V.crochetLResult f acc' x) .
   flip (,) (Just acc0) .
   chunks



-- * sub-vectors

{-# INLINE take #-}
take :: (Storable a) => Int -> Vector a -> Vector a
{- this order of pattern matches is certainly the most lazy one
> take 4 (pack (chunkSize 2) $ "abcd" List.++ undefined)
VectorLazy.fromChunks [Vector.pack "ab",Vector.pack "cd"]
-}
take 0 _ = empty
take _ (SV []) = empty
take n (SV (x:xs)) =
   let m = V.length x
   in  if m<=n
         then SV $ (x:) $ chunks $ take (n-m) $ SV xs
         else fromChunk (V.take n x)

{- |
Take n values from the end of the vector in a memory friendly way.
@takeEnd n xs@ should perform the same as @drop (length xs - n) xs@,
but the latter one would have to materialise @xs@ completely.
In contrast to that
@takeEnd@ should hold only chunks of about @n@ elements at any time point.
-}
{-# INLINE takeEnd #-}
takeEnd :: (Storable a) => Int -> Vector a -> Vector a
takeEnd n xs =
   -- cf. Pattern.drop
   List.foldl (flip drop) xs $ List.map V.length $ chunks $ drop n xs

{-# INLINE drop #-}
drop :: (Storable a) => Int -> Vector a -> Vector a
drop _ (SV []) = empty
drop n (SV (x:xs)) =
   let m = V.length x
   in  if m<=n
         then drop (n-m) (SV xs)
         else SV (V.drop n x : xs)

{-# INLINE splitAt #-}
splitAt :: (Storable a) => Int -> Vector a -> (Vector a, Vector a)
splitAt n0 =
   {- this order of pattern matches is certainly the most lazy one
   > splitAt 4 (pack (chunkSize 2) $ "abcd" List.++ undefined)
   (VectorLazy.fromChunks [Vector.pack "ab",Vector.pack "cd"],VectorLazy.fromChunks *** Exception: Prelude.undefined
   -}
   let recourse 0 xs = ([], xs)
       recourse _ [] = ([], [])
       recourse n (x:xs) =
          let m = V.length x
          in  if m<=n
                then mapFst (x:) $ recourse (n-m) xs
                else mapPair ((:[]), (:xs)) $ V.splitAt n x
   in  mapPair (SV, SV) . recourse n0 . chunks



{-# INLINE dropMarginRem #-}
-- I have used this in an inner loop thus I prefer inlining
{- |
@dropMarginRem n m xs@
drops at most the first @m@ elements of @xs@
and ensures that @xs@ still contains @n@ elements.
Additionally returns the number of elements that could not be dropped
due to the margin constraint.
That is @dropMarginRem n m xs == (k,ys)@ implies @length xs - m == length ys - k@.
Requires @length xs >= n@.
-}
dropMarginRem :: (Storable a) => Int -> Int -> Vector a -> (Int, Vector a)
dropMarginRem n m xs =
   List.foldl'
      (\(mi,xsi) k -> (mi-k, drop k xsi))
      (m,xs)
      (List.map V.length $ chunks $ take m $ drop n xs)

{-
This implementation does only walk once through the dropped prefix.
It is maximally lazy and minimally space consuming.
-}
{-# INLINE dropMargin #-}
dropMargin :: (Storable a) => Int -> Int -> Vector a -> Vector a
dropMargin n m xs =
   List.foldl' (flip drop) xs
      (List.map V.length $ chunks $ take m $ drop n xs)



{-# INLINE dropWhile #-}
dropWhile :: (Storable a) => (a -> Bool) -> Vector a -> Vector a
dropWhile _ (SV []) = empty
dropWhile p (SV (x:xs)) =
   let y = V.dropWhile p x
   in  if V.null y
         then dropWhile p (SV xs)
         else SV (y:xs)

{-# INLINE takeWhile #-}
takeWhile :: (Storable a) => (a -> Bool) -> Vector a -> Vector a
takeWhile _ (SV []) = empty
takeWhile p (SV (x:xs)) =
   let y = V.takeWhile p x
   in  if V.length y < V.length x
         then fromChunk y
         else SV (x : chunks (takeWhile p (SV xs)))


{-# INLINE span #-}
span, _span :: (Storable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)
span p =
   let recourse [] = ([],[])
       recourse (x:xs) =
          let (y,z) = V.span p x
          in  if V.null z
                then mapFst (x:) (recourse xs)
                else (chunks $ fromChunk y, (z:xs))
   in  mapPair (SV, SV) . recourse . chunks

_span p =
   let recourse (SV []) = (empty, empty)
       recourse (SV (x:xs)) =
         let (y,z) = V.span p x
         in  if V.length y == 0
               then mapFst (SV . (x:) . chunks) (recourse (SV xs))
               else (SV [y], SV (z:xs))
   in  recourse


-- * other functions


{-# INLINE filter #-}
filter :: (Storable a) => (a -> Bool) -> Vector a -> Vector a
filter p =
   SV . List.filter (not . V.null) . List.map (V.filter p) . chunks


{- |
Generates laziness breaks
wherever one of the input signals has a chunk boundary.
-}
{-# INLINE zipWith #-}
zipWith :: (Storable a, Storable b, Storable c) =>
      (a -> b -> c)
   -> Vector a
   -> Vector b
   -> Vector c
zipWith = zipWithCont (const empty) (const empty)

{-# INLINE zipWith3 #-}
zipWith3 :: (Storable a, Storable b, Storable c, Storable d) =>
      (a -> b -> c -> d)
   -> Vector a
   -> Vector b
   -> Vector c
   -> Vector d
zipWith3 f as0 bs0 cs0 =
   let recourse at@(a:_) bt@(b:_) ct@(c:_) =
          let z = V.zipWith3 f a b c
              n = V.length z
          in  z : recourse
                     (chunks $ drop n $ fromChunks at)
                     (chunks $ drop n $ fromChunks bt)
                     (chunks $ drop n $ fromChunks ct)
       recourse _ _ _ = []
   in  fromChunks $ recourse (chunks as0) (chunks bs0) (chunks cs0)

{-# INLINE zipWith4 #-}
zipWith4 :: (Storable a, Storable b, Storable c, Storable d, Storable e) =>
      (a -> b -> c -> d -> e)
   -> Vector a
   -> Vector b
   -> Vector c
   -> Vector d
   -> Vector e
zipWith4 f as0 bs0 cs0 ds0 =
   let recourse at@(a:_) bt@(b:_) ct@(c:_) dt@(d:_) =
          let z = V.zipWith4 f a b c d
              n = V.length z
          in  z : recourse
                     (chunks $ drop n $ fromChunks at)
                     (chunks $ drop n $ fromChunks bt)
                     (chunks $ drop n $ fromChunks ct)
                     (chunks $ drop n $ fromChunks dt)
       recourse _ _ _ _ = []
   in  fromChunks $
       recourse (chunks as0) (chunks bs0) (chunks cs0) (chunks ds0)


{-# INLINE zipWithAppend #-}
zipWithAppend :: (Storable a) =>
      (a -> a -> a)
   -> Vector a
   -> Vector a
   -> Vector a
zipWithAppend = zipWithCont id id

{-# INLINE zipWithCont #-}
zipWithCont :: (Storable a, Storable b, Storable c) =>
      (Vector a -> Vector c)
   -> (Vector b -> Vector c)
   -> (a -> b -> c)
   -> Vector a
   -> Vector b
   -> Vector c
zipWithCont ga gb f as0 bs0 =
   let recourse at@(a:_) bt@(b:_) =
          let z = V.zipWith f a b
              n = V.length z
          in  z : recourse
                     (chunks $ drop n $ fromChunks at)
                     (chunks $ drop n $ fromChunks bt)
       recourse [] bs = chunks $ gb $ fromChunks bs
       recourse as [] = chunks $ ga $ fromChunks as
   in  fromChunks $ recourse (chunks as0) (chunks bs0)


{- |
Preserves chunk pattern of the last argument.
-}
{-# INLINE zipWithLastPattern #-}
zipWithLastPattern :: (Storable a, Storable b, Storable c) =>
      (a -> b -> c)
   -> Vector a
   -> Vector b
   -> Vector c
zipWithLastPattern f =
   crochetL (\y -> liftM (mapFst (flip f y)) . Ptr.viewL) . pointer

{- |
Preserves chunk pattern of the last argument.
-}
{-# INLINE zipWithLastPattern3 #-}
zipWithLastPattern3 ::
   (Storable a, Storable b, Storable c, Storable d) =>
   (a -> b -> c -> d) ->
   (Vector a -> Vector b -> Vector c -> Vector d)
zipWithLastPattern3 f s0 s1 =
   crochetL (\z (xt,yt) ->
      liftM2
         (\(x,xs) (y,ys) -> (f x y z, (xs,ys)))
         (Ptr.viewL xt)
         (Ptr.viewL yt))
      (pointer s0, pointer s1)

{- |
Preserves chunk pattern of the last argument.
-}
{-# INLINE zipWithLastPattern4 #-}
zipWithLastPattern4 ::
   (Storable a, Storable b, Storable c, Storable d, Storable e) =>
   (a -> b -> c -> d -> e) ->
   (Vector a -> Vector b -> Vector c -> Vector d -> Vector e)
zipWithLastPattern4 f s0 s1 s2 =
   crochetL (\w (xt,yt,zt) ->
      liftM3
         (\(x,xs) (y,ys) (z,zs) -> (f x y z w, (xs,ys,zs)))
         (Ptr.viewL xt)
         (Ptr.viewL yt)
         (Ptr.viewL zt))
      (pointer s0, pointer s1, pointer s2)


{-# INLINE zipWithSize #-}
zipWithSize :: (Storable a, Storable b, Storable c) =>
      ChunkSize
   -> (a -> b -> c)
   -> Vector a
   -> Vector b
   -> Vector c
zipWithSize size f s0 s1 =
   unfoldr size (\(xt,yt) ->
      liftM2
         (\(x,xs) (y,ys) -> (f x y, (xs,ys)))
         (Ptr.viewL xt)
         (Ptr.viewL yt))
      (pointer s0, pointer s1)

{-# INLINE zipWithSize3 #-}
zipWithSize3 ::
   (Storable a, Storable b, Storable c, Storable d) =>
   ChunkSize -> (a -> b -> c -> d) ->
   (Vector a -> Vector b -> Vector c -> Vector d)
zipWithSize3 size f s0 s1 s2 =
   unfoldr size (\(xt,yt,zt) ->
      liftM3
         (\(x,xs) (y,ys) (z,zs) -> (f x y z, (xs,ys,zs)))
         (Ptr.viewL xt)
         (Ptr.viewL yt)
         (Ptr.viewL zt))
      (pointer s0, pointer s1, pointer s2)

{-# INLINE zipWithSize4 #-}
zipWithSize4 ::
   (Storable a, Storable b, Storable c, Storable d, Storable e) =>
   ChunkSize -> (a -> b -> c -> d -> e) ->
   (Vector a -> Vector b -> Vector c -> Vector d -> Vector e)
zipWithSize4 size f s0 s1 s2 s3 =
   unfoldr size (\(xt,yt,zt,wt) ->
      liftM4
         (\(x,xs) (y,ys) (z,zs) (w,ws) -> (f x y z w, (xs,ys,zs,ws)))
         (Ptr.viewL xt)
         (Ptr.viewL yt)
         (Ptr.viewL zt)
         (Ptr.viewL wt))
      (pointer s0, pointer s1, pointer s2, pointer s3)


-- * interleaved vectors

{-# INLINE sieve #-}
sieve :: (Storable a) => Int -> Vector a -> Vector a
sieve n =
   fromChunks . List.filter (not . V.null) . snd .
   List.mapAccumL
      (\offset chunk ->
         (mod (offset - V.length chunk) n,
          V.sieve n $ V.drop offset chunk)) 0 .
   chunks

{-# INLINE deinterleave #-}
deinterleave :: (Storable a) => Int -> Vector a -> [Vector a]
deinterleave n =
   P.map (sieve n) . P.take n . P.iterate (switchL empty (flip const))

{- |
Interleave lazy vectors
while maintaining the chunk pattern of the first vector.
All input vectors must have the same length.
-}
{-# INLINE interleaveFirstPattern #-}
interleaveFirstPattern, _interleaveFirstPattern ::
   (Storable a) => [Vector a] -> Vector a
interleaveFirstPattern [] = empty
interleaveFirstPattern vss@(vs:_) =
   let pattern = List.map V.length $ chunks vs
       split xs =
          snd $
          List.mapAccumL
             (\x n -> swap $ mapFst (V.concat . chunks) $ splitAt n x)
             xs pattern
   in  fromChunks $ List.map V.interleave $
       List.transpose $ List.map split vss

_interleaveFirstPattern [] = empty
_interleaveFirstPattern vss@(vs:_) =
   fromChunks . snd .
   List.mapAccumL
      (\xss n ->
         swap $
         mapFst (V.interleave . List.map (V.concat . chunks)) $
         List.unzip $ List.map (splitAt n) xss)
      vss .
   List.map V.length . chunks $ vs



{- |
Ensure a minimal length of the list by appending pad values.
-}
{- disabled INLINE pad -}
pad :: (Storable a) => ChunkSize -> a -> Int -> Vector a -> Vector a
pad size y n0 =
   let recourse n xt =
          if n<=0
            then xt
            else
              case xt of
                 [] -> chunks $ replicate size n y
                 x:xs -> x : recourse (n - V.length x) xs
   in  SV . recourse n0 . chunks

{-# WARNING padAlt "use 'pad' instead" #-}
padAlt :: (Storable a) => ChunkSize -> a -> Int -> Vector a -> Vector a
padAlt size x n xs =
   append xs
      (let m = length xs
       in  if n>m
             then replicate size (n-m) x
             else empty)

compact :: (Storable a) => ChunkSize -> Vector a -> Vector a
compact size (SV xs) =
   SV $ List.map V.concat $
   compactGen
      (\x y -> mfilter (<=size) $ Just $ mappend x y)
      (ChunkSize . V.length) xs

compactGen :: (b -> b -> Maybe b) -> (a -> b) -> [a] -> [[a]]
compactGen _ _ [] = []
compactGen plus measure (x0:xs0) =
   uncurry (:) $ mapFst (x0:) $
   List.foldr
      (\y go s0 ->
         let ym = measure y
         in  case plus s0 ym of
               Just s1 -> mapFst (y:) $ go s1
               Nothing -> ([], uncurry (:) $ mapFst (y:) $ go ym))
      (const ([], [])) xs0 (measure x0)




-- * Helper functions for StorableVector


{-# WARNING cancelNullVector "do not use it" #-}
{-# INLINE cancelNullVector #-}
cancelNullVector :: (V.Vector a, b) -> Maybe (V.Vector a, b)
cancelNullVector y =
   toMaybe (not (V.null (fst y))) y

-- if the chunk has length zero, an empty sequence is generated
{-# INLINE fromChunk #-}
fromChunk :: (Storable a) => V.Vector a -> Vector a
fromChunk x =
   if V.null x
     then empty
     else SV [x]



{-
reduceLVector :: Storable x =>
   (x -> acc -> Maybe acc) -> acc -> Vector x -> (acc, Bool)
reduceLVector f acc0 x =
   let recourse i acc =
          if i < V.length x
            then (acc, True)
            else
               maybe
                  (acc, False)
                  (recourse (succ i))
                  (f (V.index x i) acc)
   in  recourse 0 acc0




{- * Fundamental functions -}

{-
Usage of 'unfoldr' seems to be clumsy but that covers all cases,
like different block sizes in source and destination list.
-}
crochetLSize :: (Storable x, Storable y) =>
      ChunkSize
   -> (x -> acc -> Maybe (y, acc))
   -> acc
   -> T x
   -> T y
crochetLSize size f =
   curry (unfoldr size (\(acc,xt) ->
      do (x,xs) <- viewL xt
         (y,acc') <- f x acc
         return (y, (acc',xs))))

crochetListL :: (Storable y) =>
      ChunkSize
   -> (x -> acc -> Maybe (y, acc))
   -> acc
   -> [x]
   -> T y
crochetListL size f =
   curry (unfoldr size (\(acc,xt) ->
      do (x,xs) <- ListHT.viewL xt
         (y,acc') <- f x acc
         return (y, (acc',xs))))



{-# NOINLINE [0] crochetFusionListL #-}
crochetFusionListL :: (Storable y) =>
      ChunkSize
   -> (x -> acc -> Maybe (y, acc))
   -> acc
   -> FList.T x
   -> T y
crochetFusionListL size f =
   curry (unfoldr size (\(acc,xt) ->
      do (x,xs) <- FList.viewL xt
         (y,acc') <- f x acc
         return (y, (acc',xs))))


{-# INLINE [0] reduceL #-}
reduceL :: Storable x =>
   (x -> acc -> Maybe acc) -> acc -> Vector x -> acc
reduceL f acc0 =
   let recourse acc xt =
          case xt of
             [] -> acc
             (x:xs) ->
                 let (acc',continue) = reduceLVector f acc x
                 in  if continue
                       then recourse acc' xs
                       else acc'
   in  recourse acc0 . chunks



{- * Basic functions -}


{-# RULEZ
  "Storable.append/repeat/repeat" forall size x.
      append (repeat size x) (repeat size x) = repeat size x ;

  "Storable.append/repeat/replicate" forall size n x.
      append (repeat size x) (replicate size n x) = repeat size x ;

  "Storable.append/replicate/repeat" forall size n x.
      append (replicate size n x) (repeat size x) = repeat size x ;

  "Storable.append/replicate/replicate" forall size n m x.
      append (replicate size n x) (replicate size m x) =
         replicate size (n+m) x ;

  "Storable.mix/repeat/repeat" forall size x y.
      mix (repeat size x) (repeat size y) = repeat size (x+y) ;

  #-}

{-# RULES
  "Storable.length/cons" forall x xs.
      length (cons x xs) = 1 + length xs ;

  "Storable.length/map" forall f xs.
      length (map f xs) = length xs ;

  "Storable.map/cons" forall f x xs.
      map f (cons x xs) = cons (f x) (map f xs) ;

  "Storable.map/repeat" forall size f x.
      map f (repeat size x) = repeat size (f x) ;

  "Storable.map/replicate" forall size f x n.
      map f (replicate size n x) = replicate size n (f x) ;

  "Storable.map/repeat" forall size f x.
      map f (repeat size x) = repeat size (f x) ;

  {-
  This can make things worse, if 'map' is applied to replicate,
  since this can use of sharing.
  It can also destroy the more important map/unfoldr fusion in
    take n . map f . unfoldr g

  "Storable.take/map" forall n f x.
      take n (map f x) = map f (take n x) ;
  -}

  "Storable.take/repeat" forall size n x.
      take n (repeat size x) = replicate size n x ;

  "Storable.take/take" forall n m xs.
      take n (take m xs) = take (min n m) xs ;

  "Storable.drop/drop" forall n m xs.
      drop n (drop m xs) = drop (n+m) xs ;

  "Storable.drop/take" forall n m xs.
      drop n (take m xs) = take (max 0 (m-n)) (drop n xs) ;

  "Storable.map/mapAccumL/snd" forall g f acc0 xs.
      map g (snd (mapAccumL f acc0 xs)) =
         snd (mapAccumL (\acc a -> mapSnd g (f acc a)) acc0 xs) ;

  #-}

{- GHC says this is an orphaned rule
  "Storable.map/mapAccumL/mapSnd" forall g f acc0 xs.
      mapSnd (map g) (mapAccumL f acc0 xs) =
         mapAccumL (\acc a -> mapSnd g (f acc a)) acc0 xs ;
-}


{- * Fusable functions -}

scanLCrochet :: (Storable a, Storable b) =>
   (a -> b -> a) -> a -> Vector b -> Vector a
scanLCrochet f start =
   cons start .
   crochetL (\x acc -> let y = f acc x in Just (y, y)) start

{-# INLINE mapCrochet #-}
mapCrochet :: (Storable a, Storable b) => (a -> b) -> (Vector a -> Vector b)
mapCrochet f = crochetL (\x _ -> Just (f x, ())) ()

{-# INLINE takeCrochet #-}
takeCrochet :: Storable a => Int -> Vector a -> Vector a
takeCrochet = crochetL (\x n -> toMaybe (n>0) (x, pred n))

{-# INLINE repeatUnfoldr #-}
repeatUnfoldr :: Storable a => ChunkSize -> a -> Vector a
repeatUnfoldr size = iterate size id

{-# INLINE replicateCrochet #-}
replicateCrochet :: Storable a => ChunkSize -> Int -> a -> Vector a
replicateCrochet size n = takeCrochet n . repeat size




{-
The "fromList/drop" rule is not quite accurate
because the chunk borders are moved.
Maybe 'ChunkSize' better is a list of chunks sizes.
-}

{-# RULEZ
  "fromList/zipWith"
    forall size f (as :: Storable a => [a]) (bs :: Storable a => [a]).
     fromList size (List.zipWith f as bs) =
        zipWith size f (fromList size as) (fromList size bs) ;

  "fromList/drop" forall as n size.
     fromList size (List.drop n as) =
        drop n (fromList size as) ;
  #-}



{- * Fused functions -}

type Unfoldr s a = (s -> Maybe (a,s), s)

{-# INLINE zipWithUnfoldr2 #-}
zipWithUnfoldr2 :: Storable z =>
      ChunkSize
   -> (x -> y -> z)
   -> Unfoldr a x
   -> Unfoldr b y
   -> T z
zipWithUnfoldr2 size h (f,a) (g,b) =
   unfoldr size
      (\(a0,b0) -> liftM2 (\(x,a1) (y,b1) -> (h x y, (a1,b1))) (f a0) (g b0))
--      (uncurry (liftM2 (\(x,a1) (y,b1) -> (h x y, (a1,b1)))) . (f *** g))
      (a,b)

{- done by takeCrochet
{-# INLINE mapUnfoldr #-}
mapUnfoldr :: (Storable x, Storable y) =>
      ChunkSize
   -> (x -> y)
   -> Unfoldr a x
   -> T y
mapUnfoldr size g (f,a) =
   unfoldr size (fmap (mapFst g) . f) a
-}

{-# INLINE dropUnfoldr #-}
dropUnfoldr :: Storable x =>
      ChunkSize
   -> Int
   -> Unfoldr a x
   -> T x
dropUnfoldr size n (f,a0) =
   maybe
      empty
      (unfoldr size f)
      (nest n (\a -> fmap snd . f =<< a) (Just a0))


{- done by takeCrochet
{-# INLINE takeUnfoldr #-}
takeUnfoldr :: Storable x =>
      ChunkSize
   -> Int
   -> Unfoldr a x
   -> T x
takeUnfoldr size n0 (f,a0) =
   unfoldr size
      (\(a,n) ->
         do guard (n>0)
            (x,a') <- f a
            return (x, (a', pred n)))
      (a0,n0)
-}


lengthUnfoldr :: Storable x =>
      Unfoldr a x
   -> Int
lengthUnfoldr (f,a0) =
   let recourse n a =
          maybe n (recourse (succ n) . snd) (f a)
   in  recourse 0 a0


{-# INLINE zipWithUnfoldr #-}
zipWithUnfoldr ::
   (Storable b, Storable c) =>
      (acc -> Maybe (a, acc))
   -> (a -> b -> c)
   -> acc
   -> T b -> T c
zipWithUnfoldr f h a y =
   crochetL (\y0 a0 ->
       do (x0,a1) <- f a0
          Just (h x0 y0, a1)) a y

{-# INLINE zipWithCrochetL #-}
zipWithCrochetL ::
   (Storable x, Storable b, Storable c) =>
      ChunkSize
   -> (x -> acc -> Maybe (a, acc))
   -> (a -> b -> c)
   -> acc
   -> T x -> T b -> T c
zipWithCrochetL size f h a x y =
   crochetL (\(x0,y0) a0 ->
       do (z0,a1) <- f x0 a0
          Just (h z0 y0, a1))
      a (zip size x y)


{-# INLINE crochetLCons #-}
crochetLCons ::
   (Storable a, Storable b) =>
      (a -> acc -> Maybe (b, acc))
   -> acc
   -> a -> T a -> T b
crochetLCons f a0 x xs =
   maybe
      empty
      (\(y,a1) -> cons y (crochetL f a1 xs))
      (f x a0)

{-# INLINE reduceLCons #-}
reduceLCons ::
   (Storable a) =>
      (a -> acc -> Maybe acc)
   -> acc
   -> a -> T a -> acc
reduceLCons f a0 x xs =
   maybe a0 (flip (reduceL f) xs) (f x a0)





{-# RULES
  "Storable.zipWith/share" forall size (h :: a->a->b) (x :: T a).
     zipWith size h x x = map (\xi -> h xi xi) x ;

--  "Storable.map/zipWith" forall size (f::c->d) (g::a->b->c) (x::T a) (y::T b).
  "Storable.map/zipWith" forall size f g x y.
     map f (zipWith size g x y) =
        zipWith size (\xi yi -> f (g xi yi)) x y ;

  -- this rule lets map run on a different block structure
  "Storable.zipWith/map,*" forall size f g x y.
     zipWith size g (map f x) y =
        zipWith size (\xi yi -> g (f xi) yi) x y ;

  "Storable.zipWith/*,map" forall size f g x y.
     zipWith size g x (map f y) =
        zipWith size (\xi yi -> g xi (f yi)) x y ;


  "Storable.drop/unfoldr" forall size f a n.
     drop n (unfoldr size f a) =
        dropUnfoldr size n (f,a) ;

  "Storable.take/unfoldr" forall size f a n.
     take n (unfoldr size f a) =
--        takeUnfoldr size n (f,a) ;
        takeCrochet n (unfoldr size f a) ;

  "Storable.length/unfoldr" forall size f a.
     length (unfoldr size f a) = lengthUnfoldr (f,a) ;

  "Storable.map/unfoldr" forall size g f a.
     map g (unfoldr size f a) =
--        mapUnfoldr size g (f,a) ;
        mapCrochet g (unfoldr size f a) ;

  "Storable.map/iterate" forall size g f a.
     map g (iterate size f a) =
        mapCrochet g (iterate size f a) ;

{-
  "Storable.zipWith/unfoldr,unfoldr" forall sizeA sizeB f g h a b n.
     zipWith n h (unfoldr sizeA f a) (unfoldr sizeB g b) =
        zipWithUnfoldr2 n h (f,a) (g,b) ;
-}

  -- block boundaries are changed here, so it changes lazy behaviour
  "Storable.zipWith/unfoldr,*" forall sizeA sizeB f h a y.
     zipWith sizeA h (unfoldr sizeB f a) y =
        zipWithUnfoldr f h a y ;

  -- block boundaries are changed here, so it changes lazy behaviour
  "Storable.zipWith/*,unfoldr" forall sizeA sizeB f h a y.
     zipWith sizeA h y (unfoldr sizeB f a) =
        zipWithUnfoldr f (flip h) a y ;

  "Storable.crochetL/unfoldr" forall size f g a b.
     crochetL g b (unfoldr size f a) =
        unfoldr size (\(a0,b0) ->
            do (y0,a1) <- f a0
               (z0,b1) <- g y0 b0
               Just (z0, (a1,b1))) (a,b) ;

  "Storable.reduceL/unfoldr" forall size f g a b.
     reduceL g b (unfoldr size f a) =
        snd
          (FList.recourse (\(a0,b0) ->
              do (y,a1) <- f a0
                 b1 <- g y b0
                 Just (a1, b1)) (a,b)) ;

  "Storable.crochetL/cons" forall g b x xs.
     crochetL g b (cons x xs) =
        crochetLCons g b x xs ;

  "Storable.reduceL/cons" forall g b x xs.
     reduceL g b (cons x xs) =
        reduceLCons g b x xs ;




  "Storable.take/crochetL" forall f a x n.
     take n (crochetL f a x) =
        takeCrochet n (crochetL f a x) ;

  "Storable.length/crochetL" forall f a x.
     length (crochetL f a x) = length x ;

  "Storable.map/crochetL" forall g f a x.
     map g (crochetL f a x) =
        mapCrochet g (crochetL f a x) ;

  "Storable.zipWith/crochetL,*" forall size f h a x y.
     zipWith size h (crochetL f a x) y =
        zipWithCrochetL size f h a x y ;

  "Storable.zipWith/*,crochetL" forall size f h a x y.
     zipWith size h y (crochetL f a x) =
        zipWithCrochetL size f (flip h) a x y ;

  "Storable.crochetL/crochetL" forall f g a b x.
     crochetL g b (crochetL f a x) =
        crochetL (\x0 (a0,b0) ->
            do (y0,a1) <- f x0 a0
               (z0,b1) <- g y0 b0
               Just (z0, (a1,b1))) (a,b) x ;

  "Storable.reduceL/crochetL" forall f g a b x.
     reduceL g b (crochetL f a x) =
        snd
          (reduceL (\x0 (a0,b0) ->
              do (y,a1) <- f x0 a0
                 b1 <- g y b0
                 Just (a1, b1)) (a,b) x) ;
  #-}

-}

{- * IO -}

{- |
Read the rest of a file lazily and
provide the reason of termination as IOError.
If IOError is EOF (check with @System.Error.isEOFError err@),
then the file was read successfully.
Only access the final IOError after you have consumed the file contents,
since finding out the terminating reason forces to read the entire file.
Make also sure you read the file completely,
because it is only closed when the file end is reached
(or an exception is encountered).

TODO:
In ByteString.Lazy the chunk size is reduced
if data is not immediately available.
Maybe we should adapt that behaviour
but when working with realtime streams
that may mean that the chunks are very small.
-}
{-
ToDo:
It might be better to let the user close the file manually
after he finished processing the file.
-}
hGetContentsAsync :: Storable a =>
   ChunkSize -> Handle -> IO (IOError, Vector a)
hGetContentsAsync (ChunkSize size) h =
   let go =
          Unsafe.interleaveIO $
          flip catch (\err -> return (err,[])) $
          do v <- V.hGet h size
             if V.null v
               then hClose h >>
                    return (Exc.mkIOError Exc.eofErrorType
                      "StorableVector.Lazy.hGetContentsAsync" (Just h) Nothing, [])
               else fmap (mapSnd (v:)) go
{-
          Unsafe.interleaveIO $
          flip catch (\err -> return (err,[])) $
          liftM2 (\ chunk ~(err,rest) -> (err,chunk:rest))
             (V.hGet h size) go
-}
   in  fmap (mapSnd SV) go

hGetContentsSync :: Storable a =>
   ChunkSize -> Handle -> IO (Vector a)
hGetContentsSync (ChunkSize size) h =
   let go =
          do v <- V.hGet h size
             if V.null v
               then return []
               else fmap (v:) go
   in  fmap SV go

hPut :: Storable a => Handle -> Vector a -> IO ()
hPut h = mapM_ (V.hPut h) . chunks

{-
*Data.StorableVector.Lazy> print . mapSnd (length :: Vector Data.Int.Int16 -> Int) =<< readFileAsync (ChunkSize 1000) "dist/build/libHSstorablevector-0.1.3.a"
(dist/build/libHSstorablevector-0.1.3.a: hGetBuf: illegal operation (handle is closed),0)
-}
{- |
The file can only closed after all values are consumed.
That is you must always assert that you consume all elements of the stream,
and that no values are missed due to lazy evaluation.
This requirement makes this function useless in many applications.
-}
readFileAsync :: Storable a => ChunkSize -> FilePath -> IO (IOError, Vector a)
readFileAsync size path =
   openBinaryFile path ReadMode >>= hGetContentsAsync size

writeFile :: Storable a => FilePath -> Vector a -> IO ()
writeFile path =
   bracket (openBinaryFile path WriteMode) hClose . flip hPut

appendFile :: Storable a => FilePath -> Vector a -> IO ()
appendFile path =
   bracket (openBinaryFile path AppendMode) hClose . flip hPut

interact :: Storable a => ChunkSize -> (Vector a -> Vector a) -> IO ()
interact (ChunkSize size) f =
   let -- almost duplicate of hGetContentsSync
       hGetContents h =
          let go =
                 Unsafe.interleaveIO $
                 do v <- V.hGet h size
                    if V.null v
                      then return []
                      else fmap (v:) go
          in  go
   in  mapM_ (V.hPut IO.stdout) . chunks . f . SV =<< hGetContents IO.stdin



{-# NOINLINE moduleError #-}
moduleError :: String -> String -> a
moduleError fun msg =
   error ("Data.StorableVector.Lazy." List.++ fun List.++ ':':' ':msg)
