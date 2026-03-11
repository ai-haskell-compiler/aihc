{-# OPTIONS_GHC -fno-warn-orphans #-}
--
-- Module      : StorableVector
-- Copyright   : (c) The University of Glasgow 2001,
--               (c) David Roundy 2003-2005,
--               (c) Simon Marlow 2005
--               (c) Don Stewart 2005-2006
--               (c) Bjorn Bringert 2006
--               (c) Spencer Janssen 2006
--               (c) Henning Thielemann 2008-2017
--
--
-- License     : BSD-style
--
-- Maintainer  : Henning Thielemann
-- Stability   : experimental
-- Portability : portable, requires ffi and cpp
-- Tested with : GHC 6.4.1 and Hugs March 2005
--

--
-- | A time and space-efficient implementation of vectors using
-- packed arrays, suitable for high performance use, both in terms
-- of large data quantities, or high speed requirements. Vectors
-- are encoded as strict arrays, held in a 'ForeignPtr',
-- and can be passed between C and Haskell with little effort.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.StorableVector as V
--
-- Original GHC implementation by Bryan O\'Sullivan. Rewritten to use
-- UArray by Simon Marlow. Rewritten to support slices and use
-- ForeignPtr by David Roundy. Polished and extended by Don Stewart.
-- Generalized to any Storable value by Spencer Janssen.
-- Chunky lazy stream, also with chunk pattern control,
-- mutable access in ST monad, Builder monoid by Henning Thieleman.

module Data.StorableVector (

        -- * The @Vector@ type
        Vector,

        -- * Introducing and eliminating 'Vector's
        empty,
        singleton,
        pack,
        unpack,
        packN,
        packWith,
        unpackWith,

        -- * Basic interface
        cons,
        snoc,
        append,
        head,
        last,
        tail,
        init,
        null,
        length,
        viewL,
        viewR,
        switchL,
        switchR,

        -- * Transforming 'Vector's
        map,
        mapIndexed,
        reverse,
        intersperse,
        transpose,

        -- * Reducing 'Vector's (folds)
        foldl,
        foldl',
        foldl1,
        foldl1',
        foldr,
        foldr1,

        -- ** Special folds
        concat,
        concatMap,
        foldMap,
        monoidConcatMap,
        any,
        all,
        maximum,
        minimum,

        -- * Building 'Vector's
        -- ** Scans
        scanl,
        scanl1,
        scanr,
        scanr1,

        -- ** Accumulating maps
        mapAccumL,
        mapAccumR,
        crochetL,
        crochetLResult,

        -- ** Unfolding 'Vector's
        replicate,
        iterateN,
        unfoldr,
        unfoldrN,
        unfoldrResultN,
        sample,

        -- * Substrings

        -- ** Breaking strings
        take,
        drop,
        splitAt,
        takeWhile,
        dropWhile,
        span,
        spanEnd,
        break,
        breakEnd,
        group,
        groupBy,
        inits,
        tails,

        -- ** Breaking into many substrings
        split,
        splitWith,
        tokens,
        sliceVertical,

        -- ** Joining strings
        join,

        -- * Predicates
        isPrefixOf,
        isSuffixOf,

        -- * Searching 'Vector's

        -- ** Searching by equality
        elem,
        notElem,

        -- ** Searching with a predicate
        find,
        filter,

        -- * Indexing 'Vector's
        index,
        elemIndex,
        elemIndices,
        elemIndexEnd,
        findIndex,
        findIndices,
        count,
        findIndexOrEnd,

        -- * Zipping and unzipping 'Vector's
        zip,
        zipWith,
        zipWith3,
        zipWith4,
        unzip,
        copy,

        -- * Interleaved 'Vector's
        sieve,
        deinterleave,
        interleave,

        -- * IO
        poke,
        peek,
        hGet,
        hPut,
        readFile,
        writeFile,
        appendFile,

  ) where

import Data.StorableVector.Base

import qualified System.Unsafe as Unsafe

import Control.Exception        (assert, bracket, )
import System.IO                (IO, FilePath, Handle, IOMode(..),
                                 openBinaryFile, hClose, hFileSize,
                                 hGetBuf, hPutBuf, )

import qualified Foreign.Storable as St
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr, )
import Foreign.Marshal.Array    (advancePtr, copyArray, withArray, )
import Foreign.Ptr              (Ptr, minusPtr, )
import Foreign.Storable         (Storable(sizeOf, alignment,
                                          pokeElemOff, peekElemOff), )

import qualified Test.QuickCheck as QC

import qualified Control.Monad.Trans.Cont as MC
import Control.Monad            (mplus, guard, when, liftM2, liftM3, liftM4,
                                 mapM, sequence_, return, (=<<), (>>=), (>>), )
import Data.Functor             (fmap, )
import Data.Monoid              (Monoid, mempty, mappend, mconcat, )
import Data.Semigroup           (Semigroup, (<>), )

import qualified Data.List as List
import qualified Data.List.HT as ListHT
import qualified Data.Strictness.HT as Strict
import Text.Show (show, )
import Data.Function (flip, id, const, ($), (.), )
import Data.List (and, (++), )
import Data.Tuple.HT (mapSnd, )
import Data.Tuple (uncurry, curry, fst, snd, )
import Data.Either (Either(Left, Right), )
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (Maybe(Just, Nothing), maybe, fromMaybe, isJust, )
import Data.Bool (Bool(False, True), not, otherwise, (&&), (||), )
import Data.Ord (Ord, min, max, (<), (<=), (>), (>=), )
import Data.Eq (Eq, (==), (/=), )

import qualified Prelude as P
import Prelude
          (String, Int, (*), (-), (+), div, mod,
           fromIntegral, error, undefined, )


-- -----------------------------------------------------------------------------

instance (Storable a, Eq a) => Eq (Vector a) where
    (==) = equal

instance (Storable a) => Semigroup (Vector a) where
    (<>) = append

instance (Storable a) => Monoid (Vector a) where
    mempty  = empty
    mappend = append
    mconcat = concat

instance (Storable a, QC.Arbitrary a) => QC.Arbitrary (Vector a) where
    arbitrary = pack `fmap` QC.arbitrary

-- | /O(n)/ Equality on the 'Vector' type.
equal :: (Storable a, Eq a) => Vector a -> Vector a -> Bool
equal a b =
   Unsafe.performIO $
   withStartPtr a $ \paf la ->
   withStartPtr b $ \pbf lb ->
    if la /= lb
      then
        return False
      else
        if paf == pbf
          then return True
          else
            let go = Strict.arguments3 $ \p q l ->
                   if l==0
                     then return True
                     else
                       do x <- St.peek p
                          y <- St.peek q
                          if x==y
                            then go (incPtr p) (incPtr q) (l-1)
                            else return False
            in  go paf pbf la
{-# INLINE equal #-}

-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'Vector's

-- | /O(1)/ The empty 'Vector'
empty :: (Storable a) => Vector a
empty = unsafeCreate 0 $ const $ return ()
{-# NOINLINE empty #-}

-- | /O(1)/ Construct a 'Vector' containing a single element
singleton :: (Storable a) => a -> Vector a
singleton c = unsafeCreate 1 $ \p -> St.poke p c
{-# INLINE singleton #-}

-- | /O(n)/ Convert a '[a]' into a 'Vector a'.
--
pack :: (Storable a) => [a] -> Vector a
pack str = unsafeCreate (P.length str) $ \p -> go p str
    where
      go = Strict.arguments2 $ \p ->
        ListHT.switchL
           (return ())
           (\x xs -> St.poke p x >> go (incPtr p) xs)

-- | /O(n)/ Convert first @n@ elements of a '[a]' into a 'Vector a'.
--
packN :: (Storable a) => Int -> [a] -> (Vector a, [a])
packN n =
   mapSnd (fromMaybe []) . unfoldrN n ListHT.viewL

-- | /O(n)/ Converts a 'Vector a' to a '[a]'.
unpack :: (Storable a) => Vector a -> [a]
unpack = foldr (:) []
{-# INLINE unpack #-}

------------------------------------------------------------------------

-- | /O(n)/ Convert a list into a 'Vector' using a conversion function
packWith :: (Storable b) => (a -> b) -> [a] -> Vector b
packWith k str = unsafeCreate (P.length str) $ \p -> go p str
    where
      go = Strict.arguments2 $ \p ->
        ListHT.switchL
           (return ())
           (\x xs -> St.poke p (k x) >> go (incPtr p) xs)
                          -- less space than pokeElemOff
{-# INLINE packWith #-}

{-
*Data.StorableVector> List.take 10 $ unpackWith id $ pack [0..10000000::Int]
[0,1,2,3,4,5,6,7,8,9]
(19.18 secs, 2327851592 bytes)
-}
-- | /O(n)/ Convert a 'Vector' into a list using a conversion function
unpackWith :: (Storable a) => (a -> b) -> Vector a -> [b]
unpackWith f = foldr ((:) . f) []
{-# INLINE unpackWith #-}

{-
That's too inefficient, since it builds the list from back to front,
that is, in a too strict manner.

-- | /O(n)/ Convert a 'Vector' into a list using a conversion function
unpackWith :: (Storable a) => (a -> b) -> Vector a -> [b]
unpackWith _ (SV _  _ 0) = []
unpackWith k v@(SV ps s l) = inlinePerformIO $ withStartPtr v $ \p ->
        go p (l - 1) []
    where
        STRICT3(go)
        go p 0 acc = St.peek p          >>= \e -> return (k e : acc)
        go p n acc = peekElemOff p n >>= \e -> go p (n-1) (k e : acc)
{-# INLINE unpackWith #-}


*Data.StorableVector> List.take 10 $ unpack $ pack [0..10000000::Int]
[0,1,2,3,4,5,6,7,8,9]
(18.57 secs, 2323959948 bytes)
*Data.StorableVector> unpack $ take 10 $ pack [0..10000000::Int]
[0,1,2,3,4,5,6,7,8,9]
(18.40 secs, 2324002120 bytes)
*Data.StorableVector> List.take 10 $ unpackWith id $ pack [0..10000000::Int]
Interrupted.
-}

-- ---------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ Test whether a 'Vector' is empty.
null :: Vector a -> Bool
null (SV _ _ l) = assert (l >= 0) $ l <= 0
{-# INLINE null #-}

-- ---------------------------------------------------------------------
-- | /O(1)/ 'length' returns the length of a 'Vector' as an 'Int'.
length :: Vector a -> Int
length (SV _ _ l) = assert (l >= 0) $ l

--
-- length/loop fusion. When taking the length of any fuseable loop,
-- rewrite it as a foldl', and thus avoid allocating the result buffer
-- worth around 10% in speed testing.
--

{-# INLINE [1] length #-}

------------------------------------------------------------------------

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires a memcpy.
cons :: (Storable a) => a -> Vector a -> Vector a
cons c v =
   unsafeWithStartPtr v $ \f l ->
   create (l + 1) $ \p -> do
      St.poke p c
      copyArray (incPtr p) f (fromIntegral l)
{-# INLINE cons #-}

-- | /O(n)/ Append an element to the end of a 'Vector'
snoc :: (Storable a) => Vector a -> a -> Vector a
snoc v c =
   unsafeWithStartPtr v $ \f l ->
   create (l + 1) $ \p -> do
      copyArray p f l
      pokeElemOff p l c
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a 'Vector', which must be non-empty.
-- It is a checked error to pass an empty 'Vector'.
head :: (Storable a) => Vector a -> a
head =
   withNonEmptyVector "head" $ \ p s _l -> foreignPeek p s
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a 'Vector', which must be non-empty.
-- It is a checked error to pass an empty 'Vector'.
tail :: (Storable a) => Vector a -> Vector a
tail =
   withNonEmptyVector "tail" $ \ p s l -> SV p (s+1) (l-1)
{-# INLINE tail #-}

laxTail :: (Storable a) => Vector a -> Vector a
laxTail v@(SV fp s l) =
   if l<=0
     then v
     else SV fp (s+1) (l-1)
{-# INLINE laxTail #-}

-- | /O(1)/ Extract the last element of a 'Vector', which must be finite and non-empty.
-- It is a checked error to pass an empty 'Vector'.
last :: (Storable a) => Vector a -> a
last =
   withNonEmptyVector "last" $ \ p s l -> foreignPeek p (s+l-1)
{-# INLINE last #-}

-- | /O(1)/ Return all the elements of a 'Vector' except the last one.
-- It is a checked error to pass an empty 'Vector'.
init :: Vector a -> Vector a
init =
   withNonEmptyVector "init" $ \ p s l -> SV p s (l-1)
{-# INLINE init #-}

-- | /O(n)/ Append two Vectors
append :: (Storable a) => Vector a -> Vector a -> Vector a
append xs ys = concat [xs,ys]
{-# INLINE append #-}

-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the 'Vector' obtained by applying @f@ to each
-- element of @xs@.
map :: (Storable a, Storable b) => (a -> b) -> Vector a -> Vector b
map f v =
   unsafeWithStartPtr v $ \a len ->
   create len $ \p ->
      let go = Strict.arguments3 $
             \ n p1 p2 ->
               when (n>0) $
                 do St.poke p2 . f =<< St.peek p1
                    go (n-1) (incPtr p1) (incPtr p2)
      in  go len a p
{-# INLINE map #-}

{-
mapByIndex :: (Storable a, Storable b) => (a -> b) -> Vector a -> Vector b
mapByIndex f v = inlinePerformIO $ withStartPtr v $ \a len ->
    create len $ \p2 ->
       let go = Strict.arguments1 $ \ n ->
              when (n<len) $
                do pokeElemOff p2 n . f =<< peekElemOff a n
                   go (n+1)
       in  go 0
-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: (Storable a) => Vector a -> Vector a
reverse v =
   unsafeWithStartPtr v $ \f l ->
   create l $ \p ->
   sequence_ [peekElemOff f i >>= pokeElemOff p (l - i - 1)
                 | i <- [0 .. l - 1]]

-- | /O(n)/ The 'intersperse' function takes a element and a
-- 'Vector' and \`intersperses\' that element between the elements of
-- the 'Vector'.  It is analogous to the intersperse function on
-- Lists.
intersperse :: (Storable a) => a -> Vector a -> Vector a
intersperse c = pack . List.intersperse c . unpack

-- | The 'transpose' function transposes the rows and columns of its
-- 'Vector' argument.
transpose :: (Storable a) => [Vector a] -> [Vector a]
transpose ps = P.map pack (List.transpose (P.map unpack ps))

-- ---------------------------------------------------------------------
-- Reducing 'Vector's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a Vector, reduces the
-- 'Vector' using the binary operator, from left to right.
foldl :: (Storable a) => (b -> a -> b) -> b -> Vector a -> b
foldl f v xs =
   foldr (\x k acc -> k (f acc x)) id xs v
{-# INLINE foldl #-}

-- | 'foldl\'' is like 'foldl', but strict in the accumulator.
foldl' :: (Storable a) => (b -> a -> b) -> b -> Vector a -> b
foldl' f b v =
   Unsafe.performIO $ withStartPtr v $ \ptr l ->
      let q  = ptr `advancePtr` l
          go = Strict.arguments2 $ \p z ->
             if p == q
               then return z
               else go (incPtr p) . f z =<< St.peek p
      in  go ptr b
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a 'Vector',
-- reduces the 'Vector' using the binary operator, from right to left.
-- However, it is not the same as 'foldl' applied to the reversed vector.
-- Actually 'foldr' starts processing with the first element,
-- and thus can be used for efficiently building a singly linked list
-- by @foldr (:) [] vec@.
-- Unfortunately 'foldr' is quite slow for low-level loops,
-- since GHC (up to 6.12.1) cannot detect the loop.
foldr :: (Storable a) => (a -> b -> b) -> b -> Vector a -> b
foldr = foldrByLoop
{-# INLINE foldr #-}

{-
*Data.StorableVector> List.length $ foldrBySwitch (:) [] $ replicate 1000000 'a'
1000000
(11.29 secs, 1183476300 bytes)
*Data.StorableVector> List.length $ foldrByIO (:) [] $ replicate 1000000 'a'
1000000
(7.86 secs, 1033901140 bytes)
*Data.StorableVector> List.length $ foldrByIndex (:) [] $ replicate 1000000 'a'
1000000
(7.86 secs, 914340420 bytes)
*Data.StorableVector> List.length $ foldrByLoop (:) [] $ replicate 1000000 'a'
1000000
(6.38 secs, 815355460 bytes)
-}
{-
We cannot simply increment the pointer,
since ForeignPtr cannot be incremented.
We also cannot convert from ForeignPtr to Ptr
and increment that instead,
because we need to keep the reference to ForeignPtr,
otherwise memory might be freed.
We can also not perform loop entirely in strict IO,
since this eat up the stack quickly
and 'foldr' might be used to build a list lazily.
-}
foldrByLoop :: (Storable a) => (a -> b -> b) -> b -> Vector a -> b
foldrByLoop f z (SV fp s l) =
   let end = s+l
       go = Strict.arguments1 $ \k ->
          if k<end
            then f (foreignPeek fp k) (go (succ k))
            else z
   in  go s
{-# INLINE foldrByLoop #-}

{-
foldrByIO :: (Storable a) => (a -> b -> b) -> b -> Vector a -> b
foldrByIO f z v@(SV fp _ _) =
   unsafeWithStartPtr v $
   let go = Strict.arguments2 $ \p l ->
          Unsafe.interleaveIO $
          if l>0
            then liftM2 f (St.peek p) (go (incPtr p) (pred l))
            else touchForeignPtr fp >> return z
   in  go
{-# INLINE foldrByIO #-}

foldrByIndex :: (Storable a) => (a -> b -> b) -> b -> Vector a -> b
foldrByIndex k z xs =
   let recourse n =
          if n < length xs
            then k (unsafeIndex xs n) (recourse (succ n))
            else z
   in  recourse 0
{-# INLINE foldrByIndex #-}

{-
This implementation is a bit inefficient,
since switchL creates a new Vector structure
instead of just incrementing an index.
-}
foldrBySwitch :: (Storable a) => (a -> b -> b) -> b -> Vector a -> b
foldrBySwitch k z =
   let recourse = switchL z (\h t -> k h (recourse t))
   in  recourse
{-# INLINE foldrBySwitch #-}
-}


-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'Vector's.
-- It is a checked error to pass an empty 'Vector'.
foldl1 :: (Storable a) => (a -> a -> a) -> Vector a -> a
foldl1 f =
   switchL
      (errorEmpty "foldl1")
      (foldl f)
{-# INLINE foldl1 #-}

-- | 'foldl1\'' is like 'foldl1', but strict in the accumulator.
-- It is a checked error to pass an empty 'Vector'.
foldl1' :: (Storable a) => (a -> a -> a) -> Vector a -> a
foldl1' f =
   switchL
      (errorEmpty "foldl1'")
      (foldl' f)
{-# INLINE foldl1' #-}

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'Vector's
-- It is a checked error to pass an empty 'Vector'.
foldr1 :: (Storable a) => (a -> a -> a) -> Vector a -> a
foldr1 f =
   switchR
      (errorEmpty "foldr1")
      (flip (foldr f))
{-# INLINE foldr1 #-}

-- ---------------------------------------------------------------------
-- Special folds

{-
We filter out empty chunks in order to benefit from the special cases
zero chunks and one chunk.
In the other cases the preprocessing does not help much.
-}
-- | /O(n)/ Concatenate a list of 'Vector's.
concat :: (Storable a) => [Vector a] -> Vector a
concat = concatCore . List.filter (not . null)

concatCore :: (Storable a) => [Vector a] -> Vector a
concatCore []     = empty
concatCore [ps]   = ps
concatCore xs     = unsafeCreate len $ \ptr -> go ptr xs
  where len = P.sum . P.map length $ xs
        go =
          Strict.arguments2 $ \ptr ->
             ListHT.switchL
                (return ())
                (\v ps -> do
                   withStartPtr v $ copyArray ptr
                   go (ptr `advancePtr` length v) ps)

-- | Map a function over a 'Vector' and concatenate the results
concatMap :: (Storable a, Storable b) => (a -> Vector b) -> Vector a -> Vector b
concatMap f = concat . unpackWith f
{-# INLINE concatMap #-}

-- | This is like @mconcat . map f@,
-- but in many cases the result of @f@ will not be storable.
foldMap :: (Storable a, Monoid m) => (a -> m) -> Vector a -> m
foldMap f = foldr (mappend . f) mempty
{-# INLINE foldMap #-}

{-# DEPRECATED monoidConcatMap "Use foldMap instead." #-}
monoidConcatMap :: (Storable a, Monoid m) => (a -> m) -> Vector a -> m
monoidConcatMap = foldMap
{-# INLINE monoidConcatMap #-}

-- | /O(n)/ Applied to a predicate and a 'Vector', 'any' determines if
-- any element of the 'Vector' satisfies the predicate.
any :: (Storable a) => (a -> Bool) -> Vector a -> Bool
any f = foldr ((||) . f) False
{-# INLINE any #-}

-- | /O(n)/ Applied to a predicate and a 'Vector', 'all' determines
-- if all elements of the 'Vector' satisfy the predicate.
all :: (Storable a) => (a -> Bool) -> Vector a -> Bool
all f = foldr ((&&) . f) True
{-# INLINE all #-}

------------------------------------------------------------------------

-- | /O(n)/ 'maximum' returns the maximum value from a 'Vector'
-- This function will fuse.
-- It is a checked error to pass an empty 'Vector'.
maximum :: (Storable a, Ord a) => Vector a -> a
maximum = foldl1' max

-- | /O(n)/ 'minimum' returns the minimum value from a 'Vector'
-- This function will fuse.
-- It is a checked error to pass an empty 'Vector'.
minimum :: (Storable a, Ord a) => Vector a -> a
minimum = foldl1' min

------------------------------------------------------------------------

switchL :: Storable a => b -> (a -> Vector a -> b) -> Vector a -> b
switchL n j x =
   if null x
     then n
     else j (unsafeHead x) (unsafeTail x)
{-# INLINE switchL #-}

switchR :: Storable a => b -> (Vector a -> a -> b) -> Vector a -> b
switchR n j x =
   if null x
     then n
     else j (unsafeInit x) (unsafeLast x)
{-# INLINE switchR #-}

viewL :: Storable a => Vector a -> Maybe (a, Vector a)
viewL = switchL Nothing (curry Just)
{-# INLINE viewL #-}

viewR :: Storable a => Vector a -> Maybe (Vector a, a)
viewR = switchR Nothing (curry Just)
{-# INLINE viewR #-}

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a 'Vector',
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new list.
mapAccumL :: (Storable a, Storable b) => (acc -> a -> (acc, b)) -> acc -> Vector a -> (acc, Vector b)
mapAccumL f acc0 as0 =
   let (bs, Just (acc2, _)) =
          unfoldrN (length as0)
             (\(acc,as) ->
                 fmap
                    (\(asHead,asTail) ->
                        let (acc1,b) = f acc asHead
                        in  (b, (acc1, asTail)))
                    (viewL as))
             (acc0,as0)
   in  (acc2, bs)
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a 'Vector',
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new 'Vector'.
mapAccumR :: (Storable a, Storable b) => (acc -> a -> (acc, b)) -> acc -> Vector a -> (acc, Vector b)
mapAccumR f acc0 as0 =
   let (bs, Just (acc2, _)) =
          unfoldlN (length as0)
             (\(acc,as) ->
                 fmap
                    (\(asInit,asLast) ->
                        let (acc1,b) = f acc asLast
                        in  (b, (acc1, asInit)))
                    (viewR as))
             (acc0,as0)
   in  (acc2, bs)
{-# INLINE mapAccumR #-}

crochetLResult ::
   (Storable x, Storable y) =>
      (x -> acc -> Maybe (y, acc))
   -> acc
   -> Vector x
   -> (Vector y, Maybe acc)
crochetLResult f acc0 x0 =
   mapSnd (fmap fst) $
   unfoldrN
      (length x0)
      (\(acc,xt) ->
         do (x,xs) <- viewL xt
            (y,acc') <- f x acc
            return (y, (acc',xs)))
      (acc0, x0)
{-# INLINE crochetLResult #-}

crochetL ::
   (Storable x, Storable y) =>
      (x -> acc -> Maybe (y, acc))
   -> acc
   -> Vector x
   -> Vector y
crochetL f acc = fst . crochetLResult f acc
{-# INLINE crochetL #-}


-- | /O(n)/ map functions, provided with the index at each position
mapIndexed :: (Storable a, Storable b) => (Int -> a -> b) -> Vector a -> Vector b
mapIndexed f = snd . mapAccumL (\i e -> (i + 1, f i e)) 0
{-# INLINE mapIndexed #-}

-- ---------------------------------------------------------------------
-- Building 'Vector's

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left. This function will fuse.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
scanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
scanl f acc0 as0 =
   fst $
      unfoldrN (succ (length as0))
         (fmap $ \(acc,as) ->
             (acc,
              fmap
                 (\(asHead,asTail) ->
                     (f acc asHead, asTail))
                 (viewL as)))
         (Just (acc0, as0))

-- less efficient but much more comprehensible
-- scanl f z ps =
--   cons z (snd (mapAccumL (\acc a -> let b = f acc a in (b,b)) z ps))

    -- n.b. haskell's List scan returns a list one bigger than the
    -- input, so we need to snoc here to get some extra space, however,
    -- it breaks map/up fusion (i.e. scanl . map no longer fuses)
{-# INLINE scanl #-}

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument.
-- This function will fuse.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Storable a) => (a -> a -> a) -> Vector a -> Vector a
scanl1 f = switchL empty (scanl f)
{-# INLINE scanl1 #-}

-- | scanr is the right-to-left dual of scanl.
scanr :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
scanr f acc0 as0 =
   fst $
      unfoldlN (succ (length as0))
         (fmap $ \(acc,as) ->
             (acc,
              fmap
                 (\(asInit,asLast) ->
                     (f asLast acc, asInit))
                 (viewR as)))
         (Just (acc0, as0))
{-# INLINE scanr #-}

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (Storable a) => (a -> a -> a) -> Vector a -> Vector a
scanr1 f = switchR empty (flip (scanl f))
{-# INLINE scanr1 #-}

-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | /O(n)/ 'replicate' @n x@ is a 'Vector' of length @n@ with @x@
-- the value of every element.
--
{- nice implementation
replicate :: (Storable a) => Int -> a -> Vector a
replicate n c =
   fst $ unfoldrN n (const $ Just (c, ())) ()
-}

{-
fast implementation

Maybe it could be made even faster by plainly copying the bit pattern of the first element.
Since there is no function like 'memset',
we could not warrant that the implementation is really efficient
for the actual machine we run on.
-}
replicate :: (Storable a) => Int -> a -> Vector a
replicate n c =
   if n <= 0
     then empty
     else unsafeCreate n $
       let go = Strict.arguments2 $ \i p ->
              if i == 0
                then return ()
                else St.poke p c >> go (pred i) (incPtr p)
       in  go n
{-# INLINE replicate #-}
{-
For 'replicate 10000000 (42::Int)' generates:

Main_zdwa_info:
	movl (%ebp),%eax
	testl %eax,%eax
	jne .LcfIQ
	movl $ghczmprim_GHCziUnit_Z0T_closure+1,%esi
	addl $8,%ebp
	jmp *(%ebp)
.LcfIQ:
	movl 4(%ebp),%ecx
	movl $42,(%ecx)
	decl %eax
	addl $4,4(%ebp)
	movl %eax,(%ebp)
	jmp Main_zdwa_info

that is, the inner loop consists of 9 instructions,
where I would write something like:
	# counter in %ecx
	testl %ecx
	jz skip_loop
	movl $42,%ebx
start_loop:
	movl %ebx,(%edx)
	addl $4,%edx
	loop start_loop
skip_loop:

and need only 3 instructions in the loop.
-}


-- | /O(n)/ 'iterateN' @n f x@ is a 'Vector' of length @n@
-- where the elements are generated by repeated application of @f@,
-- starting at @x@.
--
iterateN :: (Storable a) => Int -> (a -> a) -> a -> Vector a
iterateN n f =
   fst . unfoldrN n (\a -> Just (a, f a))
{-# INLINE iterateN #-}

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- 'Vector' from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the 'Vector or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next element in the 'Vector',
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
unfoldr :: (Storable b) => (a -> Maybe (b, a)) -> a -> Vector b
unfoldr f = concat . unfoldChunk 32 64
  where unfoldChunk n n' x =
          case unfoldrN n f x of
            (s, mx) -> s : maybe [] (unfoldChunk n' (n+n')) mx
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a 'Vector' from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
unfoldrN :: (Storable b) => Int -> (a -> Maybe (b, a)) -> a -> (Vector b, Maybe a)
unfoldrN n f x0 =
   if n <= 0
     then (empty, Just x0)
     else Unsafe.performIO $ createAndTrim' n $ \p -> go p n x0
       {-
       go must not be strict in the accumulator
       since otherwise packN would be too strict.
       -}
       where
          go = Strict.arguments2 $ \p i -> \x ->
             if i == 0
               then return (0, n-i, Just x)
               else
                 case f x of
                   Nothing     -> return (0, n-i, Nothing)
                   Just (w,x') -> do St.poke p w
                                     go (incPtr p) (i-1) x'
{-# INLINE unfoldrN #-}

{-
Examples:

f i = Just (i::Char, succ i)

f i = toMaybe (i<='p') (i::Char, succ i)

-}
-- | /O(n)/ Like 'unfoldrN' this function builds a 'Vector'
-- from a seed value with limited size.
-- Additionally it returns a value, that depends on the state,
-- but is not necessarily the state itself.
-- If end of vector and end of the generator coincide,
-- then the result is as if only the end of vector is reached.
--
-- Example:
--
-- > unfoldrResultN 30 Char.ord (\c -> if c>'z' then Left 1000 else Right (c, succ c)) 'a'
--
-- The following equation relates 'unfoldrN' and 'unfoldrResultN':
--
-- > unfoldrN n f s ==
-- >    unfoldrResultN n Just
-- >       (maybe (Left Nothing) Right . f) s
--
-- It is not possible to express 'unfoldrResultN' in terms of 'unfoldrN'.
--
unfoldrResultN :: (Storable b) => Int -> (a -> c) -> (a -> Either c (b, a)) -> a -> (Vector b, c)
unfoldrResultN i g f x0 =
   if i <= 0
     then (empty, g x0)
     else Unsafe.performIO $ createAndTrim' i $ \p -> go p 0 x0
       {-
       go must not be strict in the accumulator
       since otherwise packN would be too strict.
       -}
       where
          go = Strict.arguments2 $ \p n -> \a0 ->
             if n == i
               then return (0, n, g a0)
               else
                 case f a0 of
                   Left c -> return (0, n, c)
                   Right (b,a1) -> do St.poke p b
                                      go (incPtr p) (n+1) a1
{-# INLINE unfoldrResultN #-}

unfoldlN :: (Storable b) => Int -> (a -> Maybe (b, a)) -> a -> (Vector b, Maybe a)
unfoldlN i f x0
    | i < 0     = (empty, Just x0)
    | otherwise = Unsafe.performIO $ createAndTrim' i $ \p -> go (p `advancePtr` i) i x0
  where go = Strict.arguments2 $ \p n -> \x ->
           if n == 0
             then return (n, i, Just x)
             else
               case f x of
                 Nothing     -> return (n, i, Nothing)
                 Just (w,x') ->
                    let p' = p `advancePtr` (-1)
                    in  do St.poke p' w
                           go p' (n-1) x'
{-# INLINE unfoldlN #-}


-- | /O(n)/, where /n/ is the length of the result.
-- This function constructs a vector by evaluating a function
-- that depends on the element index.
-- It is a special case of 'unfoldrN' and can in principle be parallelized.
--
-- Examples:
--
-- >    sample 26 (\x -> chr(ord 'a'+x))
-- > == pack "abcdefghijklmnopqrstuvwxyz"
--
sample :: (Storable a) => Int -> (Int -> a) -> Vector a
sample n f =
   fst $ unfoldrN n (\i -> Just (f i, succ i)) 0
{-# INLINE sample #-}


-- ---------------------------------------------------------------------
-- Substrings

-- | /O(1)/ 'take' @n@, applied to a 'Vector' @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: (Storable a) => Int -> Vector a -> Vector a
take n ps@(SV x s l)
    | n <= 0    = empty
    | n >= l    = ps
    | otherwise = SV x s n
{-# INLINE take #-}

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or 'empty' if @n > 'length' xs@.
drop  :: (Storable a) => Int -> Vector a -> Vector a
drop n ps@(SV x s l)
    | n <= 0    = ps
    | n >= l    = empty
    | otherwise = SV x (s+n) (l-n)
{-# INLINE drop #-}

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: (Storable a) => Int -> Vector a -> (Vector a, Vector a)
splitAt n ps@(SV x s l)
    | n <= 0    = (empty, ps)
    | n >= l    = (ps, empty)
    | otherwise = (SV x s n, SV x (s+n) (l-n))
{-# INLINE splitAt #-}

{- | 'sliceVertical' @n xs@ divides vector in chunks of size @n@.
Requires time proportionally to length of result list,
i.e. @ceiling (length xs / n)@. -}
sliceVertical :: (Storable a) => Int -> Vector a -> [Vector a]
sliceVertical n =
   List.unfoldr (\x -> toMaybe (not (null x)) (splitAt n x))
{-# INLINE sliceVertical #-}

_sliceVertical :: (Storable a) => Int -> Vector a -> [Vector a]
_sliceVertical n xs =
   List.map (take n . flip drop xs) $
   List.takeWhile (< length xs) $ List.iterate (n+) 0


-- | 'takeWhile', applied to a predicate @p@ and a 'Vector' @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Storable a) => (a -> Bool) -> Vector a -> Vector a
takeWhile f ps = unsafeTake (findIndexOrEnd (not . f) ps) ps
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Storable a) => (a -> Bool) -> Vector a -> Vector a
dropWhile f ps = unsafeDrop (findIndexOrEnd (not . f) ps) ps
{-# INLINE dropWhile #-}

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Storable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)
break p ps = case findIndexOrEnd p ps of n -> (unsafeTake n ps, unsafeDrop n ps)
{-# INLINE break #-}

-- | 'breakEnd' behaves like 'break' but from the end of the 'Vector'
--
-- breakEnd p == spanEnd (not.p)
breakEnd :: (Storable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)
breakEnd  p ps = splitAt (findFromEndUntil p ps) ps

-- | 'span' @p xs@ breaks the 'Vector' into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Storable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)
span p ps = break (not . p) ps
{-# INLINE span #-}

-- | 'spanEnd' behaves like 'span' but from the end of the 'Vector'.
-- We have
--
-- > spanEnd (not.isSpace) "x y z" == ("x y ","z")
--
-- and
--
-- > spanEnd (not . isSpace) ps
-- >    ==
-- > let (x,y) = span (not.isSpace) (reverse ps) in (reverse y, reverse x)
--
spanEnd :: (Storable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)
spanEnd  p ps = splitAt (findFromEndUntil (not.p) ps) ps

-- | /O(n)/ Splits a 'Vector' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith (=='a') []        == []
--
splitWith :: (Storable a) => (a -> Bool) -> Vector a -> [Vector a]
splitWith _ (SV _ _ 0) = []
splitWith p ps = loop ps
    where
        loop =
           uncurry (:) .
           mapSnd (switchL [] (\ _ t -> loop t)) .
           break p
{-# INLINE splitWith #-}

-- | /O(n)/ Break a 'Vector' into pieces separated by the
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X"]
-- > split 'x'  "x"          == ["",""]
--
-- and
--
-- > join [c] . split c == id
-- > split == splitWith . (==)
--
-- As for all splitting functions in this library, this function does
-- not copy the substrings, it just constructs new 'Vector's that
-- are slices of the original.
--
split :: (Storable a, Eq a) => a -> Vector a -> [Vector a]
split w v = splitWith (w==) v
{-# INLINE split #-}

-- | Like 'splitWith', except that sequences of adjacent separators are
-- treated as a single separator. eg.
--
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Storable a) => (a -> Bool) -> Vector a -> [Vector a]
tokens f = P.filter (not.null) . splitWith f
{-# INLINE tokens #-}

-- | The 'group' function takes a 'Vector' and returns a list of
-- 'Vector's such that the concatenation of the result is equal to the
-- argument.  Moreover, each sublist in the result contains only equal
-- elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test. It is about 40% faster than
-- /groupBy (==)/
group :: (Storable a, Eq a) => Vector a -> [Vector a]
group xs =
   switchL []
      (\ h _ ->
          let (ys, zs) = span (== h) xs
          in  ys : group zs)
      xs

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Storable a) => (a -> a -> Bool) -> Vector a -> [Vector a]
groupBy k xs =
   switchL []
      (\ h t ->
          let n = 1 + findIndexOrEnd (not . k h) t
          in  unsafeTake n xs : groupBy k (unsafeDrop n xs))
      xs
{-# INLINE groupBy #-}


-- | /O(n)/ The 'join' function takes a 'Vector' and a list of
-- 'Vector's and concatenates the list after interspersing the first
-- argument between each element of the list.
join :: (Storable a) => Vector a -> [Vector a] -> Vector a
join s = concat . List.intersperse s
{-# INLINE join #-}

-- ---------------------------------------------------------------------
-- Indexing 'Vector's

-- | /O(1)/ 'Vector' index (subscript) operator, starting from 0.
index :: (Storable a) => Vector a -> Int -> a
index ps n
    | n < 0          = moduleError "index" ("negative index: " ++ show n)
    | n >= length ps = moduleError "index" ("index too large: " ++ show n
                                         ++ ", length = " ++ show (length ps))
    | otherwise      = ps `unsafeIndex` n
{-# INLINE index #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'Vector' which is equal to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: (Storable a, Eq a) => a -> Vector a -> Maybe Int
elemIndex c = findIndex (c==)
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'Vector' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs ==
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexEnd :: (Storable a, Eq a) => a -> Vector a -> Maybe Int
elemIndexEnd c =
   fst .
   foldl
      (\(ri,i) x -> (if c==x then Just i else ri, succ i))
      (Nothing,0)
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: (Storable a, Eq a) => a -> Vector a -> [Int]
elemIndices c = findIndices (c==)
{-# INLINE elemIndices #-}

-- | count returns the number of times its argument appears in the 'Vector'
--
-- > count = length . elemIndices
--
-- But more efficiently than using length on the intermediate list.
count :: (Storable a, Eq a) => a -> Vector a -> Int
count w =
   foldl (flip $ \c -> if c==w then succ else id) 0
{-
count w sv =
   List.length $ elemIndices w sv
-}
{-# INLINE count #-}

-- | The 'findIndex' function takes a predicate and a 'Vector' and
-- returns the index of the first element in the 'Vector'
-- satisfying the predicate.
findIndex :: (Storable a) => (a -> Bool) -> Vector a -> Maybe Int
findIndex p xs =
   {- The implementation is in principle the same as for findIndices,
      but we use the First monoid, instead of the List/append monoid.
      We could also implement findIndex in terms of monoidConcatMap. -}
   foldr
      (\x k n ->
         toMaybe (p x) n `mplus` k (succ n))
      (const Nothing) xs 0
{-# INLINE findIndex #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Storable a) => (a -> Bool) -> Vector a -> [Int]
findIndices p xs =
   foldr
      (\x k n ->
         (if p x then (n:) else id)
            (k (succ n)))
      (const []) xs 0
{-# INLINE findIndices #-}

-- | 'findIndexOrEnd' is a variant of findIndex, that returns the length
-- of the string if no element is found, rather than Nothing.
findIndexOrEnd :: (Storable a) => (a -> Bool) -> Vector a -> Int
findIndexOrEnd p xs =
   foldr
      (\x k n ->
         if p x then n else k (succ n))
      id xs 0
{-# INLINE findIndexOrEnd #-}

-- ---------------------------------------------------------------------
-- Searching Vectors

-- | /O(n)/ 'elem' is the 'Vector' membership predicate.
elem :: (Storable a, Eq a) => a -> Vector a -> Bool
elem c ps = isJust $ elemIndex c ps
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: (Storable a, Eq a) => a -> Vector a -> Bool
notElem c ps = not (elem c ps)
{-# INLINE notElem #-}

-- | /O(n)/ 'filter', applied to a predicate and a 'Vector',
-- returns a 'Vector' containing those elements that satisfy the predicate.
filter :: (Storable a) => (a -> Bool) -> Vector a -> Vector a
filter p (SV fp s l) =
   let end = s+l
   in  fst $
       unfoldrN l
          (let go = Strict.arguments1 $ \k0 ->
                  do guard (k0<end)
                     let x = foreignPeek fp k0
                         k1 = succ k0
                     if p x
                       then Just (x,k1)
                       else go k1
           in  go)
          s
{-# INLINE filter #-}

-- | /O(n)/ The 'find' function takes a predicate and a 'Vector',
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Storable a) => (a -> Bool) -> Vector a -> Maybe a
find f p = fmap (unsafeIndex p) (findIndex f p)
{-# INLINE find #-}

-- ---------------------------------------------------------------------
-- Searching for substrings

-- | /O(n)/ The 'isPrefixOf' function takes two 'Vector' and returns 'True'
-- iff the first is a prefix of the second.
isPrefixOf :: (Storable a, Eq a) => Vector a -> Vector a -> Bool
isPrefixOf x@(SV _ _ l1) y@(SV _ _ l2) =
    l1 <= l2 && x == unsafeTake l1 y

-- | /O(n)/ The 'isSuffixOf' function takes two 'Vector's and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
isSuffixOf :: (Storable a, Eq a) => Vector a -> Vector a -> Bool
isSuffixOf x@(SV _ _ l1) y@(SV _ _ l2) =
    l1 <= l2 && x == unsafeDrop (l2 - l1) y

-- ---------------------------------------------------------------------
-- Zipping

-- | /O(n)/ 'zip' takes two 'Vector's and returns a list of
-- corresponding pairs of elements. If one input 'Vector' is short,
-- excess elements of the longer 'Vector' are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: (Storable a, Storable b) => Vector a -> Vector b -> [(a, b)]
zip ps qs =
   maybe [] id $
      do (ph,pt) <- viewL ps
         (qh,qt) <- viewL qs
         return ((ph,qh) : zip pt qt)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two 'Vector's to produce the list of
-- corresponding sums.
zipWith :: (Storable a, Storable b, Storable c) =>
   (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f as bs =
   unsafeWithStartPtr as $ \pa0 la ->
   withStartPtr       bs $ \pb0 lb ->
   let len = min la lb
   in  create len $ \p0 ->
       let go = Strict.arguments4 $ \n p pa pb ->
              when (n>0) $
                 liftM2 f (St.peek pa) (St.peek pb) >>= St.poke p >>
                 go (pred n) (incPtr p) (incPtr pa) (incPtr pb)
       in  go len p0 pa0 pb0


-- zipWith f ps qs = pack $ List.zipWith f (unpack ps) (unpack qs)
{-# INLINE zipWith #-}

-- | Like 'zipWith' but for three input vectors
zipWith3 :: (Storable a, Storable b, Storable c, Storable d) =>
   (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
zipWith3 f as bs cs =
   unsafeWithStartPtr as $ \pa0 la ->
   withStartPtr       bs $ \pb0 lb ->
   withStartPtr       cs $ \pc0 lc ->
   let len = la `min` lb `min` lc
   in  create len $ \p0 ->
       let go = Strict.arguments5 $ \n p pa pb pc ->
              when (n>0) $
                 liftM3 f (St.peek pa) (St.peek pb) (St.peek pc) >>= St.poke p >>
                 go (pred n) (incPtr p) (incPtr pa) (incPtr pb) (incPtr pc)
       in  go len p0 pa0 pb0 pc0
{-# INLINE zipWith3 #-}

-- | Like 'zipWith' but for four input vectors
-- If you need even more input vectors,
-- you might write a function yourselve using unfoldrN and viewL.
zipWith4 :: (Storable a, Storable b, Storable c, Storable d, Storable e) =>
   (a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
zipWith4 f as bs cs ds =
   unsafeWithStartPtr as $ \pa0 la ->
   withStartPtr       bs $ \pb0 lb ->
   withStartPtr       cs $ \pc0 lc ->
   withStartPtr       ds $ \pd0 ld ->
   let len = la `min` lb `min` lc `min` ld
   in  create len $ \p0 ->
       let go =
              Strict.arguments2 $ \n p ->
              Strict.arguments4 $ \pa pb pc pd ->
              when (n>0) $
                 liftM4 f (St.peek pa) (St.peek pb) (St.peek pc) (St.peek pd) >>= St.poke p >>
                 go (pred n) (incPtr p) (incPtr pa) (incPtr pb) (incPtr pc) (incPtr pd)
       in  go len p0 pa0 pb0 pc0 pd0
{-# INLINE zipWith4 #-}

-- | /O(n)/ 'unzip' transforms a list of pairs of elements into a pair of
-- 'Vector's. Note that this performs two 'pack' operations.
unzip :: (Storable a, Storable b) => [(a, b)] -> (Vector a, Vector b)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))
{-# INLINE unzip #-}

-- ---------------------------------------------------------------------
-- Interleaved 'Vector's

-- | /O(l/n)/ 'sieve' selects every 'n'th element.
sieve :: (Storable a) => Int -> Vector a -> Vector a
sieve n xs =
   case P.compare n 1 of
      P.LT -> error "sieve: non-positive step size"
      P.EQ -> xs
      P.GT -> sieveCore n xs

sieveCore :: (Storable a) => Int -> Vector a -> Vector a
sieveCore n (SV fp s l) =
   let end = s+l
   in  fst $
       unfoldrN (- div (-l) n)
          (Strict.arguments1 $ \k0 ->
              do guard (k0<end)
                 Just (foreignPeek fp k0, k0 + n))
          s
{-# INLINE sieve #-}

-- | /O(n)/
-- Returns n sieved vectors with successive starting elements.
-- @deinterleave 3 (pack ['a'..'k']) = [pack "adgj", pack "behk", pack "cfi"]@
-- This is the same as 'Data.List.HT.sliceHorizontal'.
deinterleave :: (Storable a) => Int -> Vector a -> [Vector a]
deinterleave n =
   P.map (sieve n) . P.take n . P.iterate laxTail

-- | /O(n)/
-- Almost the inverse of deinterleave.
-- Restriction is that all input vector must have equal length.
-- @interleave [pack "adgj", pack "behk", pack "cfil"] = pack ['a'..'l']@
interleave :: (Storable a) => [Vector a] -> Vector a
interleave [] = empty
interleave [xs] = xs
interleave vs =
   Unsafe.performIO $
   MC.runContT
      (do
         pls <- mapM (\v -> MC.ContT (withStartPtr v . curry)) vs
         let (ps,ls) = P.unzip pls
         ptrs <- MC.ContT (withArray ps)
         if and (ListHT.mapAdjacent (==) ls)
           then return (ptrs, P.sum ls)
           else moduleError "interleave" "all input vectors must have the same length")
      (\(ptrs, totalLength) -> create totalLength $ \p ->
         let len = P.length vs
             go = Strict.arguments1 $ \m ->
                when (m < totalLength) $ do
                   {-
                   divMod would be more correct,
                   but is slower on the architectures I know
                   -}
                   let (j,k) = P.quotRem m len
                   pokeElemOff p m =<< flip peekElemOff j =<< peekElemOff ptrs k
                   go $ succ m
         in  go 0)
{-# INLINE interleave #-}


-- ---------------------------------------------------------------------
-- Special lists

-- | /O(n)/ Return all initial segments of the given 'Vector', shortest first.
inits :: (Storable a) => Vector a -> [Vector a]
inits (SV x s l) = List.map (SV x s) [0..l]

-- | /O(n)/ Return all final segments of the given 'Vector', longest first.
tails :: (Storable a) => Vector a -> [Vector a]
tails p =
   switchL [empty] (\ _ t -> p : tails t) p

-- ---------------------------------------------------------------------
-- ** Ordered 'Vector's

-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(n)/ Make a copy of the 'Vector' with its own storage.
--   This is mainly useful to allow the rest of the data pointed
--   to by the 'Vector' to be garbage collected, for example
--   if a large string has been read in, and only a small part of it
--   is needed in the rest of the program.
copy :: (Storable a) => Vector a -> Vector a
copy v =
   unsafeWithStartPtr v $ \f l ->
   create l $ \p ->
   copyArray p f (fromIntegral l)



-- ---------------------------------------------------------------------
-- IO

-- | Write a 'Vector' to a contiguous piece of memory.
poke :: (Storable a) => Ptr a -> Vector a -> IO ()
poke dst v =
   withStartPtr v $ \src len -> copyArray dst src len

-- | Read a 'Vector' from a contiguous piece of memory.
peek :: (Storable a) => Int -> Ptr a -> IO (Vector a)
peek len src =
   create len $ \dst -> copyArray dst src len

-- | Outputs a 'Vector' to the specified 'Handle'.
hPut :: (Storable a) => Handle -> Vector a -> IO ()
hPut h v =
   when (not (null v)) $
      withStartPtr v $ \ ptrS l ->
         let ptrE = advancePtr ptrS l
             -- use advancePtr and minusPtr in order to respect alignment
         in  hPutBuf h ptrS (minusPtr ptrE ptrS)

-- | Read a 'Vector' directly from the specified 'Handle'.  This
-- is far more efficient than reading the characters into a list
-- and then using 'pack'.
--
hGet :: (Storable a) => Handle -> Int -> IO (Vector a)
hGet _ 0 = return empty
hGet h l =
   createAndTrim l $ \p ->
      let elemType :: Ptr a -> a
          elemType _ = undefined
          roundUp m n = n + mod (-n) m
          sizeOfElem =
             roundUp
                (alignment (elemType p))
                (sizeOf (elemType p))
      in  fmap (flip div sizeOfElem) $
          hGetBuf h p (l * sizeOfElem)
{-
   createAndTrim l $ \p ->
      fmap (flip div (incPtr p `minusPtr` p)) $
      hGetBuf h p (advancePtr p l `minusPtr` p)
-}

-- | Read an entire file strictly into a 'Vector'.  This is far more
-- efficient than reading the characters into a 'String' and then using
-- 'pack'.  It also may be more efficient than opening the file and
-- reading it using hGet. Files are read using 'binary mode' on Windows.
--
readFile :: (Storable a) => FilePath -> IO (Vector a)
readFile f =
   bracket (openBinaryFile f ReadMode) hClose
      (\h -> hGet h . fromIntegral =<< hFileSize h)

-- | Write a 'Vector' to a file.
writeFile :: (Storable a) => FilePath -> Vector a -> IO ()
writeFile f txt =
   bracket (openBinaryFile f WriteMode) hClose
      (\h -> hPut h txt)

-- | Append a 'Vector' to a file.
appendFile :: (Storable a) => FilePath -> Vector a -> IO ()
appendFile f txt =
   bracket (openBinaryFile f AppendMode) hClose
      (\h -> hPut h txt)


-- ---------------------------------------------------------------------
-- Internal utilities


-- These definitions of succ and pred do not check for overflow
-- and are faster than their counterparts from Enum class.
succ :: Int -> Int
succ n = n+1
{-# INLINE succ #-}

pred :: Int -> Int
pred n = n-1
{-# INLINE pred #-}

unsafeWithStartPtr :: Storable a => Vector a -> (Ptr a -> Int -> IO b) -> b
unsafeWithStartPtr v f =
   Unsafe.performIO (withStartPtr v f)
{-# INLINE unsafeWithStartPtr #-}

foreignPeek :: Storable a => ForeignPtr a -> Int -> a
foreignPeek fp k =
   inlinePerformIO $ withForeignPtr fp $ flip peekElemOff k
{-# INLINE foreignPeek #-}

withNonEmptyVector ::
   String -> (ForeignPtr a -> Int -> Int -> b) -> Vector a -> b
withNonEmptyVector fun f (SV x s l) =
   if l <= 0
     then errorEmpty fun
     else f x s l
{-# INLINE withNonEmptyVector #-}

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmpty :: String -> a
errorEmpty fun = moduleError fun "empty Vector"
{-# NOINLINE errorEmpty #-}

moduleError :: String -> String -> a
moduleError fun msg = error ("Data.StorableVector." ++ fun ++ ':':' ':msg)
{-# NOINLINE moduleError #-}

-- Find from the end of the string using predicate
findFromEndUntil :: (Storable a) => (a -> Bool) -> Vector a -> Int
findFromEndUntil = Strict.arguments2 $ \f ps@(SV x s l) ->
    if null ps then 0
    else if f (last ps) then l
         else findFromEndUntil f (SV x s (l-1))
