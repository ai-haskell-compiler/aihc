
{-# LANGUAGE CPP #-}

-- | Binary lists are lists whose number of elements is a power of two.
--   This data structure is efficient for some computations like:
--
-- * Splitting a list in half.
--
-- * Appending two lists of the same length.
--
-- * Extracting an element from the list.
--
--   All the functions exported are total except for 'fromListWithDefault'.
--   It is impossible for the user of this library to create a binary list
--   whose length is /not/ a power of two.
--
--   Since many names in this module clash with the names of some "Prelude"
--   functions, you probably want to import this module this way:
--
-- > import Data.BinaryList (BinList,Exponent)
-- > import qualified Data.BinaryList as BL
--
--   Remember that binary lists are an instance of the 'Foldable' and 'Traversable'
--   classes. If you are missing a function here, look for functions using those
--   instances.
--
--   Note that some functions like 'replicate', 'generate', or 'take', don't use
--   the length of the list as argument, but the exponent of its length expressed
--   as a power of two. Throughout this document, this is referred
--   as the /length exponent/. For example, if the list has length 16, its length exponent
--   is 4 since 2^4 = 16. Therefore @replicate 4 0@ will create a list with 16 zeroes.
--   Keep this in mind when using this library. Note as well that this implies that
--   there is no need to check that the length argument is or is not a power of two.
--
module Data.BinaryList (
    -- * Type
    BinList
  , Exponent
    -- * Construction
  , singleton
  , append
  , replicate
  , replicateA
  , replicateAR
  , generate
  , generateM
    -- * Queries
  , lengthExponent
#if !MIN_VERSION_base(4,8,0)
  , length
#endif
  , lookup
  , head
  , last
    -- * Deconstruction
  , split
  , take
  , takeEnd
    -- * Transformation
  , replace
  , reverse
    -- * Tuples
  , joinPairs
  , disjoinPairs
    -- * Zipping and Unzipping
  , zip , unzip
  , zipWith
    -- * Lists
    -- ** From list
  , fromList
  , fromListWithDefault
  , fromListSplit
    -- ** To list
  , toListFilter
  , toListSegment
    -- * Others
  , traverseSegment
    -- * Example: Radix-2 FFT
    -- $fft
  ) where

import Prelude hiding
  ( map, head, last
  , reverse, replicate
  , take, lookup
  , zip, unzip, zipWith
  , foldr1, length, foldr
    )
#if MIN_VERSION_base(4,8,0)
import Prelude (length, foldr, foldr1)
#endif
import Data.Foldable (fold,toList)
import Foreign.Storable (sizeOf)
import Data.List (find)
import Data.BinaryList.Internal
import Control.Applicative.Backwards
import Control.Arrow ((***))
import Control.Monad.Trans.State
  ( StateT (..)
  , evalStateT ,evalState
  , runState
  , get ,modify )
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity (..))
import Control.Applicative.PhantomState
import Data.Word (Word8)

-- GHC-7.8 compatibility
#if !MIN_VERSION_base(4,8,0)

import qualified Prelude
import Data.Foldable (Foldable (..))
import Data.Traversable (Traversable (..))
import Control.Applicative (Applicative (..), (<$>))
import Data.Monoid (mappend)

-- | /O(1)/. Number of elements in the list.
length :: BinList a -> Int
length = (2^) . lengthExponent

#endif


-- | An exponent.
type Exponent = Word8

-- | /O(1)/. Build a list with a single element.
singleton :: a -> BinList a
singleton = ListEnd

-- | /O(1)/. Given a binary list @l@ with length @2^k@:
--
-- > lengthExponent l = k
--
lengthExponent :: BinList a -> Exponent
lengthExponent (ListNode n _ _) = n
lengthExponent (ListEnd _) = 0

{-# RULES
       "Data.BinaryList: length equality"
         forall xs ys . length xs == length ys = lengthExponent xs == lengthExponent ys
  #-}

-- | /O(log n)/. Lookup an element in the list by its index (starting from 0).
--   If the index is out of range, 'Nothing' is returned.
lookup :: BinList a -> Int -> Maybe a
lookup (ListNode n l r) i =
   let m = 2^(n-1) -- Number of elements in a single branch
   in  if i < m
          then lookup l i       -- Lookup in the left branch
          else lookup r $ i - m -- Lookup in the right branch
lookup (ListEnd x) 0 = Just x
lookup _ _ = Nothing

-- | /O(log n)/. Replace a single element in the list. If the index is
--   out of range, returns the original list.
replace :: Int -- ^ Index to look for
        -> a   -- ^ Element to insert
        -> BinList a -> BinList a
replace i0 y = go i0
  where
    go i (ListNode n l r) =
      let m = 2^(n-1)
      in  if i < m
             then ListNode n (go i l)          r
             else ListNode n       l (go (i-m) r)
    go 0 (ListEnd _) = ListEnd y
    go _ e = e

-- | /O(1)/. Append two binary lists. This is only possible
--   if both lists have the same length. If this condition
--   is not hold, 'Nothing' is returned.
append :: BinList a -> BinList a -> Maybe (BinList a)
append xs ys =
  let i = lengthExponent xs
  in  if i == lengthExponent ys
         then Just $ ListNode (i+1) xs ys
         else Nothing

-- | /O(1)/. Split a binary list into two sublists of half the length,
--   unless the list only contains one element. In that case, it
--   just returns that element.
split :: BinList a -> Either a (BinList a,BinList a)
split (ListNode _ l r) = Right (l,r)
split (ListEnd x) = Left x

-- | /O(log n)/. Calling @take n xs@ returns the first @min (2^n) (length xs)@ elements of @xs@.
take :: Exponent -> BinList a -> BinList a
take k xs@(ListNode n l _) = if k >= n then xs else take k l
take _ xs = xs

-- | /O(log n)/. Calling @takeEnd n xs@ returns the last @min (2^n) (length xs)@ elements of @xs@.
takeEnd :: Exponent -> BinList a -> BinList a
takeEnd k xs@(ListNode n _ r) = if k >= n then xs else takeEnd k r
takeEnd _ xs = xs

-- | Calling @replicateA n f@ builds a binary list collecting the results of
--   executing @2^n@ times the applicative action @f@.
replicateA :: Applicative f => Exponent -> f a -> f (BinList a)
replicateA n f = go n
  where
    go 0 = ListEnd <$> f
    go i = let b = go (i-1)
           in  ListNode <$> pure i <*> b <*> b

-- | The same as 'replicateA', but the actions are executed in reversed order.
replicateAR :: Applicative f => Exponent -> f a -> f (BinList a)
replicateAR n = forwards . replicateA n . Backwards

{-# RULES
      "Data.BinaryList: map reverse/replicateA"
         forall i f . map reverse (replicateA  i f) = replicateAR i f
  #-}

{-# RULES
      "Data.BinaryList: map reverse/replicateAR"
         forall i f . map reverse (replicateAR i f) = replicateA  i f
  #-}

-- | /O(log n)/. Calling @replicate n x@ builds a binary list with
--   @2^n@ occurences of @x@.
replicate :: Exponent -> a -> BinList a
replicate n = runIdentity . replicateA n . Identity

{-# RULES
      "Data.BinaryList: map/replicate"
         forall f n x . map f (replicate n x) = replicate n (f x)
  #-}

-- | /O(n)/. Build a binary list with the given length exponent (see 'lengthExponent')
--   by applying a function to each index.
generate :: Exponent -> (Int -> a) -> BinList a
generate l f = evalState (replicateA l $ fmap f get <* modify (+1)) 0

-- | Like 'generate', but the generator function returns a value in a 'Monad'.
--   Therefore, the result is as well contained in a 'Monad'.
generateM :: (Applicative m, Monad m) => Exponent -> (Int -> m a) -> m (BinList a)
generateM l f = evalStateT (replicateA l $ (get >>= lift . f) <* modify (+1)) 0

-- | /O(log n)/. Get the first element of a binary list.
head :: BinList a -> a
head (ListNode _ l _) = head l
head (ListEnd x) = x

-- | /O(log n)/. Get the last element of a binary list.
last :: BinList a -> a
last (ListNode _ _ r) = last r
last (ListEnd x) = x

-- | /O(n)/. Reverse a binary list.
reverse :: BinList a -> BinList a
{-# INLINE[2] reverse #-}
reverse (ListNode n l r) = ListNode n (reverse r) (reverse l)
reverse xs = xs

{-# RULES
      "Data.BinaryList: reverse/reverse"
         forall xs. reverse (reverse xs) = xs
  #-}

------------------------------
-- Transformations with tuples

-- | /O(n)/. Transform a list of pairs into a flat list. The
--   resulting list will have twice more elements than the
--   original.
joinPairs :: BinList (a,a) -> BinList a
{-# INLINE[1] joinPairs #-}
joinPairs (ListEnd (x,y)) = ListNode 1 (ListEnd x) (ListEnd y)
joinPairs (ListNode n l r) = ListNode (n+1) (joinPairs l) (joinPairs r)

-- | /O(n)/. Opposite transformation of 'joinPairs'. It halves
--   the number of elements of the input. As a result, when
--   applied to a binary list with a single element, it returns
--   'Nothing'.
disjoinPairs :: BinList a -> Maybe (BinList (a,a))
{-# INLINE [1] disjoinPairs #-}
disjoinPairs (ListEnd _) = Nothing
disjoinPairs xs = Just $ disjoinPairsNodes xs

disjoinPairsNodes :: BinList a -> BinList (a,a)
disjoinPairsNodes (ListNode _ (ListEnd x) (ListEnd y)) = ListEnd (x,y)
disjoinPairsNodes (ListNode n l r) = ListNode (n-1) (disjoinPairsNodes l) (disjoinPairsNodes r)
disjoinPairsNodes _ = error "disjoinPairsNodes: bug. Please, report this with an example input."

{-# RULES
      "Data.BinaryList: disjoinPairs/joinPairs"
         forall xs . disjoinPairs (joinPairs xs) = Just xs
  #-}

{-# RULES
      "Data.BinaryList: disjoinPairs/map/joinPairs"
         forall f xs . disjoinPairs (map f (joinPairs xs)) = Just (map (f *** f) xs)
  #-}

-- | Expression @pairBuilder f xs@ is equivalent to @joinPairs (map f xs)@, but does
--   not build any intermediate structure. Used for rewriting rules.
pairBuilder :: (a -> (b,b)) -> BinList a -> BinList b
{-# INLINE[0] pairBuilder #-}
pairBuilder f = go
  where
    go (ListEnd x) = let (a,b) = f x in ListNode 1 (ListEnd a) (ListEnd b)
    go (ListNode n l r) = ListNode (n+1) (go l) (go r)

{-# RULES
      "Data.BinaryList: joinPairs/map"
         forall f xs . joinPairs (map f xs) = pairBuilder f xs
  #-}

-- | Expression @zipAndJoing f g xs ys@ is equivalent to @pairBuilder f (zipWith g xs ys)@,
--   but does not build any intermediate structure. Used for rewriting rules.
zipAndJoin :: (c -> (d,d)) -> (a -> b -> c) -> BinList a -> BinList b -> BinList d
zipAndJoin f g = go
  where
    -- Recursion
    go xs@(ListNode n l r) ys@(ListNode n' l' r')
         -- If both lists have the same length, recurse assuming it
         -- to avoid comparisons.
       | n == n'   = ListNode (n+1) (goEquals l l') (goEquals r r')
         -- If the first list is larger, the second fits entirely in
         -- the left branch of the first.
       | n >  n'   = go l ys
         -- If the second list is larger, the first fits entirely in
         -- the left branch of the second.
       | otherwise = go xs l'
    go xs ys       = let (x,y) = f $ g (head xs) (head ys)
                     in  ListNode 1 (ListEnd x) (ListEnd y)
    -- Recursion assuming both lists have the same length
    goEquals (ListNode n l r) (ListNode _ l' r') =
                     ListNode (n+1) (goEquals l l') (goEquals r r')
    goEquals (ListEnd x) (ListEnd y) =
      let (x',y') = f $ g x y
      in  ListNode 1 (ListEnd x') (ListEnd y')
    goEquals _ _ = undefined -- This can't happen!

{-# RULES
      "Data.BinaryList: pairBuilder/zipWith"
         forall f g xs ys . pairBuilder f (zipWith g xs ys) = zipAndJoin f g xs ys
  #-}

------------------------
-- Zipping and Unzipping

-- | /O(n)/. Zip two binary lists using an operator.
zipWith :: (a -> b -> c) -> BinList a -> BinList b -> BinList c
{-# INLINE[1] zipWith #-}
zipWith f = go
  where
    -- Recursion
    go xs@(ListNode n l r) ys@(ListNode n' l' r')
         -- If both lists have the same length, recurse assuming it
         -- to avoid comparisons.
       | n == n'   = ListNode n (goEquals l l') (goEquals r r')
         -- If the first list is larger, the second fits entirely in
         -- the left branch of the first.
       | n >  n'   = go l ys
         -- If the second list is larger, the first fits entirely in
         -- the left branch of the second.
       | otherwise = go xs l'
    go xs ys       = ListEnd $ f (head xs) (head ys)
    -- Recursion assuming both lists have the same length
    goEquals (ListNode n l r) (ListNode _ l' r') =
                     ListNode n (goEquals l l') (goEquals r r')
    goEquals (ListEnd x) (ListEnd y) = ListEnd (f x y)
    goEquals _ _ = undefined -- this can't happen

-- | /O(n)/. Zip two binary lists in pairs.
zip :: BinList a -> BinList b -> BinList (a,b)
{-# INLINE zip #-}
zip = zipWith (,)

-- | /O(n)/. Unzip a binary list of pairs.
unzip :: BinList (a,b) -> (BinList a, BinList b)
{-# INLINE[1] unzip #-}
unzip (ListEnd (x,y)) = (ListEnd x, ListEnd y)
unzip (ListNode n l r) =
  let (la,lb) = unzip l
      (ra,rb) = unzip r
  in  (ListNode n la ra, ListNode n lb rb)

-- | Expression @unzipMap f xs@ is equivalent to @unzip (map f xs)@, but
--   does not create any intermediate structure.
unzipMap :: ((a,b) -> (c,d)) -> BinList (a,b) -> (BinList c,BinList d)
unzipMap f = go
  where
    go (ListEnd p) = ListEnd *** ListEnd $ f p
    go (ListNode n l r) =
      let (lc,ld) = go l
          (rc,rd) = go r
      in  (ListNode n lc rc, ListNode n ld rd)

{-# RULES
      "Data.BinaryList: unzip/map"
         forall f xs . unzip (map f xs) = unzipMap f xs
  #-}

-----------------------------
-- Transforming from/to lists

-- | /O(log n)/. Calculate the exponent of a positive integer number expressed
--   as a power of two.
exponentInBasisTwo :: Int -> Maybe Exponent
exponentInBasisTwo 1 = Just 0
exponentInBasisTwo n =
  if even n
     then fmap (+1) $ exponentInBasisTwo $ div n 2
     else Nothing

-- | /O(n)/. Build a binary list from a linked list. If the input list
--   has length different from a power of two, it returns 'Nothing'.
fromList :: [a] -> Maybe (BinList a)
fromList xs = fmap builder . exponentInBasisTwo $ Prelude.length xs
  where
    builder l = evalState (replicateA l $ StateT $ \(h:t) -> pure (h,t)) xs

-- | /O(1)/. This is the last exponent that has power of two defined in the type 'Int'.
--
-- /Note: This value is system dependent, since the type 'Int' varies in size/
-- /from system to system./
--
lastExponentOfTwo :: Exponent
lastExponentOfTwo = fromIntegral $ 8 * sizeOf (undefined :: Int) - 2

-- | /O(1)/. Calculate the next power of two exponent, if there is any. It is possible
--   to not find a next one since the type 'Int' is finite. If the input is
--   already a power of two, its exponent is returned.
nextExponentOfTwo :: Int -> Maybe Exponent
nextExponentOfTwo n = find (\i -> n <= 2^i) [0 .. lastExponentOfTwo]

-- | /O(n)/. Build a binary list from a linked list. If the input list
--   has length different from a power of two, fill to the next
--   power of two with a default element.
--
-- /Warning: this function crashes if the input list length is larger than any/
-- /power of two in the type 'Int'. However, this is very unlikely./
fromListWithDefault :: a -> [a] -> BinList a
fromListWithDefault e xs =
  let l = Prelude.length xs
  in  case nextExponentOfTwo l of
        Just n ->
          evalState (replicateA n $ StateT $
             \ys -> pure $ case ys of
                      (h:t) -> (h,t )
                      _     -> (e,[])
               ) xs
        _ -> error "[binary-list] fromListWithDefault: input list is too big."

-- | /O(n)/. Build a binary list from a linked list. It returns a binary list
--   with length @2 ^ n@ (where @n@ is the supplied 'Int' argument), and
--   the list of elements of the original list that were not used. If the
--   input list is shorter than @2 ^ n@, a default element will be used to
--   complete the binary list. This method for building binary lists is faster
--   than both 'fromList' and 'fromListWithDefault'.
fromListSplit :: a   -- ^ Default element
              -> Exponent -- ^ Length exponent
              -> [a] -- ^ Input list
              -> (BinList a, [a])
fromListSplit e n =
  runState $ replicateA n $ StateT $
    \xs -> pure $ case xs of
                    (h:t) -> (h,t )
                    _     -> (e,[])

-- | /O(n)/. Create a list from the elements of a binary list matching a given
--   condition.
toListFilter :: (a -> Bool) -> BinList a -> [a]
{-# INLINE toListFilter #-}
toListFilter c = foldr (\x -> if c x then (x:) else id) []

-- | /O(n)/. Create a list extracting a sublist of elements from a binary list.
toListSegment :: Int -> Int -> BinList a -> [a]
{-# INLINE toListSegment #-}
toListSegment s e xs = runPhantomState (traverseSegment (changeState . (:)) s e xs) []

-- | Apply an applicative action to every element in a segment of a binary list, from left to right.
traverseSegment :: Applicative f => (a -> f ()) -> Int -> Int -> BinList a -> f ()
{-# INLINE traverseSegment #-}
traverseSegment f s e xs
  | s > e = pure ()
  | e < 0 = pure ()
  | s >= length xs = pure ()
  | otherwise = traverseSegmentFromTo f (max 0 s) e xs

traverseSegmentFromTo :: Applicative f => (a -> f ()) -> Int -> Int -> BinList a -> f ()
{-# INLINE traverseSegmentFromTo #-}
traverseSegmentFromTo f = go
  where
    go s e (ListNode n l r) =
      let k = 2^(n-1)
      in  if s >= k
             -- Sublist is contained in right portion
             then go (s - k) (e - k) r
             else if e < k
                     -- Sublist is contained in left portion
                     then go s e l
                     -- Sublist is divided in both portions
                     else traverseSegmentFrom f s l *> traverseSegmentTo f (e - k) r
    go _ _ (ListEnd x) = f x

traverseSegmentFrom :: Applicative f => (a -> f ()) -> Int -> BinList a -> f ()
{-# INLINE traverseSegmentFrom #-}
traverseSegmentFrom f = go
  where
    go s (ListNode n l r) =
      let k = 2^(n-1)
      in  if s >= k
             -- Sublist is contained in right portion
             then go (s - k) r
             -- Sublist is divided in both portions, but right
             -- portion is taken entirely
             else go s l *> traverseFull f r
    go _ (ListEnd x) = f x

traverseSegmentTo :: Applicative f => (a -> f ()) -> Int -> BinList a -> f ()
{-# INLINE traverseSegmentTo #-}
traverseSegmentTo f = go
  where
    go e (ListNode n l r) =
      let k = 2^(n-1)
      in  if e < k
             -- Sublist is contained in left portion
             then go e l
             -- Sublist is divided in both portions, but left
             -- portion is taken entirely
             else traverseFull f l *> go (e - k) r
    go _ (ListEnd x) = f x

traverseFull :: Applicative f => (a -> f ()) -> BinList a -> f ()
{-# INLINE traverseFull #-}
traverseFull f = go
  where
    go (ListEnd x) = f x
    go (ListNode _ l r) = go l *> go r

------------------------------------------------
------------------------------------------------

-----------------------------
-- Show and Functor instances

instance Show a => Show (BinList a) where
  show = show . toList

{- Internal map

Although we encourage the use of 'fmap', we define fmap as a custom 'map'
function and inline 'fmap' to make them equivalent, so writing 'fmap' is
actually writing 'map'. We do this to use 'map' in rewriting rules.

-}

map :: (a -> b) -> BinList a -> BinList b
{-# INLINE[1] map #-}
map f = go
  where
    go (ListEnd x) = ListEnd (f x)
    go (ListNode n l r) = ListNode n (go l) (go r)

instance Functor BinList where
  {-# INLINE fmap #-}
  fmap = map

instance Foldable BinList where
  -- Folding
  foldr1 f = go
    where
      go (ListEnd x) = x
      go (ListNode _ l r) = f (go l) (go r)
  --
  fold = foldr1 mappend
  foldl1 = foldr1
  foldMap f = fold . fmap f
#if MIN_VERSION_base(4,8,0)
  length = (2^) . lengthExponent
#endif

instance Traversable BinList where
  sequenceA (ListEnd f) = ListEnd <$> f
  sequenceA (ListNode n l r) = ListNode <$> pure n <*> sequenceA l <*> sequenceA r

-----------------------------
-- Example: Radix-2 FFT

{- $fft

This is an example demonstrating the use of binary lists to calculate the Discrete
Fourier Transform of complex vectors with the Radix-2 Fast Fourier Transform algorithm.

> import Data.BinaryList (BinList)
> import qualified Data.BinaryList as BL
> 
> import Data.Complex
> import Data.Maybe (fromJust)
> 
> i :: Complex Double
> i = 0 :+ 1
> 
> fft :: BinList (Complex Double) -> BinList (Complex Double)
> fft xs =
>   case BL.disjoinPairs xs of
>     Nothing -> xs
>     Just ps ->
>       let (evens,odds) = BL.unzip ps
>           n = BL.lengthExponent xs - 1
>           q = negate $ pi * i / fromIntegral (2^n)
>           twiddles = BL.generate n $ \k -> exp $ q * fromIntegral k
>           oddsfft = BL.zipWith (*) twiddles $ fft odds
>           evensfft = fft evens
>       in  fromJust $
>             BL.append (BL.zipWith (+) evensfft oddsfft)
>                       (BL.zipWith (-) evensfft oddsfft)

-}
