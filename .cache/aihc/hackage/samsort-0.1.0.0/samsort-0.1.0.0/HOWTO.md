# Using this library

This is a short guide to demonstrate how this library may be used. Familiarity
with some form of arrays and mutable arrays in Haskell is expected. If your
array type is not present here, it should still be possible to adapt some of the
code below to your use case.

## Sorting boxed elements

This library offers the function

```hs
sortArrayBy
  :: (a -> a -> Ordering)  -- ^ comparison
  -> MutableArray# s a
  -> Int                   -- ^ offset
  -> Int                   -- ^ length
  -> ST s ()
```

So how does one use this?

* The first parameter is a comparison function that will be used to order the
  elements.
* The second parameter is a [`MutableArray#`](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exts.html#t:MutableArray-35-)
  with elements of type `a`. This is a primitive array type provided by GHC.
  This array will be sorted in place.
* The third and fourth parameters are `Int`s which demarcate a slice of the
  array. Elements in this slice will be sorted, and other elements will not be
  touched.
* The return type is an `ST` action. If you are not familiar with `ST`, please
  see the documentation for [`Control.Monad.ST`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Monad-ST.html).

Clearly, to use `sortArrayBy`, an important step is to put the elements to be
sorted into a `MutableArray#`. The most convenient way to do this depends on how
the elements are stored prior to sorting.

### Example 1: [`MVector`](https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector-Mutable.html#t:MVector)

Consider that we need to sort a mutable vector `MVector` from the `vector`
library. This is quite easy, and in fact we do not need to put elements anywhere
because the underlying representation of an `MVector` is a `MutableArray#`! We
only need to get it out of the `MVector`.

```hs
import Control.Monad.Primitive (PrimMonad(..), stToPrim)  -- from the package "primitive"
import Data.Primitive.Array (MutableArray(..))  -- also from "primitive"
import Data.Vector.Mutable (MVector(..))

import qualified Data.SamSort as Sam

-- | Sort a mutable vector in place.
sortMV :: (PrimMonad m, Ord a) => MVector (PrimState m) a -> m ()
sortMV = sortMVBy compare

-- | Sort a mutable vector in place using a comparison function.
sortMVBy :: PrimMonad m => (a -> a -> Ordering) -> MVector (PrimState m) a -> m ()
sortMVBy cmp (MVector off len (MutableArray ma)) =
  stToPrim $ Sam.sortArrayBy cmp ma off len
```

### Example 2: [`Vector`](https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector.html#t:Vector)

Now consider sorting an (immutable) `Vector`, again from the `vector` library.
Since we cannot mutate it, we will return a sorted copy. The most convenient way
here is to thaw to a `MVector` and sort it as we did above.

```hs
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Sort a vector.
sortV :: Ord a => Vector a -> Vector a
sortV = sortVBy compare

-- | Sort a vector using a comparison function.
sortVBy :: (a -> a -> Ordering) -> Vector a -> Vector a
sortVBy cmp v = V.create $ do
  mv <- V.thaw v
  sortMVBy cmp mv -- from Example 1 above
  pure mv
```

We can test it out in GHCI.

```hs
>>> sortV (V.fromList [5,2,6,3,4,1])
[1,2,3,4,5,6]
>>> import Data.Ord (comparing)
>>> sortVBy (comparing length) (V.fromList ["Lunar","11.3","Candle","Magic"])
["11.3","Lunar","Magic","Candle"]
```

### Example 3: List

Let us now try to sort a list, like [`Data.List.sort`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List.html#v:sort)
does. We will need to move the elements from the list into a `MutableArray#`.

I recommend using the [`primitive`](https://hackage.haskell.org/package/primitive-0.9.0.0/docs/Data-Primitive-Array.html)
library for this task. `primitive` provides boxed wrappers over GHC primitive
types and functions to work with them. While it is possible to do this without
any library, it is easiest to use what is already available. If you are unable
to use `primitive`, you can take a peek at the relevant definitions there and
use them directly.

```hs
import Control.Monad.Primitive (stToPrim)
import qualified Data.Foldable as F
import Data.Primitive.Array (MutableArray(..))
import qualified Data.Primitive.Array as A

import qualified Data.SamSort as Sam

-- | Sort a list.
sortL :: Ord a => [a] -> [a]
sortL = sortLBy compare

-- | Sort a list using a comparison function.
sortLBy :: (a -> a -> Ordering) -> [a] -> [a]
sortLBy cmp xs = F.toList $ A.runArray $ do
  let a = A.arrayFromList xs
      n = A.sizeofArray a
  ma@(MutableArray ma') <- A.thawArray a 0 n
  stToPrim $ Sam.sortArrayBy cmp ma' 0 n
  pure ma
```

In GHCI,
```hs
>>> sortL ["Fall","In","The","Dark"]
["Dark","Fall","In","The"]
>>> import Data.Ord (Down, comparing)
>>> sortLBy (comparing Down) [3.4,8.5,9.1,7.9,3.1,6.2]
[9.1,8.5,7.9,6.2,3.4,3.1]
```

> [!TIP]
>
> Avoid `Data.List`'s `sort` and `sortBy` when a large number of elements need
> to be fully sorted and performance is a concern. Sorting lists is quite
> inefficient. Put the elements in a mutable array and use this (or some other)
> sorting library instead.

## Sorting `Int`s

Converting to a `MutableArray#` and sorting, as explained in the above section,
should cover the majority of use cases. However, sometimes it is not the
best option. For instance, we may be storing `Int`s in an unboxed array for
efficiency. Having to pull them out and box them for sorting does not sound
good.

The second function provided by this library is

```hs
sortIntArrayBy
  :: (Int -> Int -> Ordering)  -- ^ comparison
  -> MutableByteArray# s
  -> Int                       -- ^ offset in Int#s
  -> Int                       -- ^ length in Int#s
  -> ST s ()
```

As you might have guessed, this sorts an unboxed array of `Int`s. We can use
this whenever we need to sort `Int`s, or even other types that may be cheaply
converted to and from `Int`s (like `Word`).

### Example 1: [Unboxed `MVector`](https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector-Unboxed-Mutable.html#t:MVector)

Let us sort a mutable unboxed `MVector Int`. Like with the boxed `MVector`,
we do not need to move the elements because the underlying representation is a
`MutableByteArray#`.

```hs
import Control.Monad.Primitive (PrimMonad(..), stToPrim)
import Data.Primitive.ByteArray (MutableByteArray(..))
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed.Base as VUB

import qualified Data.SamSort as Sam

sortVUMInt :: PrimMonad m => VUM.MVector (PrimState m) Int -> m ()
sortVUMInt = sortVUMIntBy compare

sortVUMIntBy
  :: PrimMonad m
  => (Int -> Int -> Ordering) -> VUM.MVector (PrimState m) Int -> m ()
sortVUMIntBy cmp mv = case mv of
  VUB.MV_Int (VP.MVector off len (MutableByteArray ma')) ->
    stToPrim $ Sam.sortIntArrayBy cmp ma' off len
```

> [!WARNING]
>
> Do not try changing the element type above to sort any other unboxed vector.
> `MutableByteArray#` is the underlying representation for many unboxed vectors,
> but it would be incorrect to use the above code if the element type is not
> `Int`.

## Sorting by index

We have now covered sorting boxed values, and sorting `Int`s. What about other
types in unboxed arrays?

### Example 1: [Unboxed `Vector`](https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector-Unboxed.html#t:Vector)

Consider that we need to sort an unboxed vector of some type `a`. The `vector`
library is designed in a way that the underlying representation of an unboxed
vector can be anything depending on the type `a`. We cannot assume anything
about it.

We know that we can index such a vector efficiently. We also know that we can
construct such vectors from an `Int -> a` using the handy `generate` function. We
will use these facts to sort such a vector.

First we will create an `Int` vector, the elements of which will be indices into
the `a` vector. Then we will sort this `Int` vector using a comparison function
that indexes the `a` vector and compares `a`s. Finally, we will construct a
vector with `a`s in the order of the sorted indices.

This technique is general enough that we can sort any flavor of `Vector`
(boxed, `Unboxed`, `Prim`, `Storable`), so let us use `Vector.Generic` to
define the functions.

```hs
import Control.Monad.Primitive (stToPrim)
import Data.Primitive.ByteArray (MutableByteArray(..))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as VPM

import qualified Data.SamSort as Sam

-- | Sort a vector.
sortByIdxVG :: (Ord a, VG.Vector v a) => v a -> v a
sortByIdxVG = sortByIdxVGBy compare

-- | Sort a vector using a comparison function.
sortByIdxVGBy :: VG.Vector v a => (a -> a -> Ordering) -> v a -> v a
sortByIdxVGBy cmp v = VG.generate n (VG.unsafeIndex v . VP.unsafeIndex ixa)
  where
    n = VG.length v
    cmp' i j = cmp (VG.unsafeIndex v i) (VG.unsafeIndex v j)
    ixa = VP.create $ do
      ixma <- VPM.generate n id
      case ixma of
        VPM.MVector off len (MutableByteArray ma') ->
          stToPrim $ Sam.sortIntArrayBy cmp' ma' off len
      pure ixma
```

In fact, this technique is general enough to be used whenever indices can
be sorted, using any method, and not just with this library!

Sorting by index is more beneficial the larger the elements are in memory,
since moving around index `Int`s is cheaper than moving around the elements
themselves.

We can see that the sort works as expected in GHCI.

```hs
>>> import Data.Ord (comparing)
>>> import qualified Data.Vector.Unboxed as VU
>>> let v = VU.fromList [(6,4),(5,4),(1,2)] :: VU.Vector (Int,Int)
>>> sortByIdxVGBy (comparing snd) v
[(1,2),(6,4),(5,4)]
```

And we can see [in benchmarks](https://github.com/meooow25/samsort/tree/master/compare#4-sort-105-int-int-ints-unboxed)
that sorting by index is indeed more efficient than sorting directly, for
elements of type `(Int, Int, Int)` and sort implementations which support both.

## Sorting unboxed arrays of small elements

So sorting by index is more beneficial the larger the element is, but what
about small elements? Perhaps we need to sort an unboxed array of `Word8`s, or
`Float`s?

Our options as seen above are

* Convert to a boxed array and sort. Lots of avoidable allocations and slow
  comparisons.
* Sort by index. Better, but has avoidable allocations in the form of the
  index array.

Neither are ideal. The most efficient way to sort small elements is to sort the
array of such elements directly. Unfortunately, this library cannot be used to
do this because there are only two functions, one to sort boxed values, and one
to sort `Int`s. If we must use this library, sorting by index is the method of
choice. It is not ideal, but it will not be slow either.

The [`primitive-sort`](https://hackage.haskell.org/package/primitive-sort)
library may also be a good choice for this task. It can sort such small elements
efficiently, though it has some drawbacks (not adaptive, cannot sort a slice,
cannot sort using a comparison function, more dependencies).

[`vector-algorithms`](https://hackage.haskell.org/package/vector-algorithms)
is also able to sort small elements, however it turns out to be
[slower in practice](https://github.com/meooow25/samsort/tree/master/compare#5-sort-105-word8s-unboxed).
