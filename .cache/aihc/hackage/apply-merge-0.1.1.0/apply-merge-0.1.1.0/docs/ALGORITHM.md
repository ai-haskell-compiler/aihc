<!--
SPDX-FileCopyrightText: Copyright Preetham Gujjula
SPDX-License-Identifier: BSD-3-Clause
-->

# Motivation

## The problem
Say that we want to compute the [3-smooth numbers](https://en.wikipedia.org/wiki/Smooth_number),
i.e., the integers that can be written in the form $2^{i} 3^{j}$, where $i$ and
$j$ are non-negative integers.

It's easy enough to compute the powers of 2 and 3:

```haskell
powersOf2 :: [Integer]
powersOf2 = iterate (*2) 1

powersOf3 :: [Integer]
powersOf3 = iterate (*3) 1
```

But how do we implement the following?

```haskell
-- take 10 smooth3 == [1, 2, 3, 4, 6, 9, 12, 16, 18, 24]
smooth3 :: [Integer]
smooth3 = ..
```

If we limited `powersOf2` and `powersOf3`, say to the first 10 elements, we
could write:

```haskell
powersOf2Bounded :: [Integer]
powersOf2Bounded = take 10 (iterate (*2) 1)

powersOf3Bounded :: [Integer]
powersOf3Bounded = take 10 (iterate (*3) 1)

smooth3Bounded :: [Integer]
smooth3Bounded = sort (liftA2 (*) powersOf2Bounded powersOf3Bounded)
```

And `smooth3Bounded` would consist of $\\{2^{i} 3^{j} \mid 0 \le i, j < 10\\}$.
But this doesn't work when `powersOf2` and `powersOf3` are infinite. We need a
way to lazily generate `smooth3` given `powersOf2` and `powersOf3`.

## A closer look
Let's lay out the elements of `smooth3` in a 2D grid:

<pre>
    |    1     2     4     8    ..
 ---------------------------------
  1 |    1     2     4     8    ..
    |
  3 |    3     6    12    24    ..
    |
  9 |    9    18    36    72    ..
    |
 27 |   27    54   108   216    ..
    |
  : |    :     :     :     :    ॱ.
</pre>

Since `(*)` is monotonically increasing in both arguments, each element in the
grid is ≥ its neighbors above and to the left. Therefore, when an element
appears in `smooth3`, its up- and left-neighbors must already have appeared.

Let's think about `smooth3` after 3 elements have been produced:

<pre>
    |    1     2     4     8    ..
 ---------------------------------
  1 |    <del>1</del>     <del>2</del>     <b>4</b>     8    ..
    |
  3 |    <del>3</del>     <b>6</b>    12    24    ..
    |
  9 |    <b>9</b>    18    36    72    ..
    |
 27 |   27    54   108   216    ..
    |
  : |    :     :     :     :    ॱ.
</pre>

After producing `1, 2, 3`, the next element in `smooth3` can only be one of
`{4, 6, 9}`. We know this without performing any comparisons, just by the
positions of these elements in the grid, as these are the only elements whose
up- and left-neighbors have already been produced.

After producing the next element, `4`, our grid becomes

<pre>
    |    1     2     4     8    ..
 ---------------------------------
  1 |    <del>1</del>     <del>2</del>     <del>4</del>     <b>8</b>    ..
    |
  3 |    <del>3</del>     <b>6</b>    12    24    ..
    |
  9 |    <b>9</b>    18    36    72    ..
    |
 27 |   27    54   108   216    ..
    |
  : |    :     :     :     :    ॱ.
</pre>

Now, the "frontier" of possible next elements is `{6, 8, 9}`. We added `8` to
the frontier as it is no longer "blocked" by `4`. Note that we cannot yet add
`12` to the frontier even though it's no longer blocked by `4`, as it's still
blocked by `6`.

After producing the next element, `6`, our grid becomes

<pre>
    |    1     2     4     8    ..
 ---------------------------------
  1 |    <del>1</del>     <del>2</del>     <del>4</del>     <b>8</b>    ..
    |
  3 |    <del>3</del>     <del>6</del>    <b>12</b>    24    ..
    |
  9 |    <b>9</b>    18    36    72    ..
    |
 27 |   27    54   108   216    ..
    |
  : |    :     :     :     :    ॱ.
</pre>

Now, the frontier consists of `{8, 9, 12}`. We added `12` as it is now unblocked
by `4` and `6`. We cannot yet add `18`, as it is still blocked by `9`.

This investigation suggests the following algorithm for producing `smooth3`:

```
1. Initialize a "frontier" with the top left element
2. Delete the smallest element z from the frontier and yield it as the next element of smooth3.
3. If the down- or right-neighbors of z are unblocked, add them to the frontier.
4. Go to step 2.
```

# The `applyMerge` function

The idea in the algorithm above is quite general, and instead of using it on
`(*)`, `powersOf2`, and `powersOf3`, we can apply it to any binary function `f`
that is non-decreasing in both arguments, and any (potentially infinite) ordered
lists `xs` and `ys`. Then we can define a general

```haskell
applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
```

where `applyMerge f xs ys` is an ordered list of all `f x y`, for each `x` in `xs`
and `y` in `ys`. In particular, `smooth3 = applyMerge (*) powersOf2 powersOf3`.

## Implementation and complexity

We can use a priority queue to maintain the frontier of elements. To determine
in step 3 whether a down- or right-neighbor is unblocked, we maintain two
`IntSet`s, for the `x`- and `y`- indices of all elements in the frontier. Then
in step 3, if the `x`- and `y`-index of a neighbor are not in the respective
`IntSet`s, the neighbor is unblocked and can be added to the frontier.

After producing $n$ elements, the size of the frontier is at most $O(\sqrt{n})$
elements. <i>(TODO: add a proof of this)</i> Manipulating the priority queue and
the `IntSet`s to produce the next element takes
$O(\text{log } \sqrt{n}) = O(\text{log } n)$ time. Therefore, producing $n$
elements of `applyMerge f xs ys` takes $O(n \log n)$ time and $O(\sqrt{n})$
auxiliary space, assuming that `f` and `compare` take $O(1)$ time.

### Note about memory usage
Note that `applyMerge` retains the input lists in memory, which could cause
unexpected memory usage when the input lists are lazily generated. For example,
```
sum (take n (applyMerge const [1 :: Int ..] [1 :: Int ..]))
```
requires retaining the first $n$ elements of the second list, and so uses $O(n)$
space. Constrast this with
```
sum (take n (applyMerge (+) [1 :: Int ..] [1 :: Int ..]))
```
which requires retaining the first $O(\sqrt{n})$ elements of each list, and uses
$O(\sqrt{n})$ space.

## More examples

With `applyMerge`, we can implement a variety of complex algorithms succinctly.
For example, the Sieve of Erastosthenes[^1] to generate prime numbers:

```haskell
primes :: [Int]
primes = 2 : ([3..] `minus` composites)    -- `minus` from data-ordlist

composites :: [Int]
composites = applyMerge (*) primes [2..]
```

Gaussian integers, ordered by norm ([Wikipedia](https://en.wikipedia.org/wiki/Gaussian_integer)):

```haskell
zs :: [Integer]
zs = 0 : concatMap (\i -> [i, -i]) [1..]

gaussianIntegers :: [GaussianInteger]      -- `GaussianInteger` from arithmoi
gaussianIntegers = map snd (applyMerge (\x y -> (norm (x :+ y), x :+ y)) zs zs)
```

Square-free integers ([Wikipedia](https://en.wikipedia.org/wiki/Square-free_integer)):

```haskell
squarefrees :: [Int]
squarefrees = [1..] `minus` applyMerge (*) (map (^2) primes) [1..]
```

# Prior work

## mergeAll from data-ordlist

In <code>[data-ordlist](https://www.stackage.org/lts/package/data-ordlist)</code>,
there is <code>[mergeAll](https://www.stackage.org/haddock/lts/data-ordlist/Data-List-Ordered.html#v:mergeAll) :: Ord a => [[a]] -> [a]</code>,
which merges a potentially infinite list of ordered lists, where the heads of
the inner lists are sorted. This is more general than `applyMerge`, in the sense
that we can implement `applyMerge` in terms of `mergeAll`:

```haskell
applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge f xs ys =
  mergeAll (map (\y -> map (\x -> f x y) xs) ys)
```

However, `mergeAll` uses $O(n)$ auxiliary space in the worst case, while our
implementation of `applyMerge` uses just $O(\sqrt{n})$ auxiliary space.

## Skiena's algorithm

In [The Algorithm Design Manual](https://doi.org/10.1007%2F978-1-84800-070-4_4),
Steven Skiena describes an algorithm for minimizing the sum of two airline
ticket fares:

> “Got it!,” I said. “We will keep track of index pairs in a priority queue,
> with the sum of the fare costs as the key for the pair. Initially we put only
> pair (1, 1) on the queue. If it proves it is not feasible, we put its two
> successors on—namely (1, 2) and (2, 1). In general, we enqueue pairs
> (i + 1, j) and (i, j + 1) after evaluating/rejecting pair (i, j). We will get
> through all the pairs in the right order if we do so.”
>
> The gang caught on quickly. “Sure. But what about duplicates? We will
> construct pair (x, y) two different ways, both when expanding (x − 1, y) and
> (x, y −1).”
>
> “You are right. We need an extra data structure to guard against duplicates.
> The simplest might be a hash table to tell us whether a given pair exists in
> the priority queue before we insert a duplicate. In fact, we will never have
> more than n active pairs in our data structure, since there can only be one
> pair for each distinct value of the first coordinate.”

This is similar to the `applyMerge` algorithm, except that `applyMerge` has an
optimization to check that we don’t add (x, y) to the priority queue when there
is already an (x′, y) with x < x′ or (x, y′) with y < y′ in the queue.

[^1]: Note that this is really the Sieve of Erastosthenes, as defined in the classic [The Genuine Sieve of Eratosthenes](https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf). Constrast this to other simple prime generation implementations, such as <pre> primes = sieve [2..] where sieve (p : xs) = p : sieve [x | x <- xs, x \`rem\` p > 0]</pre> which is actually trial division and not a faithful implementation of the Sieve of Erastosthenes.
