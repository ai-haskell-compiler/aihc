# Change log for the `set-cover` package

## 0.1

 * `SetCover.Exact.decisionTree`, `SetCover.Exact.Priority.decisionTree`:
   Allow the programmer to generate human-friendly solutions.

 * `SetCover.Cuboid`: `dx`, `dy`, `dz` -> `rotX`, `rotY`, `rotZ`

 * `SetCover.Bit`: method `complement` replaced by `difference`.
   This way, we do not need the cumbersome `SetCover.IntSet` module anymore.

 * `SetCover.BitMap` made private.

 * `SetCover.BitPriorityQueue` made public.

 * `SetCover.Exact.State.usedSubsets`: Only store labels, not assignments.
   This is consistent with `SetCover.Exact.Priority.State`.

 * `SetCover.Exact.minimize`: allow an empty list of available subsets
   `SetCover.Exact.step`, `SetCover.Exact.Priority.step`:
   They do not need to test for an empty `availableSubset` anymore.

 * `SetCover.Exact.step`: Require non-empty set of free elements.
   This is consistent with `SetCover.Exact.Priority.step`.
   Until now, `step` returned an empty list if the were no free elements.
   This is not very helpful
   since it will throw away already completed solutions.
   The test is also redundant when `step` is called from `search`.

## 0.0.8

 * `SetCover.Exact.Priority` implements the Algorithm X
   using a priority queue that registers the sets
   each element is contained in.
   This allows for drastic speedup of the `Nonogram` example.

## 0.0.7

 * `ESC.bitVectorFromSetAssigns` allows to turn sets
   into bit vectors without manual bit position gymnastics.

 * Use it in `Sudoku` and `Nonogram` examples.

## 0.0.6

 * Add `SetCover.Exact.Set` instances for `Map`, `Integer`, `IntSet`.

## 0.0.5.1

 * `example/Nonogram`: explore different encodings of the problem
