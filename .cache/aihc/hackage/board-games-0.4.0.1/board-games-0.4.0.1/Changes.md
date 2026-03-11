# Change log for the `board-games` package

## 0.4

 * `Labyrinth`: New game "Das verrückte Labyrinth".

## 0.3

 * `Mastermind.CodeSet`:
   Move from `Set` to `EnumSet` since this is three to ten times faster.
   It works best for alphabets with contiguous `fromEnum`-associated `Int`s,
   but even if not it should not be much worse than `Set`.
   If this is still too slow for you, you might consider mapping your alphabet
   to a contiguous set of `Int`s first.
   I tried to maintain both `Set` and `EnumSet` in one interface.
   It is possible even in Haskell 98 using explicit method dictionaries.
   However, it gets complicated and I am afraid
   that the speed advantage is diminished by the generalization overhead.

## 0.2.1

 * add criterion benchmarks for `Mastermind`

## 0.2

 * improved game strategy for `Mastermind`

## 0.1

 * hierarchical module names
