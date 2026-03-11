# nonempty-zipper

A non-empty comonadic list zipper

## Why not another Zipper implementation?

A [Hoogle search][hoogle-search] reveals various implementations of zippers.

The two that have the most similar feature sets to `nonempty-zipper` are
[list-zipper][list-zipper] and [non-empty-zipper][non-empty-zipper].

[list-zipper][list-zipper] has a solid lens interface, and a ton of instances,
however it's lacking documentation and an `NFData` instance.

[non-empty-zipper][non-empty-zipper] is most similar to this package but it
appears to be unmaintained (e.g. `base` has drifted).

  [hoogle-search]: https://hoogle.haskell.org/?hoogle=zipper
  [list-zipper]: https://hackage.haskell.org/package/list-zipper
  [non-empty-zipper]: https://hackage.haskell.org/package/non-empty-zipper
