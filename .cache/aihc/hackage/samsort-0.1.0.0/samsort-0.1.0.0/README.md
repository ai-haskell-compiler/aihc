# samsort

[![Hackage](https://img.shields.io/hackage/v/samsort?logo=haskell&color=blue)](https://hackage.haskell.org/package/samsort)
[![Haskell-CI](https://github.com/meooow25/samsort/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/meooow25/samsort/actions/workflows/haskell-ci.yml)

A stable adapative mergesort implementation

## Features

This is a lightweight library offering two high performance sort functions:

* `sortArrayBy` sorts a GHC [`MutableArray#`](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exts.html#t:MutableArray-35-)
  of boxed elements in place.
* `sortIntArrayBy` sorts a GHC [`MutableByteArray#`](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exts.html#t:MutableByteArray-35-)
  of `Int#`s in place.

There are no dependencies outside of `base`. This means that this library is
not tied to array abstractions from any particular library. This also means
that you may need to write a few lines of code to get a `MutableArray#` or
`MutableByteArray#` from your data, which can then be sorted. See
[`HOWTO.md`](https://github.com/meooow25/samsort/blob/master/HOWTO.md)
for a guide.

If you need to use this library in an environment where you cannot depend on
other packages, you may simply copy the lone source file
[`src/Data/SamSort.hs`](https://github.com/meooow25/samsort/blob/master/src/Data/SamSort.hs)
to your project.

## Algorithm details

* The sort is a comparison-based $O(n \log n)$ mergesort.
* The sort is stable, i.e. the order of equal elements in the input is
  preserved.
* The sort is adaptive, i.e. the sort identifies and uses ascending and
  descending runs of elements occuring in the input to perform less work. As a
  result, the sort is $O(n)$ for already sorted inputs.
* The sort is the fastest among implementations from other libraries in most
  scenarios. [See the benchmarks](https://github.com/meooow25/samsort/tree/master/compare)
  for details.

## Known issues

Ideally, this library would offer only an algorithm, capable of sorting arrays
of any flavor. To support different arrays we would need to rely on some
abstraction, either from another library (like `vector`), or created here. We
cannot do either of those while also keeping the library as lightweight as it
is now.

## Contributing

Questions, bug reports, documentation improvements, code contributions welcome!
Please [open an issue](https://github.com/meooow25/samsort/issues) as the first
step. Slow performance counts as a bug!
