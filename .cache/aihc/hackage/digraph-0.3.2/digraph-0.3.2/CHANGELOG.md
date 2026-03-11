# Revision history for digraph

## 0.3.2 -- 2023-04-01

* Fix spelling of Petersen graph. The old function name `petersonGraph` is
  marked as deprecated and will be removed in the next major version. The new
  function name is `petersenGraph`.

## 0.3.1 -- 2023-03-31

* Add some degree-diameter graphs to the list of known graphs. These graphs
  represent the best currently known solutions for the respective degree and
  diameter. In concrete the graphs `d3k4`, `d4k3`, `d4k4`, `d5k3`, and `d5k4`
  have been added.

## 0.3.0 -- 2023-02-03

* Fix `fromAdjacencySets` to preserve edge direction. This affects the functions
  for computing shortest paths, distance, and diameter. These functions now
  return correct results for directed graphs. Before these functions silently
  turned the input into an undirected graph. (Contributed by
  [Geometer1729](https://github.com/Geometer1729))

* Add `pentagon` and `ascendingCube` to the list of known graphs.
  (Contributed by [Geometer1729](https://github.com/Geometer1729))

## 0.2.3 -- 2023-02-02

* Support ghc-9.2 and ghc-9.4

## 0.2.2 -- 2021-08-02

* Support for `massiv >=1.01`

## 0.2.1 -- 2021-06-24

* Build with GHC-9

## 0.2 -- 2020-05-19

* Fix the `twentyChainGraph` to have diameter 3 and match the respective graph
  from the chainweb paper.

* Allow build with base < 4.15

## 0.1.0.2 -- 2019-06-03

* Haddocks fixed for older Haddock versions.

## 0.1.0.1 -- 2019-05-31

* Support for `hashable >=1.2`.

## 0.1.0.0 -- 2019-05-30

* First version. Released on an unsuspecting world.
