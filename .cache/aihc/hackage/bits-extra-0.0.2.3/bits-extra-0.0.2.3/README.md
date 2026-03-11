# bits-extra
[![CircleCI](https://circleci.com/gh/haskell-works/bits-extra.svg?style=svg)](https://circleci.com/gh/haskell-works/bits-extra)
[![Travis](https://travis-ci.org/haskell-works/bits-extra.svg?branch=master)](https://travis-ci.org/haskell-works/bits-extra)

Useful bitwise operations.

This library exposes support for some BMI2 CPU instructions on some x86 based
CPUs.

Currently support operations include:

* `pdep` - Parallel Deposit
* `pext` - Parallel Extract

These operations are useful for high performance bit manipulation for
applications such as succinct data structures.

In the case where the target CPU architectures do not support these
instruction set, a very slow emulated version of these operations is
provided instead.

Note `ghc-8.4.1` is required to target the BMI2 CPU instructions.

## Compilation

It is sufficient to build, test and benchmark the library as follows
for emulated behaviour:

```text
stack build
stack test
stack bench
```

To target the BMI2 instruction set, add the `bmi2` flag:

```text
stack build --flag bits-extra:bmi2
stack test  --flag bits-extra:bmi2
stack bench --flag bits-extra:bmi2
```

## Benchmark results

Benchmarks with `bmi2` flag defined on `ghc-8.4.1` on `Intel Core i7 2.9 GHz`

```text
Benchmark bench: RUNNING...
Fast pdep enabled: True
Fast pext enabled: True
benchmarking popCount-single/popCount64
time                 6.698 ns   (6.634 ns .. 6.766 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 6.750 ns   (6.692 ns .. 6.841 ns)
std dev              243.5 ps   (195.3 ps .. 347.1 ps)
variance introduced by outliers: 60% (severely inflated)

benchmarking popCount-single/popCount32
time                 5.543 ns   (5.511 ns .. 5.574 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 5.596 ns   (5.546 ns .. 5.650 ns)
std dev              177.0 ps   (144.8 ps .. 215.8 ps)
variance introduced by outliers: 54% (severely inflated)

benchmarking pdep-vector/popCount64
time                 1.083 μs   (1.074 μs .. 1.097 μs)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 1.089 μs   (1.075 μs .. 1.104 μs)
std dev              50.20 ns   (37.87 ns .. 70.22 ns)
variance introduced by outliers: 62% (severely inflated)

benchmarking pdep-vector/popCount32
time                 1.074 μs   (1.065 μs .. 1.083 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.079 μs   (1.071 μs .. 1.090 μs)
std dev              31.57 ns   (23.09 ns .. 49.88 ns)
variance introduced by outliers: 40% (moderately inflated)

benchmarking pdep-single/pdep64
time                 4.920 ns   (4.856 ns .. 5.007 ns)
                     0.996 R²   (0.990 R² .. 1.000 R²)
mean                 4.945 ns   (4.874 ns .. 5.125 ns)
std dev              361.5 ps   (150.9 ps .. 703.7 ps)
variance introduced by outliers: 86% (severely inflated)

benchmarking pdep-single/pdep32
time                 5.203 ns   (5.163 ns .. 5.244 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 5.229 ns   (5.193 ns .. 5.268 ns)
std dev              126.2 ps   (109.9 ps .. 148.6 ps)
variance introduced by outliers: 41% (moderately inflated)

benchmarking pdep-vector/pdep64
time                 1.627 μs   (1.617 μs .. 1.641 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.679 μs   (1.652 μs .. 1.723 μs)
std dev              107.2 ns   (74.61 ns .. 155.4 ns)
variance introduced by outliers: 75% (severely inflated)

benchmarking pdep-vector/pdep32
time                 1.626 μs   (1.621 μs .. 1.634 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.637 μs   (1.624 μs .. 1.651 μs)
std dev              44.27 ns   (35.00 ns .. 58.61 ns)
variance introduced by outliers: 35% (moderately inflated)

benchmarking pext-single/pext64
time                 5.182 ns   (5.133 ns .. 5.239 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 5.255 ns   (5.181 ns .. 5.371 ns)
std dev              314.1 ps   (211.7 ps .. 441.7 ps)
variance introduced by outliers: 81% (severely inflated)

benchmarking pext-single/pext32
time                 5.269 ns   (5.222 ns .. 5.340 ns)
                     0.997 R²   (0.993 R² .. 1.000 R²)
mean                 5.315 ns   (5.248 ns .. 5.513 ns)
std dev              388.9 ps   (145.5 ps .. 713.4 ps)
variance introduced by outliers: 86% (severely inflated)

benchmarking pext-vector/pext64
time                 1.647 μs   (1.632 μs .. 1.667 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 1.673 μs   (1.649 μs .. 1.719 μs)
std dev              107.5 ns   (67.19 ns .. 171.9 ns)
variance introduced by outliers: 76% (severely inflated)

benchmarking pext-vector/pext32
time                 1.626 μs   (1.616 μs .. 1.637 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.630 μs   (1.617 μs .. 1.649 μs)
std dev              53.18 ns   (33.93 ns .. 80.86 ns)
variance introduced by outliers: 44% (moderately inflated)

benchmarking plus-vector/plus64
time                 1.641 μs   (1.628 μs .. 1.657 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.647 μs   (1.635 μs .. 1.666 μs)
std dev              50.67 ns   (38.54 ns .. 76.92 ns)
variance introduced by outliers: 41% (moderately inflated)

benchmarking plus-vector/plus32
time                 1.943 μs   (1.932 μs .. 1.953 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.948 μs   (1.934 μs .. 1.961 μs)
std dev              45.86 ns   (37.10 ns .. 59.21 ns)
variance introduced by outliers: 29% (moderately inflated)

Benchmark bench: FINISH
```

Benchmarks with `bmi2` flag NOT defined on `ghc-8.4.1` on `Intel Core i7 2.9 GHz`

```
Benchmark bench: RUNNING...
Fast pdep enabled: False
Fast pext enabled: False
benchmarking popCount-single/popCount64
time                 6.572 ns   (6.433 ns .. 6.765 ns)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 6.637 ns   (6.551 ns .. 6.758 ns)
std dev              346.9 ps   (252.8 ps .. 542.9 ps)
variance introduced by outliers: 76% (severely inflated)

benchmarking popCount-single/popCount32
time                 5.196 ns   (5.152 ns .. 5.253 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 5.232 ns   (5.186 ns .. 5.293 ns)
std dev              186.6 ps   (152.8 ps .. 237.0 ps)
variance introduced by outliers: 60% (severely inflated)

benchmarking pdep-vector/popCount64
time                 5.409 μs   (5.344 μs .. 5.476 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 5.458 μs   (5.391 μs .. 5.549 μs)
std dev              258.1 ns   (193.1 ns .. 374.6 ns)
variance introduced by outliers: 60% (severely inflated)

benchmarking pdep-vector/popCount32
time                 3.849 μs   (3.814 μs .. 3.885 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 3.860 μs   (3.818 μs .. 3.919 μs)
std dev              166.6 ns   (131.0 ns .. 236.7 ns)
variance introduced by outliers: 56% (severely inflated)

benchmarking pdep-single/pdep64
time                 10.46 ns   (10.35 ns .. 10.58 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 10.53 ns   (10.42 ns .. 10.65 ns)
std dev              385.2 ps   (321.8 ps .. 480.9 ps)
variance introduced by outliers: 60% (severely inflated)

benchmarking pdep-single/pdep32
time                 10.57 ns   (10.49 ns .. 10.68 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 10.69 ns   (10.57 ns .. 10.80 ns)
std dev              404.1 ps   (341.0 ps .. 501.8 ps)
variance introduced by outliers: 62% (severely inflated)

benchmarking pdep-vector/pdep64
time                 3.113 μs   (3.091 μs .. 3.140 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.130 μs   (3.106 μs .. 3.158 μs)
std dev              82.60 ns   (65.72 ns .. 103.1 ns)
variance introduced by outliers: 32% (moderately inflated)

benchmarking pdep-vector/pdep32
time                 3.096 μs   (3.070 μs .. 3.125 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.086 μs   (3.059 μs .. 3.114 μs)
std dev              91.30 ns   (77.41 ns .. 110.3 ns)
variance introduced by outliers: 37% (moderately inflated)

benchmarking pext-single/pext64
time                 73.06 ns   (72.35 ns .. 73.84 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 73.65 ns   (73.02 ns .. 74.35 ns)
std dev              2.368 ns   (1.974 ns .. 2.918 ns)
variance introduced by outliers: 50% (severely inflated)

benchmarking pext-single/pext32
time                 74.07 ns   (73.46 ns .. 74.62 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 74.18 ns   (73.41 ns .. 74.83 ns)
std dev              2.550 ns   (2.161 ns .. 3.134 ns)
variance introduced by outliers: 54% (severely inflated)

benchmarking pext-vector/pext64
time                 72.78 μs   (71.21 μs .. 74.87 μs)
                     0.996 R²   (0.993 R² .. 0.999 R²)
mean                 72.17 μs   (71.38 μs .. 73.41 μs)
std dev              3.105 μs   (2.302 μs .. 4.741 μs)
variance introduced by outliers: 46% (moderately inflated)

benchmarking pext-vector/pext32
time                 72.69 μs   (72.04 μs .. 73.28 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 73.00 μs   (72.25 μs .. 73.85 μs)
std dev              2.634 μs   (2.237 μs .. 3.137 μs)
variance introduced by outliers: 37% (moderately inflated)

benchmarking plus-vector/plus64
time                 1.856 μs   (1.833 μs .. 1.876 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 1.861 μs   (1.844 μs .. 1.880 μs)
std dev              63.39 ns   (54.74 ns .. 75.83 ns)
variance introduced by outliers: 46% (moderately inflated)

benchmarking plus-vector/plus32
time                 1.547 μs   (1.532 μs .. 1.562 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.554 μs   (1.538 μs .. 1.578 μs)
std dev              62.25 ns   (48.22 ns .. 91.40 ns)
variance introduced by outliers: 54% (severely inflated)

Benchmark bench: FINISH
```

## Reference

* [Bit Manipulation Instruction Sets (Wikipedia)](https://en.wikipedia.org/wiki/Bit_Manipulation_Instruction_Sets)
* [GHC differential D4236](https://phabricator.haskell.org/D4236)

## Acknowledgement

A lot of people helped in the writing of this library and the underlying GHC patch:

* Ben Gamari
* Moritz Angermann
* Alex Mason
* Moritz Kiefer
