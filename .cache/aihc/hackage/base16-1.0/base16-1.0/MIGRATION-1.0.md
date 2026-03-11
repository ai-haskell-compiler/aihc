Migration Guide for 1.0
----

Between the last major version (0.3.2.1) and the current major epoch (1.0), many API-related constructs have changed, and I'd like to justify them here and now so that users may have an immortalized explanation for what is most likely a disruptive change to their code.

## A faster loop

First, I'd like to say that I don't *like* breaking people's code. As an author and maintainer, I try and make sure that any API breakages are justified either by a significant UX improvement, or by a measurable performance increase large enough to warrant such a breakage. As such, I believe both of these criteria are met by the 0.3.x -> 1.0 upgrade: not only is the API safer to use, but the use of type data to establish the provenance of values encoded by this library also allows the performance-sensitive loops to be much cleaner, eschewing error checking where type data suffices. To prove this point, I've benchmarked the library between these last two epochs. The benchmarks say it all (all benchmarks are done on a Thinkpad P15 Gen 2 Intel i9-11950H, 64GB DDR4, Ubuntu 22.04 with GHC 8.10.7 stock, -O2):

In `base16-0.3.2.1`:

```
benchmarking decode/25/base16-bytestring
time                 31.66 ns   (31.64 ns .. 31.69 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 31.62 ns   (31.60 ns .. 31.67 ns)
std dev              113.1 ps   (71.94 ps .. 207.9 ps)

benchmarking decode/25/base16
time                 32.31 ns   (32.27 ns .. 32.35 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 32.33 ns   (32.30 ns .. 32.42 ns)
std dev              178.2 ps   (84.80 ps .. 340.1 ps)

benchmarking decode/100/base16-bytestring
time                 74.31 ns   (74.27 ns .. 74.35 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 74.37 ns   (74.34 ns .. 74.41 ns)
std dev              122.0 ps   (102.0 ps .. 147.8 ps)

benchmarking decode/100/base16
time                 83.74 ns   (83.70 ns .. 83.78 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 83.57 ns   (83.43 ns .. 83.68 ns)
std dev              380.3 ps   (273.2 ps .. 473.3 ps)

benchmarking decode/1k/base16-bytestring
time                 582.5 ns   (582.3 ns .. 582.8 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 582.8 ns   (582.6 ns .. 583.1 ns)
std dev              791.4 ps   (632.0 ps .. 1.101 ns)

benchmarking decode/1k/base16
time                 686.1 ns   (685.7 ns .. 686.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 686.2 ns   (685.9 ns .. 686.6 ns)
std dev              1.086 ns   (910.3 ps .. 1.357 ns)

benchmarking decode/10k/base16-bytestring
time                 5.640 μs   (5.633 μs .. 5.649 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.663 μs   (5.656 μs .. 5.671 μs)
std dev              25.71 ns   (21.29 ns .. 29.83 ns)

benchmarking decode/10k/base16
time                 6.628 μs   (6.609 μs .. 6.649 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.614 μs   (6.609 μs .. 6.622 μs)
std dev              20.78 ns   (12.02 ns .. 37.27 ns)

benchmarking decode/100k/base16-bytestring
time                 58.41 μs   (58.38 μs .. 58.45 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 58.41 μs   (58.38 μs .. 58.45 μs)
std dev              111.5 ns   (90.90 ns .. 152.0 ns)

benchmarking decode/100k/base16
time                 66.40 μs   (66.21 μs .. 66.64 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 66.55 μs   (66.47 μs .. 66.62 μs)
std dev              264.0 ns   (209.8 ns .. 332.3 ns)

benchmarking decode/1mm/base16-bytestring
time                 577.4 μs   (576.6 μs .. 578.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 577.0 μs   (576.5 μs .. 577.6 μs)
std dev              1.997 μs   (1.661 μs .. 2.474 μs)

benchmarking decode/1mm/base16
time                 670.9 μs   (670.3 μs .. 671.5 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 671.1 μs   (670.7 μs .. 671.9 μs)
std dev              2.003 μs   (1.211 μs .. 3.227 μs)
```

vs in `base64-1.0.0.0`:

```
benchmarking decode/25/base16-bytestring
time                 24.29 ns   (24.27 ns .. 24.32 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 24.27 ns   (24.24 ns .. 24.30 ns)
std dev              95.03 ps   (76.90 ps .. 125.9 ps)

benchmarking decode/25/base16
time                 25.64 ns   (25.49 ns .. 25.81 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 25.64 ns   (25.57 ns .. 25.72 ns)
std dev              262.9 ps   (220.7 ps .. 312.9 ps)

benchmarking decode/100/base16-bytestring
time                 75.10 ns   (74.95 ns .. 75.31 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 75.33 ns   (75.25 ns .. 75.40 ns)
std dev              267.5 ps   (202.6 ps .. 340.3 ps)

benchmarking decode/100/base16
time                 60.99 ns   (60.92 ns .. 61.05 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 60.95 ns   (60.89 ns .. 61.03 ns)
std dev              238.1 ps   (186.6 ps .. 325.0 ps)

benchmarking decode/1k/base16-bytestring
time                 606.2 ns   (605.3 ns .. 607.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 606.4 ns   (605.5 ns .. 609.2 ns)
std dev              4.832 ns   (1.865 ns .. 9.636 ns)

benchmarking decode/1k/base16
time                 472.5 ns   (472.0 ns .. 473.0 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 471.6 ns   (471.3 ns .. 472.0 ns)
std dev              1.165 ns   (965.8 ps .. 1.434 ns)

benchmarking decode/10k/base16-bytestring
time                 5.885 μs   (5.881 μs .. 5.890 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.891 μs   (5.888 μs .. 5.895 μs)
std dev              13.03 ns   (10.87 ns .. 16.58 ns)

benchmarking decode/10k/base16
time                 4.560 μs   (4.551 μs .. 4.567 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.549 μs   (4.544 μs .. 4.554 μs)
std dev              16.61 ns   (14.04 ns .. 19.41 ns)

benchmarking decode/100k/base16-bytestring
time                 58.71 μs   (58.56 μs .. 58.84 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 58.59 μs   (58.54 μs .. 58.66 μs)
std dev              201.4 ns   (163.3 ns .. 251.0 ns)

benchmarking decode/100k/base16
time                 45.74 μs   (45.69 μs .. 45.80 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 45.72 μs   (45.67 μs .. 45.78 μs)
std dev              172.5 ns   (146.4 ns .. 209.1 ns)

benchmarking decode/1mm/base16-bytestring
time                 584.6 μs   (583.1 μs .. 586.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 587.8 μs   (586.7 μs .. 589.0 μs)
std dev              3.931 μs   (3.108 μs .. 5.364 μs)

benchmarking decode/1mm/base16
time                 459.0 μs   (458.5 μs .. 459.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 458.9 μs   (458.4 μs .. 459.5 μs)
std dev              1.839 μs   (1.355 μs .. 2.951 μs)
```

Benchmarks are included in this repo for you to reproduce these results on your own. You can see a parity in the `encode` step between the previous library iterations and the new epoch, with a *marked* improvement in decode speed (up to 25% faster on average between the old and new versions in the optimal case, and up to 40% in the suboptimal case) which justifies the performance aspect to me. Without deferring to pipelining instructions, hex encoding can only get so fast. In the future, this change also opens the library up to an optimal SIMD implementations.

## A sounder api

Second, I do not believe that these changes are unsound or overburdensome to the point that a migration to the new paradigm would be untenable. While it may be inconvenient to unwrap `Base16` types, in the `encode` case (all one must do is call `extractBase16` to extract the value from its wrapper, all caveats implied), and in the case of `decode`, an untyped variant is supplied, and is semantically consistent with the old behavior (the loop is the same). Hence, a migration is fairly easy to sketch out:

```
"encodeBase16'" -> "extractBase16 . encodeBase16'"
"encodeBase16" -> "extractBase16 . encodeBase16"
"decodebase16" -> "decodeBase16Untyped"
"decodeBase16Unpadded" -> "decodeBase16UnpaddedUntyped"
"decodeBase16Padded" -> "decodeBase16PaddedUntyped"
"decodeBase16W*With" -> "decodeBase16*WithUntyped"
```

And that is all. In order to make use of the new loops, one must only `assertBase16` and proceed with using `decodeBase16` as usual in order to decode. You'll note that an untyped `encodeBase16` is not supplied, and this is due to the fact that it's trivial to extract a `Base16` encoded value once you have it. However, I want to encourage people to use the new API, so I have only supplied a decode with error checking in the untyped case, because sometimes we deal with other people's data and cannot establish provenance. In the encode case, I would rather keep that provenance a part of the API, and the user may opt to strip that data upon sending to others or around their systems. It's not my problem at that point!
