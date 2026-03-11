
# Pool adjacent violators algorithm

<p align="center"><img src="https://travis-ci.org/dschrempf/pava.svg?branch=master"/></p>

Compute **greatest convex majorants** and **least concave minorants** using the pool
adjacent violators algorithm.

The pool adjacent violators algorithm (PAVA) is an iterative algorithm for
solving monotonic regression problems. In particular, (antitonic) regression is
the computation of a non-decreasing (non-increasing) sequence of values such
that a given problem is optimized. PAVA can also be used to compute the greatest
convex minorant and the least concave majorant of a given set of observables.

At the moment, greatest convex majorants and least concave minorants can be
computed efficiently. More general isotonic regression is not yet supported, but
may be in future releases.


## Reading list

-   A good [introduction to PAVA](https://repository.tudelft.nl/islandora/object/uuid:5a111157-1a92-4176-9c8e-0b848feb7c30?collection=education).
-   [Isotone regression in R](https://cran.r-project.org/web/packages/isotone/index.html).
-   [Computation of greatest convex minorants in R](http://search.r-project.org/library/fdrtool/html/gcmlcm.html).
-   [Wikipedia article on isotonic regression](https://en.wikipedia.org/wiki/Isotonic_regression).


## Run times

Run times with random exponential vectors of different lengths:

    cabal bench 2>&1

    pava> benchmarks
    Running 1 benchmarks...
    Benchmark pava-bench: RUNNING...
    benchmarking Greatest convex minorant/Vector of length 1e3
    time                 397.8 μs   (384.9 μs .. 421.6 μs)
                         0.984 R²   (0.959 R² .. 0.999 R²)
    mean                 392.7 μs   (387.4 μs .. 406.6 μs)
    std dev              30.48 μs   (10.82 μs .. 55.79 μs)
    variance introduced by outliers: 67% (severely inflated)
    
    benchmarking Greatest convex minorant/Vector of length 1e4
    time                 3.806 ms   (3.689 ms .. 3.892 ms)
                         0.959 R²   (0.869 R² .. 0.999 R²)
    mean                 4.001 ms   (3.859 ms .. 4.631 ms)
    std dev              801.4 μs   (123.5 μs .. 1.807 ms)
    variance introduced by outliers: 88% (severely inflated)
    
    benchmarking Greatest convex minorant/Vector of length 1e5
    time                 39.91 ms   (38.41 ms .. 41.42 ms)
                         0.996 R²   (0.993 R² .. 0.998 R²)
    mean                 40.01 ms   (39.19 ms .. 41.10 ms)
    std dev              1.906 ms   (1.265 ms .. 2.777 ms)
    variance introduced by outliers: 13% (moderately inflated)
    
    Benchmark pava-bench: FINISH

