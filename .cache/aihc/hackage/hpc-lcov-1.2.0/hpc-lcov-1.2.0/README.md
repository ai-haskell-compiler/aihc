# hpc-lcov

[![GitHub Actions](https://img.shields.io/github/actions/workflow/status/brandonchinn178/hpc-lcov/ci.yml?branch=main)](https://github.com/brandonchinn178/hpc-lcov/actions?query=branch%3Amain)
[![codecov](https://codecov.io/gh/brandonchinn178/hpc-lcov/branch/main/graph/badge.svg?token=8TErU2ntw9)](https://codecov.io/gh/brandonchinn178/hpc-lcov)
[![Hackage](https://img.shields.io/hackage/v/hpc-lcov)](https://hackage.haskell.org/package/hpc-lcov)

Convert HPC output into `lcov.info` files that can be uploaded to coverage
services, like [Codecov](https://codecov.io).

## Quickstart

1. Download `hpc-lcov` from the releases page or install it from Hackage with your favorite package manager
1. Run your tests with coverage enabled
    * `stack test --coverage`
    * Cabal is not yet supported (https://github.com/brandonchinn178/hpc-lcov/issues/3)
1. Run `hpc-lcov`
1. Upload the generated `lcov.info` file to your coverage service

## FAQs

### How do I convert coverage for an executable?

Note: If you have both tests and executables, HPC will write module information to the same file. Because of this, you'll have to load the coverage for each separately, with a `stack clean` in between.

1. Build a single executable with coverage enabled (e.g. `stack build :my-exe --coverage`)
1. Run the executable
1. This should generate a `.tix` file in the current directory
1. Run the following, specifying the package that builds the executable:
    ```bash
    stack exec -- hpc-lcov --file my-exe.tix --main-package my-package
    ```

### How do I merge coverage files?

1. Install LCOV (e.g. `brew install lcov`)
1. Run

    ```bash
    lcov -a lcov1.info -a lcov2.info ... > lcov.info
    ```

## Resources

* [LCOV format description](http://ltp.sourceforge.net/coverage/lcov/geninfo.1.php)
* [More info on HPC](https://wiki.haskell.org/Haskell_program_coverage)
