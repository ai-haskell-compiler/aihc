# [mpi-hs-cereal](https://github.com/eschnett/mpi-hs-cereal)

[MPI](https://www.mpi-forum.org) bindings for Haskell

* [GitHub](https://github.com/eschnett/mpi-hs-cereal): Source code repository
* [Hackage](http://hackage.haskell.org/package/mpi-hs-cereal): Haskell
  package and documentation
* [Stackage](https://www.stackage.org/package/mpi-hs-cereal): Stackage
  snapshots
* [Azure
  Pipelines](https://dev.azure.com/schnetter/mpi-hs-cereal/_build):
  Build Status [![Build
  Status](https://dev.azure.com/schnetter/mpi-hs-cereal/_apis/build/status/eschnett.mpi-hs-cereal?branchName=master)](https://dev.azure.com/schnetter/mpi-hs-cereal/_build/latest?definitionId=8&branchName=master)



## Overview

This is a companion package to
[`mpi-hs`](https://github.com/eschnett/mpi-hs). Read the documentation
there.

This package `mpi-hs-cereal` provides a high-level interface based on
`Data.Serialize` in the
[`cereal`](https://hackage.haskell.org/package/cereal) package. This
is a separate package to reduce dependencies for `mpi-hs`. Note that
`mpi-hs` has a similar high-level interface based on `Data.Storable`
already built in.

### Running the tests

There is one test case provided in `tests`:

```
stack build --test --no-run-tests
mpiexec -n 3 stack exec -- $(stack path --dist-dir)/build/mpi-test-cereal/mpi-test-cereal
```
