# withdependencies

[![Haskell-CI](https://github.com/bartavelle/withdependencies/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/bartavelle/withdependencies/actions/workflows/haskell-ci.yml)

Model computation with dependencies, and evaluate them over streams of elements.

This module was created to handle tree-like computations that require elements from a stream that might arrive in arbitrary order.

The particular use case was a computation requiring several files from a tar archive. The archive would be parsed as a stream of files, and the computation would end as soon as all required files would be traversed.

