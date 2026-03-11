# rng-utils

This simple utility wraps an IORef around random for the common use
case of initializing an RNG once and using it in various places within
an IO context. The haddocks should otherwise be self explanatory.

Any comments, improvement opportunities, feedback most welcome.


## Upgrade Notes

In version 0.3.0, rng-utils switched from targeting `mwc-random` to
`random`. `random` sees more use in the Haskell ecosystem, has fewer
dependencies and the performance difference was negligible in
comparison to the concurrency mechanism. If you feel strongly that
`mwc-random` should be part of the library, let us know in an issue
and we can look into supporting multiple backends.
