# hackage-revdeps [![Hackage](http://img.shields.io/hackage/v/hackage-revdeps.svg)](https://hackage.haskell.org/package/hackage-revdeps) [![Stackage LTS](http://stackage.org/package/hackage-revdeps/badge/lts)](http://stackage.org/lts/package/hackage-revdeps) [![Stackage Nightly](http://stackage.org/package/hackage-revdeps/badge/nightly)](http://stackage.org/nightly/package/hackage-revdeps)

Command-line tool to list Hackage reverse dependencies.

It is different from how Hackage itself tracks them: this tool accounts for all package components, including tests and benchmarks, and counts dependencies only across the latest releases. The approach is roughly equivalent to what [packdeps.haskellers.com](https://packdeps.haskellers.com) used to do.
