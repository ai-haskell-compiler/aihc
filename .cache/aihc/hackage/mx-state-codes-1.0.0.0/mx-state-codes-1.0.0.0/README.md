# ISO 3166-2:MX State Codes

[![Build Status](https://travis-ci.org/prikhi/mx-state-codes.svg?branch=master)](https://travis-ci.org/prikhi/mx-state-codes)


This is a Haskell package that provides a data type for Mexican ISO 3166-2
State codes, as well as functions for converting codes into their English
subdivision names.


## Building

Use `stack` to build this for local development:

```sh
stack build --pedantic --test --haddock --file-watch
```


## Prior Art

The API for this package is based off of the
[ca-province-codes](https://hackage.haskell.org/package/mx-state-codes) package
which is based off of the
[state-codes](https://hackage.haskell.org/package/state-codes) and
[iso3166-country-codes](https://hackage.haskell.org/package/iso3166-country-codes)
packages.


## License

BSD-3, exceptions available.
