# cointracking-imports

[![cointracking-imports Build Status](https://github.com/prikhi/cointracking-imports/actions/workflows/main.yml/badge.svg)](https://github.com/prikhi/cointracking-imports/actions/workflows/main.yml)


A Haskell library for generating CSV & XLSX files for importing into
[CoinTracking][cointracking]. Build a `[CTImportData]` & write to a file with
`writeImportDataToFile`.

Requires [`stack`][get-stack].

[cointracking]: https://cointracking.info/
[get-stack]: https://docs.haskellstack.org/en/stable/README/


## Build

You can build the project with stack:

```sh
stack build
```

For development, you can enable fast builds with file-watching,
documentation-building, & test-running:

```sh
stack test --haddock --fast --file-watch --pedantic
```

To build & open the documentation, run:

```sh
stack haddock --open cointracking-imports
```


## LICENSE

BSD-3
