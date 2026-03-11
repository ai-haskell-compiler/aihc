# gemini-exports

[![gemini-exports Build Status](https://github.com/prikhi/gemini-exports/actions/workflows/main.yml/badge.svg)](https://github.com/prikhi/gemini-exports/actions/workflows/main.yml)


Generate CSV Exports of your Gemini Trades.

Requires [`stack`][get-stack]:

```sh
$ stack run -- -k <API_KEY> -s <API_SECRET>
time,base-asset,quote-asset,type,description,price,quantity,total,fee,fee-currency,trade-id
2022-04-20 04:20:00,GUSD,USD,Buy,,1.0,9001.0,9001.0,0.0,USD,900142424242
$ stack run -- --help
```

[get-stack]: https://docs.haskellstack.org/en/stable/README/


## Install

You can install the CLI exe by running `stack install`. This lets you call the
executable directly instead of through stack:

```sh
stack install
export PATH="${HOME}/.local/bin/:${PATH}"
gemini-exports
```


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
stack haddock --open gemini-exports
```


## LICENSE

BSD-3
