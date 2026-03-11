# binance-exports

[![binance-exports Build Status](https://github.com/prikhi/binance-exports/actions/workflows/main.yml/badge.svg)](https://github.com/prikhi/binance-exports/actions/workflows/main.yml)


Export Your Binance Trade History to a CSV.

Sometime during February 2022, Binance removed their `Trade History` page,
along with the ability to export your completed trades. The `Order History`
export is still available, but the format is more difficult to parse. This
command is a replacement for the `Trade History` export, generating CSVs with
an almost-identical format. There are two differences: we split the trade
symbol into two separate asset columns & include the trade ID.

Requires [`stack`][get-stack] & a Binance.us API key & secret:

```sh
stack run -- -k <API_KEY> -s <API_SECRET> <SYMBOL1> <SYMBOL2> etc
stack run -- --help
```

TODO:

* Switch between Binance & Binance US APIs
* Include Fiat/Crypto Deposits & Withdrawals


[get-stack]: https://docs.haskellstack.org/en/stable/README/


## Install

You can install the CLI exe by running `stack install`. This lets you call the
executable directly instead of through stack:

```sh
$ stack install
$ export PATH="${HOME}/.local/bin/:${PATH}"
$ binance-exports -k <API_KEY> -s <API_SECRET> SOLUSD
time,base-asset,quote-asset,type,price,quantity,total,fee,fee-currency,trade-id
2022-03-01 21:20:44,SOL,USD,BUY,42.2424,0.42,42.90010000,0.0009001,BNB,9001
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
stack haddock --open binance-exports
```


## LICENSE

BSD-3
