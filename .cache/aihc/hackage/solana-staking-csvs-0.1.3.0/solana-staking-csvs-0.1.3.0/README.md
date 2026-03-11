# Solana Staking CSVs

[![solana-staking-csvs Build Status](https://github.com/prikhi/solana-staking-csvs/actions/workflows/main.yml/badge.svg)](https://github.com/prikhi/solana-staking-csvs/actions/workflows/main.yml)


Generate CSVs of your Solana staking rewards.

Requires [`stack`][get-stack] & a SolanaBeach API key, which you can request
[here][solanabeach-api].

```sh
stack run -- <YOUR_API_KEY> <ACCOUNT_PUBKEY>
stack run -- --help
```

TODO:

* Allow sourcing pubkey & apikey from env variables?
* Move SolanaBeach API to separate, published package.

[get-stack]: https://docs.haskellstack.org/en/stable/README/
[solanabeach-api]: https://github.com/solana-beach/api


## Install

You can install the CLI exe by running `stack install`. This lets you call the
executable directly instead of through stack:

```sh
$ stack install
$ export PATH="${HOME}/.local/bin/:${PATH}"
$ solana-staking-csvs <YOUR_API_KEY> 6MTkiDNY5N5PoJHN862D91jM5ztF3KQWDyBeobo2rSgK
time,amount,stakeAccount,epoch
2021-07-03 19:49:49UTC,27.115357569,8yfoauy7WhfBGA441GsHnjQedeAga8MsZXu8Pn16xMmY,197
2021-07-06 21:44:25UTC,27.197834728,8yfoauy7WhfBGA441GsHnjQedeAga8MsZXu8Pn16xMmY,198
2021-07-10 00:02:06UTC,27.231624940,8yfoauy7WhfBGA441GsHnjQedeAga8MsZXu8Pn16xMmY,199
2021-07-10 00:02:06UTC,27.233380734,7XitpDt2tUwwmmmxfbPC4jJ6cCseuBBQHw5p6kWqmqvn,199
```


## Build

You can build the project with stack:

```code
stack build
```

For development, you can enable fast builds with file-watching,
documentation-building, & test-running:

```code
stack test --haddock --fast --file-watch --pedantic
```

To build & open the documentation, run

```code
stack haddock --open solana-staking-csv
```


## LICENSE

BSD-3
