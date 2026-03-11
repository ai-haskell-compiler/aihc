# bnb-staking-csvs

[![bnb-staking-csvs Build Status](https://github.com/prikhi/bnb-staking-csvs/actions/workflows/main.yml/badge.svg)](https://github.com/prikhi/bnb-staking-csvs/actions/workflows/main.yml)


Generate CSV exports of your staking rewards on BinanceChain.

Requires [`stack`][get-stack]:

```sh
stack run -- <DELEGATOR_PUBKEY>
stack run -- --help
```

[get-stack]: https://docs.haskellstack.org/en/stable/README/


## Install

You can install the CLI exe by running `stack install`. This lets you call the
executable directly instead of through stack:

```sh
$ stack install
$ export PATH="${HOME}/.local/bin/:${PATH}"
$ bnb-staking-csvs bnb1hwzqet2vusraqvxfxgr6zhupd8kpgz5hpl2wf2 | head -n 4
time,amount,currency,delegator,validator,validatorAddress,height
2021-04-09T20:00:00-04:00,0.00118792,BNB,bnb1hwzqet2vusraqvxfxgr6zhupd8kpgz5hpl2wf2,BscScan,bva1t42gtf6hawqgpmdpjzmvlzvmlttlqtkvlmgjxt,155843908
2021-04-10T20:00:00-04:00,0.00095682,BNB,bnb1hwzqet2vusraqvxfxgr6zhupd8kpgz5hpl2wf2,BscScan,bva1t42gtf6hawqgpmdpjzmvlzvmlttlqtkvlmgjxt,156060618
2021-04-11T20:00:00-04:00,1.74196290,BNB,bnb1hwzqet2vusraqvxfxgr6zhupd8kpgz5hpl2wf2,TW Staking,bva1c6aqe9ndzcn2nsan963z43xg6kgrvzynl97785,156277904
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

To build & open the documentation, run

```sh
stack haddock --open bnb-staking-csvs
```


## LICENSE

BSD-3
