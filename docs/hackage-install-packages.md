# Hackage install list

The packages below are installed from Hackage by the daily
[Hackage install](../.github/workflows/hackage-install.yml) workflow, which runs
`aihc install NAME-VERSION --lint` for each of them and opens an issue when one
fails. `scripts/install-hackage-packages.sh` reads this file, so the table is
the list the workflow installs; edit it to add, remove, or bump a package.

The workflow installs every package nine times, once per configuration: for
the `linux-amd64`, `llvm`, and `wasm32-wasip3` targets, each at `-O0`, `-O2`,
and `-Os`. Every configuration is its own job, with a three-hour timeout. A
package that fails in any of them gets one issue, with a section per failing
configuration, so a failure that only shows for one backend or only under
whole-program compilation is still found.

Order matters. Packages are installed from top to bottom into one store, and a
package may only depend on packages above it: the installs share a workspace, so
a dependency is taken from the pinned source next to it rather than resolved
against Hackage. Versions are exact for the same reason — a floating version
would make the run depend on whatever Hackage prefers that day, and a failure
would no longer point at a change in aihc.

Each unpacked release gets the latest Hackage revision of its cabal file, as
it would under cabal-install. The dependency solver checks the version bounds
of every package, and the bounds a release was uploaded with often exclude the
newer `base` or `bytestring` it is built against here; a revision is how
Hackage relaxes them after the fact. The `nix flake check` list in
`scripts/nix/hackage-packages.nix` pins the revision it uses per package.

## Packages

| Package | Version |
| ------- | ------- |
| deepseq | 1.5.2.0 |
| array | 0.5.8.0 |
| containers | 0.8 |
| data-default | 0.8.0.2 |
| bytestring | 0.12.2.0 |
| binary | 0.8.9.3 |
| transformers | 0.6.3.0 |
| mtl | 2.3.2 |
| split | 0.2.5.1 |
| pretty | 1.1.3.6 |
| base64-bytestring | 1.2.1.0 |
| base16-bytestring | 1.0.2.0 |
| tagged | 0.8.10 |
| dlist | 1.0 |
| data-array-byte | 0.1.0.2 |
| primitive | 0.9.1.0 |
| parser-combinators | 1.3.1 |
| text | 2.1.4 |
| prettyprinter | 1.7.2 |
| th-abstraction | 0.7.2.0 |
| OneTuple | 0.4.3 |
| splitmix | 0.1.3.2 |
| parsec | 3.1.18.0 |
| regex-base | 0.94.0.3 |
| stm | 2.5.3.1 |
| exceptions | 0.10.12 |
| os-string | 2.0.11 |
| filepath | 1.5.5.0 |
| aihc-cpp | 2.0.0.0 |
| hashable | 1.5.1.0 |
| case-insensitive | 1.2.1.0 |
| integer-logarithms | 1.0.5 |
| scientific | 0.3.8.1 |
| megaparsec | 9.8.2 |

## Running it locally

```console
$ nix run .#install-hackage-packages
```

The app takes the same options as the script: `--target TARGET` (default
`llvm`), `-O LEVEL` (default `0`), `--store DIR` for the package store,
`--list FILE` to read a different table, and `--report-dir DIR` to write the
per-package Markdown the workflow puts in its issues. One store holds the
entries of every target and level, so the configurations can share `--store`.
`scripts/merge-hackage-install-reports.sh` joins the report directories of
several configurations into the one-report-per-package form the workflow
opens issues from.
