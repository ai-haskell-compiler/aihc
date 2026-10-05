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
| containers | 0.7 |
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
| StateVar | 1.2.2 |
| contravariant | 1.5.6 |
| aihc-cabal-syntax | 2.0.0.0 |
| aihc-parser | 5.0.0.0 |
| appar | 0.1.8 |
| assoc | 1.1.1 |
| atomic-counter | 0.1.2.4 |
| base-orphans | 0.9.4 |
| basement | 0.0.16 |
| blaze-builder | 0.4.4.1 |
| boring | 0.2.2 |
| byteorder | 1.0.4 |
| cereal | 0.5.8.3 |
| character-ps | 0.1 |
| colour | 2.3.7 |
| ansi-terminal-types | 1.1.3 |
| ansi-terminal | 1.1.5 |
| constraints | 0.14.4 |
| cryptohash-sha256 | 0.11.102.1 |
| data-default-class | 0.2.0.0 |
| data-fix | 0.3.4 |
| distributive | 0.6.3 |
| barbies | 2.1.1.0 |
| erf | 2.0.0.0 |
| ghc-bignum | 1.3 |
| half | 0.3.3 |
| cborg | 0.2.10.0 |
| haskell-lexer | 1.2.1 |
| hourglass | 0.2.12 |
| http-types | 0.12.4 |
| indexed-traversable | 0.1.4 |
| comonad | 5.0.10 |
| bifunctors | 5.6.3 |
| integer-conversion | 0.1.1 |
| integer-gmp | 1.1 |
| memory | 0.18.0 |
| asn1-types | 0.3.4 |
| asn1-encoding | 0.9.6 |
| asn1-parse | 0.9.5 |
| crypton | 1.0.6 |
| mime-types | 0.1.2.2 |
| old-locale | 1.0.0.7 |
| old-time | 1.1.1.0 |
| pem | 0.2.4 |
| crypton-x509 | 1.7.7 |
| pretty-show | 1.10 |
| prettyprinter-ansi-terminal | 1.1.3 |
| random | 1.2.1.3 |
| QuickCheck | 2.15.0.1 |
| safe-exceptions | 0.1.7.4 |
| terminal-size | 0.3.4 |
| text-short | 0.1.6.1 |
| th-compat | 0.1.7 |
| network-uri | 2.6.4.2 |
| these | 1.2.1 |
| strict | 0.5.1 |
| time | 1.14 |
| cookie | 0.5.1 |
| time-compat | 1.9.9 |
| text-iso8601 | 0.1.1.1 |
| transformers-compat | 0.7.2 |
| mmorph | 1.2.2 |
| transformers-base | 0.4.6.1 |
| monad-control | 1.0.3.1 |
| lifted-base | 0.2.3.12 |
| unix | 2.8.8.0 |
| directory-ospath-streaming | 0.2.2 |
| file-io | 0.1.6 |
| directory | 1.3.10.1 |
| crypton-x509-store | 1.6.14 |
| network | 3.2.8.0 |
| crypton-socks | 0.6.2 |
| iproute | 1.7.15 |
| crypton-x509-validation | 1.6.14 |
| process | 1.6.26.1 |
| crypton-x509-system | 1.6.8 |
| optparse-applicative | 0.18.1.0 |
| tar | 0.6.4.0 |
| unbounded-delays | 0.1.1.1 |
| tasty | 1.5.4 |
| unix-time | 0.4.17 |
| unliftio-core | 0.2.1.0 |
| resourcet | 1.3.0 |
| unordered-containers | 0.2.20.1 |
| async | 2.2.6 |
| concurrent-output | 1.10.21 |
| lifted-async | 0.10.2.7 |
| semigroupoids | 6.0.2 |
| uuid-types | 1.0.6.1 |
| vector-stream | 0.1.0.1 |
| vector | 0.13.2.0 |
| indexed-traversable-instances | 0.1.2.1 |
| semialign | 1.3.1.1 |
| serialise | 0.2.6.1 |
| witherable | 0.5 |
| aeson | 2.2.4.1 |
| wl-pprint-annotated | 0.1.0.1 |
| hedgehog | 1.5 |
| tasty-hedgehog | 1.4.0.2 |
| zlib-clib | 1.3.1 |
| zlib | 0.7.1.1 |
| streaming-commons | 0.2.3.1 |
| http-client | 0.7.19 |
| tls | 2.1.8 |
| crypton-connection | 0.4.5 |
| http-client-tls | 0.3.6.4 |

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
