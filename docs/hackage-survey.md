# Hackage survey

This file tells which Hackage packages compile with GHC and with aihc.
`scripts/hackage-survey.sh` adds packages to the two tables below.
On each run, the script takes the most downloaded Hackage packages that are
not in the tables. For each new package, the script compiles the latest
release with `cabal build` and GHC. If GHC compiles it, the script compiles
the same release with `aihc install NAME-VERSION --lint`.

Run the script with `nix run .#hackage-survey`. Give the names of packages
as arguments to survey those packages instead of the download list.
The script does not compile a package that is in a table again. To compile
a package again, remove its row.

## Summary

- Packages that compile with GHC: 6
- Packages that compile with aihc: 6
- Packages that do not compile with GHC: 1

## Packages that compile with GHC

| Package | Version | AIHC | Error |
| ------- | ------- | ---- | ----- |
| attoparsec | 0.14.4 | pass |  |
| hashable | 1.5.1.0 | pass |  |
| scientific | 0.3.10.0 | pass |  |
| split | 0.2.5.1 | pass |  |
| unordered-containers | 0.2.21 | pass |  |
| vector | 0.13.2.0 | pass |  |

## Packages that do not compile with GHC

| Package | Version | Error |
| ------- | ------- | ----- |
| haskell98 | 2.0.0.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: haskell98 => base>=4.7 && <4.8) |
