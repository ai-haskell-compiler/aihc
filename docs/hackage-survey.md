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

- Packages that compile with GHC: 15
- Packages that compile with aihc: 6
- Packages that do not compile with GHC: 2

## Packages that compile with GHC

| Package | Version | AIHC | Error |
| ------- | ------- | ---- | ----- |
| attoparsec | 0.14.4 | pass |  |
| hashable | 1.5.1.0 | pass |  |
| hspec | 2.11.18 | fail | HUnit-1.6.2.0/src/Test/HUnit/Lang.hs:42:15: error: unsolved constraint Eq (Maybe SrcLoc) |
| lens | 5.3.6 | fail | <unknown location in Control.Monad.Codensity>: error: internal type checker abort: internal type annotation error: unzonked meta-variable in finalized instance  |
| pandoc | 3.13 | fail | Could not resolve dependencies: |
| persistent | 2.18.1.0 | fail | auto-update-0.2.8/Control/AutoUpdate/Thread.hs:25:23: error: not exported: term name ‘labelThread’ |
| sbv | 14.9 | fail | syb-0.7.4/src/Data/Generics/Builders.hs:36:13: error: unbound term name ‘fromConstrB’ |
| scientific | 0.3.10.0 | pass |  |
| split | 0.2.5.1 | pass |  |
| texmath | 0.13.3.3 | fail | syb-0.7.4/src/Data/Generics/Builders.hs:36:13: error: unbound term name ‘fromConstrB’ |
| unordered-containers | 0.2.21 | pass |  |
| vector | 0.13.2.0 | pass |  |
| wai-extra | 3.1.18 | fail | HUnit-1.6.2.0/src/Test/HUnit/Lang.hs:42:15: error: unsolved constraint Eq (Maybe SrcLoc) |
| warp | 3.4.21 | fail | time-manager-0.5.2/System/TimeManager.hs:371:9: error: unbound term name ‘updateTimeout’ |
| yesod-core | 1.7.0.0 | fail | auto-update-0.2.8/Control/AutoUpdate/Thread.hs:25:23: error: not exported: term name ‘labelThread’ |

## Packages that do not compile with GHC

| Package | Version | Error |
| ------- | ------- | ----- |
| git-annex | 10.20261026 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| haskell98 | 2.0.0.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: haskell98 => base>=4.7 && <4.8) |
