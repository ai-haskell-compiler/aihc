# Release history for stack-all

## 0.7.1 (2025-08-05)
- check for stack
- for lts <= 11 fallback to stack-2.15.7
- MajorVer: improve parsing of nightly (nightly-\*), lts (lts-\*), ghc-\*
- add ghc9.10 alias for lts24

## 0.7 (2025-05-25)
- complete rework of stack*.yaml config file logic
- add --stack option to specify stack program (eg stack-2)
- support "snapshot:" as well as "resolver:"
- add -S,--make-all-lts flag (pbrisbin,#4)
- bump default oldest major version from LTS 18 to 20
- error for lts11 with stack > 3
- fix --update-resolver for nightly snapshot
- change --default-resolver short option from -d to -r
- add ghc9.8 alias for lts23

## 0.6.4 (2024-10-13)
- Fix the missing stack.yaml logic to first check for a .cabal file before
  trying parent directory

## 0.6.3 (2024-06-10)
- `--update-resolver` to update default resolver to latest minor
- command option args are now handled more precisely with an ADT

## 0.6.2 (2024-05-21)
- `--default-resolver` to update stack.yaml resolver
- change `--debug` short option to `-D`
- test for `package.yaml` if no .cabal file

## 0.6.1 (2024-05-20)
- fix `--create-config` and `--make-lts` failing if no command

## 0.6 (2024-05-19)
- stack-all now works outside of projects too like stack does
- error when `--make-lts` combined with a command args
- also error for `--create-config` with versions or command args
- bump default oldest lts from 16 to 18 (ghc-8.10)

## 0.5.2 (2024-04-18)
- fix parsing of ltsXX
- use https for snapshots url

## 0.5.1 (2024-03-19)
- update the stackage snapshots.json url

## 0.5 (2024-02-19)
- support ghc-X.Y/ghcX.Y aliases for lts major versions
- default oldest lts bumped from lts-11 (ghc8.2) to lts-16 (ghc8.8)
- ignore .cabal file directory

## 0.4.2 (2023-10-01)
- support unversioned "lts" argument
- add --refresh-cache option to force refresh of cached snapshots.json

## 0.4.1 (2023-02-20)
- ignore nightly as a base when creating a new lts file

## 0.4.0.1 (2022-03-27)
- fix build with simple-cmd-0.2.4

## 0.4 (2022-01-24)
- 'lts' alias can now be used as a commandline version argument
- stack-nightly.yaml is now only used for building nightly
- if creating stack.yaml use latest lts snapshot resolver from snapshot.json
- support aeson-2.0

## 0.3.1 (2021-09-06)
- newest LTS for a project can now be configured
- use cached-json-file for snapshots.json
- add --keep-going
- exclude lts17 now by default

## 0.3 (2021-06-26)
- use hackage.stackage.org/snapshots.json:
  - to determine the latest LTS major version
  - to resolve major LTS version to latest minor snapshot and also print it
- cache snapshots.json locally in ~/.cache/stack-all/ for a few hours
- new --make-lts command to generate a stack-ltsXX.yaml file from newer one

## 0.2.2 (2021-06-07)
- create a stack.yaml file if only an .cabal file is found

## 0.2.1 (2021-04-30)
- if stack fails, print the ~/.stack snapshot path for reference

## 0.2 (2021-04-03)
- better error messages when reading snapshot option/arg (TristanCacqueray)
- search parent dirs for stack project dir, like stack
- snapshot options/args can now be in compact ltsXY form as well as lts-XY
- support stack commands and options

## 0.1.2 (2021-02-05)
- --create-config comment line mentions older version
- show error for unversioned "stack-lts.yaml"
- add lts-17

## 0.1.1 (2020-12-04)
- fix ordering of stack-lts-*.yaml
- allow --newest to override oldest lts config

## 0.1.0 (2020-11-14)
- initial release with --create-config, --debug, --cmd, --newest
- VersionSpec: --all-lts, --oldest, and lts args
