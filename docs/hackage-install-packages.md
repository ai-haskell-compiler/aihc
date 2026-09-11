# Hackage install list

The packages below are installed from Hackage by the daily
[Hackage install](../.github/workflows/hackage-install.yml) workflow, which runs
`aihc install NAME-VERSION --lint` for each of them and opens an issue when one
fails. `scripts/install-hackage-packages.sh` reads this file, so the table is
the list the workflow installs; edit it to add, remove, or bump a package.

Order matters. Packages are installed from top to bottom into one store, and a
package may only depend on packages above it: the installs share a workspace, so
a dependency is taken from the pinned source next to it rather than resolved
against Hackage. Versions are exact for the same reason — a floating version
would make the run depend on whatever Hackage prefers that day, and a failure
would no longer point at a change in aihc.

## Packages

| Package | Version |
| ------- | ------- |
| deepseq | 1.5.2.0 |
| array | 0.5.8.0 |
| containers | 0.8 |
| bytestring | 0.12.2.0 |
| binary | 0.8.9.3 |

## Running it locally

```console
$ nix run .#install-hackage-packages
```

The app takes the same options as the script: `--target TARGET` (default
`llvm`), `--store DIR` for the package store, `--list FILE` to read a different
table, and `--report-dir DIR` to write the per-package Markdown the workflow
puts in its issues.
