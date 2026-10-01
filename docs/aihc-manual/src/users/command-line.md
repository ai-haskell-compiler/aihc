# Command line

The `aihc` executable has four commands.

| Command | Purpose |
| --- | --- |
| `aihc build INPUT` | Build one executable from a main module, or every executable of a Cabal package. |
| `aihc install PACKAGE` | Build and install one Cabal library into the store. |
| `aihc link-exe BUNDLE` | Link one executable from a bundle that `build --no-link` wrote. |
| `aihc plan INPUT` | Solve the dependency plan of a Cabal package and print it. |

Use `aihc COMMAND --help` to get the full option list of each command.

## Inputs

`INPUT` and `PACKAGE` accept these forms:

| Form | Example | Meaning |
| --- | --- | --- |
| Main module | `Main.hs` | One Haskell file with a `main` function. Only `aihc build` accepts this form. |
| Local package | `./my-package` | A directory with a `.cabal` file. |
| Hackage package | `text` or `text-2.1.4` | A package from Hackage, with an optional version. |

## Build

```bash
aihc build INPUT [OPTIONS]
```

| Option | Meaning |
| --- | --- |
| `--output PATH` | Write the executable to `PATH`. For a package, write the executables under the directory `PATH`. |
| `--source-dir DIR` | Add a source directory for a main module. The default directory is `.`. |
| `--package CONSTRAINT`, `-p` | Add an installed package constraint for a main module. |
| `--executable NAME` | Build only the named executable of a package. |
| `--target TARGET` | Select the target. See [Select a target](getting-started.md#select-a-target). |
| `-O LEVEL` | Select the optimization level. See [Optimization levels](#optimization-levels). |
| `--lto` | Compile each module to System FC only, and compile the merged program once. |
| `--no-link` | Compile only. Write the objects and a `link.json` manifest to a bundle directory. |
| `--verbose` | Print each build step. |

## Install

```bash
aihc install PACKAGE [OPTIONS]
```

| Option | Meaning |
| --- | --- |
| `--immutable` | Install a local package into the store as if it were a Hackage release. |
| `--reinstall` | Build the package again when it exists. |
| `--no-code` | Check the package but do not generate code. |
| `--verbose` | Print each installation step. |
| `--print-timings` | Print the time of each compiler stage. |

## Dependency plans

`aihc build`, `aihc install`, and `aihc plan` solve the dependency plan of a package.
The commands write the plan to `aihc.lock` in the package directory.

| Option | Meaning |
| --- | --- |
| `--constraint CONSTRAINT` | Restrict the plan. `NAME RANGE` fixes the versions of a package. `NAME +flag` and `NAME -flag` fix one of its Cabal flags. |
| `--workspace DIR` | Take the sources of a dependency from `DIR/NAME` before Hackage. |
| `--locked` | Take the plan from `aihc.lock`. Fail if the lock is absent or stale. |
| `--update` | Ignore `aihc.lock`, solve the plan again, and rewrite the lock. |
| `--update-package NAME` | Ignore what the lock says about `NAME` and the packages that depend on it. |

## Optimization levels

| Level | Meaning |
| --- | --- |
| `-O0` | Run no System FC pass. |
| `-Os` | Run the shrinking inliner. Compile the whole program at once. |
| `-O1` | Run the shrinking inliner and the growing inliner. |
| `-O2` | Run all passes. Compile the whole program at once. |

Clang receives the same level for C sources and for LLVM output.

## Store and build root

AIHC keeps installed libraries in a store.
It keeps the module artifacts of a build under `.aihc-target`.

| Option | Meaning |
| --- | --- |
| `--store DIR` | Override the store root. |
| `--build-root DIR` | Write the module artifacts under `DIR` instead of `.aihc-target`. |

## Debug options

These options keep intermediate files or add run-time checks.

| Option | Meaning |
| --- | --- |
| `--keep-core` | Keep the System FC files. They are binary. Use `aihc-dev fc-print` to show one. |
| `--keep-grin` | Keep the GRIN files. |
| `--keep-lir` | Keep the Lir files. |
| `--keep-native` | Keep the native output files. |
| `--lint` | Run the lint checks of each intermediate language. |
| `--check-prim-bounds` | Check the index of each array primitive and abort on an out-of-bounds access. |
