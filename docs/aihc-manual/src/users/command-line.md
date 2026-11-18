# Command line

The `aihc` executable has six commands.

| Command | Purpose |
| --- | --- |
| `aihc build INPUT` | Build one executable from a main module, or every executable of a Cabal package. |
| `aihc install PACKAGE` | Build and install one Cabal library into the store. |
| `aihc link-exe BUNDLE` | Link one executable from a bundle that `build --no-link` wrote. |
| `aihc plan INPUT` | Solve the dependency plan of a Cabal package and print it. |
| `aihc repl` | Read Haskell inputs and evaluate each of them in one session. |
| `aihc test INPUT` | Build the test suites of a Cabal package and run each of them. |

Use `aihc COMMAND --help` to get the full option list of each command.

## Inputs

`INPUT` and `PACKAGE` accept these forms:

| Form | Example | Meaning |
| --- | --- | --- |
| Main module | `Main.hs` | One Haskell file with a `main` function. Only `aihc build` accepts this form. |
| Local package | `./my-package` | A directory with a `.cabal` file. |
| Hackage package | `text` or `text-2.1.4` | A package from Hackage, with an optional version. |
| Project target | `exe:aihc`, `aihc:test:spec`, or `aihc` | A package of `cabal.project`, or one of its executables or test suites. |
| All packages | `all` | Every package of `cabal.project`. Only `aihc test` accepts this form. |

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
| `--no-link` | Compile only. Write the objects and a `link.args` response file to a bundle directory. |
| `--verbose` | Print each build step. |
| `--print-timings` | Print the time of each compiler stage. |

## Test

```bash
aihc test INPUT [OPTIONS]
```

The command builds each `exitcode-stdio-1.0` test suite of the package as an executable.
Then, it runs each test suite in the directory of its package.
The command writes `Test suite NAME: PASS` or `Test suite NAME: FAIL` for each test suite, and then the count of the test suites that passed.
If a test suite fails, the command fails.
With `all`, the command tests each package of the project that has a test suite.

| Option | Meaning |
| --- | --- |
| `--test-suite NAME` | Build and run only the named test suite of a package. |
| `--test-option ARG` | Give `ARG` to each test suite. Give the option again to add an argument. |
| `--target TARGET` | Select the target. See [Select a target](getting-started.md#select-a-target). |
| `-O LEVEL` | Select the optimization level. See [Optimization levels](#optimization-levels). |
| `--lto` | Compile each module to System FC only, and compile the merged program once. |
| `--verbose` | Print each build step. |

## Repl

```bash
aihc repl [OPTIONS]
```

The command reads one input from each line and evaluates it.
The command compiles each input to WebAssembly and runs it in one instance.
The instance stays alive, thus a later input can use the names of the earlier inputs.
The command needs the `wasm32-wasip3` toolchain and Node.js.

| Input | Example | Result |
| --- | --- | --- |
| Declarations | `f x = x + 1` or `let y = 2` | The names are available to the subsequent inputs. |
| Bind statement | `text <- readFile "notes.txt"` | The command runs the action one time. The names get the result. |
| Expression | `f 2` | The command shows the value. If the expression is an action, the command runs it and shows its result. The name `it` gets the value or the result. |
| Import | `import Data.List (sort)` | The subsequent inputs can use the names of the module. |
| `:{` | | Start an input of more than one line. The line `:}` is its end. |
| `:set -XNAME` | `:set -XLambdaCase` | Set a language extension for the subsequent inputs. |
| `:help` | | Show the commands. |
| `:quit` | | Stop the session. |

A new definition of a name hides the earlier definition.
An exception in an input is a message, and the session continues.
The code can write to the standard output and to the standard error.
The code can read the files of the current directory.
The code cannot read the standard input, write files, or use the network.

| Option | Meaning |
| --- | --- |
| `--package CONSTRAINT`, `-p` | Add a package that an input can import. The package `base` is always available. |
| `--build-root DIR` | Keep the files of the session under `DIR`. The default is a temporary directory. |

The command has these limits:

- A pattern match that fails stops the session, because the compiled code traps.
- The location in a compiler message refers to the module that the command generates for the input.
- An expression that has no `Show` instance and is not an action compiles four times before the command shows the message.
- An import with an import list and a later definition of the same name give an ambiguous name.

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

## Progress output

`aihc build`, `aihc install`, and `aihc test` write their progress to the standard error stream.
The result goes to the standard output stream: the store entry of an install, the path of each executable, or the result of each test suite.

When the standard error stream is a terminal, the commands show a live view.
The view starts with the plan: the number of packages and executables, and their names.
Below the plan, a small frame redraws in place.
Its first line has a bar of all the modules of the plan, and counts the packages that are complete, the busy threads, and the time.
The next lines name the packages that received work last, several to a line, each with its compiled module count.
The last line counts the packages that the store holds, the running tasks of each kind, the other packages that compile, and the packages that wait.
A package that is complete stays on the screen above the frame, with its module count and its time.

When the standard error stream is not a terminal, the commands write one line for each step.
The lines name the plan, each package that the store holds, each package that builds, each executable that links, and each item that is complete.
Set `NO_COLOR` to remove the colors from the live view.
Set `TERM=dumb` to get the plain lines on a terminal.

## Dependency plans

`aihc build`, `aihc install`, `aihc plan`, and `aihc test` solve the dependency plan of a package.
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
| `-O2` | Run the standard optimization passes. Compile the whole program at once. |

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
