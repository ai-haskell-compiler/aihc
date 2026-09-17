# Installed runtimes and libraries

`aihc compile` consumes installed library artifacts. It does not compile a
dependency from source while building an application.

The runtime system is a package like any other: `aihc-rts` under `core-libs`,
the standin for the `rts` boot library of GHC. It holds the C sources and
the Lir units of the runtime and no Haskell module. `aihc-prim` depends on
it, so the first install into a store builds the runtime for the target
before anything else, and every program links it through the ordinary
package dependencies.

Install the packages needed by the applications. Package dependencies are
installed recursively, and Cabal metadata selects the library modules. The
frontend is compiled once even when several targets are requested:

```console
aihc install core-libs/aihc-base \
  --offline \
  --store "$AIHC_STORE" \
  --target llvm \
  --target wasm32-wasip3
```

The package argument is a local Cabal package directory. When no such directory
exists, it is read as a Hackage package name with an optional version and the
sources are fetched from Hackage; without a version the preferred version on
Hackage is used:

```console
aihc install nats --store "$AIHC_STORE" --target llvm
aihc install nats-1.1.1 --store "$AIHC_STORE" --target llvm
```

Application compilation only selects the installed store and target:

```console
aihc compile Main.hs \
  --store "$AIHC_STORE" \
  --target llvm \
  --output program
```

There is no special core-library installation mechanism: `aihc-base`,
`aihc-prim` and `aihc-rts` are ordinary packages, each installed through the
package dependency of the one above it. The store contains installed library
interfaces, whole-program bodies, and library archives for each target.
Each target has one installed entry per package name and version.
A change to a runtime source is a change to the `aihc-rts` package, so the
store fingerprints and invalidates the runtime the way it does every other
package. An incomplete store is an error; application compilation never
fills in missing artifacts by rebuilding source dependencies.

## The runtime package

The C sources of `aihc-rts` are its `c-sources`, compiled with the C
compiler of the target and the `cc-options` of the package, which hold
`-O2` so the runtime is optimized whatever level a program names. The Lir
units are named by the aihc-specific field `x-aihc-lir-sources` and
compiled with the Lir backend of the target; see the "Runtime units"
section of `docs/lir.md`. Both kinds of object land in the `cbits`
directory of the store entry, and a link takes every object there as it
is, so no unit of the runtime is left to an archive member search.

The `wasm32-wasip3` host layer takes the WASI 0.3 C bindings under
`wasm/generated`, which `wit-bindgen` writes from the component world in
`bin/aihc/compiler/wasm/runtime/wit`. The bindings are committed, so a
compiler needs no `wit-bindgen` to build a wasm program; the link embeds the
component type of the same world into the core module with `wasm-tools
component embed`. `scripts/update-wit-bindings.sh --update` rewrites the
bindings after a change to the world or to the pinned `wit-bindgen`, and
the `wit-bindings` Nix check fails when they drift.

The entry unit of an executable, which starts the runtime and enters the
program, is the same for every executable. `aihc build` generates it as Lir
and compiles it to `entry.o` beside the module objects of the executable.

## Preprocessed sources

A module that a package ships as a `.hsc` file goes through `hsc2hs` before
aihc reads it, as it does under Cabal. The suffix selects the tool; the cabal
file does not have to name it. The tool is found on the `PATH`, or named with
`AIHC_HSC2HS`; the standalone Hackage `hsc2hs` is preferred over the wrapper
that ships with GHC, which adds GHC's own C flags and include directory.

hsc2hs always runs in its cross-compilation mode, with the C compiler and
flags of the target, so the constants it reads are the target's and no
program is run. The generated module lands under the package's output path
for that target, in `preprocess/`, next to the `configure/` directory of a
`build-type: Configure` package, whose headers it can include.

## Compiler headers

C code of a package expects the headers that a GHC installation gives:
`HsFFI.h`, `MachDeps.h`, and the two that GHC writes for its own host,
`ghcplatform.h` and `ghcautoconf.h`. aihc holds the text of each header once,
in `Aihc.Hackage.Headers`, and answers both readers from it. The CPP pass over
the Haskell sources takes the text directly. A C compile needs a file, so aihc
writes the headers into `include/` under the build root of the target and
gives that directory to the `c-sources` of the package, to the C wrappers of
its `capi` imports and to `hsc2hs`.

Every answer comes from the target, and none from the host that runs aihc.
The pointer size and the Haskell word are separate, because they differ: a
`wasm32` pointer is four bytes, while an `Int#` and a heap slot are eight
bytes on every target. `ghcplatform.h` gives the pointer, the byte order and
the `*_HOST_OS` and `*_HOST_ARCH` macros. `MachDeps.h` gives the Haskell
sizes. `ghcautoconf.h` has no feature macros, because aihc runs no configure
script.

## Linking on another host

`aihc build --no-link` stops before the link and writes a bundle directory
instead of an executable. The bundle holds a copy of every link
input, so it is complete on its own: the module objects and the entry
object of the executable, and the C, Lir and module objects and the archives
of the installed packages, the runtime among them. A `link.json` manifest
lists them in link order with paths relative to the bundle.

```console
aihc build Main.hs \
  --store "$AIHC_STORE" \
  --target apple-arm64 \
  --no-link \
  --output program-bundle
```

On Linux, compiling for `apple-arm64` needs the macOS SDK headers for the C
runtime and for package C sources. `AIHC_APPLE_SDK` names the SDK root, which
the compiler passes as `-isysroot`, and `AIHC_APPLE_CLANG` selects the Clang
executable, which under Nix should be the unwrapped one because the wrapper
adds Linux-only arguments. The Nix package `cross-examples-apple-arm64` sets
both, fetching the SDK the way nixpkgs does for its Darwin toolchain.

`aihc link-exe` finishes the bundle on a host that has the linker for the
target, such as a Mac for `apple-arm64` objects compiled on Linux:

```console
aihc link-exe program-bundle --output program
```

The manifest is plain JSON, so a host without the compiler can also run the
link through the C driver of the target directly. The weekly cross-compilation
workflow does this: `nix build .#cross-examples-apple-arm64` compiles every
example to a bundle on Linux, and `scripts/link-and-run-example-bundles.sh`
links and runs the bundles on macOS.

## Configure packages

A package with `build-type: Configure`, such as `time` or `unix`, has a
configure script that writes the headers its sources include, such as
`HsTimeConfig.h`. `aihc install` runs the script before it preprocesses
anything. The script runs out of tree, from `<package>/configure` under the
package's own output path, because the unpacked source tree in the cache is
shared by every target while the answers configure finds are per target.
Autoconf writes the outputs of `AC_CONFIG_HEADERS` and `AC_CONFIG_FILES`
relative to the working directory, so the source tree stays untouched. Every
`include-dirs` entry of the package gets a counterpart under that directory
that is searched first, for the CPP pass over the Haskell sources and the C
sources alike, and a `<package>.buildinfo` the script writes is merged the
way Cabal merges it.

The script sees the C compiler of the target: `CC` and `CFLAGS` are the
driver and arguments the C sources are later compiled with, including the
`--target`, the macOS SDK from `AIHC_APPLE_SDK`, and the WASI sysroot. A
target other than the host is named with `--host`, which tells the script it
cannot run the programs it compiles. The script itself, the compiler, and
those arguments are hashed into `configure.hash`, so a local package only
reconfigures when one of them changes.

## Kept intermediate output

`aihc build` and `aihc install` take `--keep-core`, `--keep-grin` and `--keep-native`, and `aihc build` also takes `--keep-lir`.
Each keeps the output of one compiler phase beside the object of the module it belongs to, as a debugging aid; nothing reads any of them back.

| Flag | Files |
| --- | --- |
| `--keep-core` | `core`, the System FC of the module |
| `--keep-grin` | `grin`, `cps.grin` and `gc.grin` |
| `--keep-lir` | `<Module>.o.lir`, the Lir of the module |
| `--keep-native` | the source the C driver of the target compiles: `<Module>.o.ll` for `llvm` and `<Module>.o.s` for `wasm32-wasip3` |

`apple-arm64` and `linux-amd64` have no such source, because their backends write the object themselves.
`--keep-native` keeps the Lir text there, which is the same file `--keep-lir` keeps.

A `--lto` build compiles the merged program rather than the modules, so `--keep-grin`, `--keep-lir` and `--keep-native` keep the output of that program under `lto/program`, and `--keep-core` keeps the merged System FC there as well as the System FC of each module.

`aihc build` keeps the output of the modules of the executable alone.
Its installed packages are built the way `aihc install` builds them, so a dependency already in the store is never rejected for lacking those outputs.
Use `aihc install --keep-core` on the package itself to keep the output of a library.

## Optimization level

`aihc build` and `aihc install` take `-O LEVEL`.
The level is 0, 1, 2 or s, and the default is 0.
`-O0`, `-O1`, `-O2` and `-Os` are also accepted.

The level names what the build is for, and `Aihc.Cli.OptimizationPlan` expands it once into a plan: the scope of the build and the System FC passes to run.
No pass reads the level.
See `docs/optimization.md` for the design.
The level is also the level Clang receives.
Clang gets it for the C sources of a package, for the `CFLAGS` of a configure script, and for the LLVM output of the `llvm` target.
The GRIN passes, the Lir lowering, and the object backends of `apple-arm64` and `linux-amd64` do not read the level.
`-O2` and `-Os` also compile the whole program at once, as `--lto` does.
`-O0` and `-O1` compile each module to its own object.
See "Whole-program compilation" below.

### The System FC passes

`-O0` runs no pass.
`-Os` eta expands the program, runs the inliner under the shrinking policy, eta expands again, and simplifies once.
`-O1` and `-O2` run the inliner under the growing policy after the shrinking one, so `-Os` is a prefix of `-O2`.
`-O1` runs the passes on each module alone, so the inliner sees only the values of that module.
`-O2` and `-Os` run them on the merged program, after the values that the entry does not reach are dropped.
`--verbose` prints one line per pass with the size of the program before and after it.

The inliner walks the values from the leaves of the call graph to its roots.
At a call that gives every parameter of a non-recursive value, it puts a copy of the body in place and reduces the copy.
A copy of a constructor application is never made: a case on a known constructor selects the field instead.
Every decision is local to the callee, the site and the value the site sits in; there is no budget for the program as a whole.
The shrinking policy keeps a copy only when the program does not get larger.
The growing policy keeps a copy that makes the program larger while the callee is under its size limit, the site is under its growth limit, and the value the site sits in has not grown past its own multiple.
A value that nothing uses after the walk is dropped, unless it is a root.
A public value of a module is a root at `-O1`, and the entry of the program is the root at `-O2` and `-Os`.

Before the walk, each method body of a dictionary becomes a top-level helper.
A dictionary is then a small constructor application, and a method of a known dictionary becomes a direct call of the helper.
The size the inliner measures follows the code that the CPS conversion of GRIN makes, which copies the continuation of a case in bind position into each alternative.
The runtime is compiled at `-O2` whatever level a program names, because the `cc-options` of `aihc-rts` say so; the entry unit is Lir and takes no level.

The level is part of the identity of an installed package.
`aihc install -O2` writes a store entry next to the entry of the default level, and its manifest records the flag `O2`; `-O1` and `-Os` record `O1` and `Os`.
`aihc build -O2` builds its modules and its packages at level 2, so the first optimized build of a store also builds `aihc-base` at level 2.

## Whole-program compilation

`aihc build --lto` and `aihc install --lto` stop each module at System FC.
`-O2` and `-Os` imply the flag.
The flag selects the same build at `-O0` and `-O1`, which is the default.
An install with the flag writes the System FC of each module to its `core` file.
It writes no GRIN, no Lir, and no object below it.
The library archive then holds only the C objects of the package and the C wrappers of its `capi` imports.
The manifest records the flag `lto`.
It also lists every module the package compiled, exposed or hidden, under `compiledModules`.

`aihc build --lto` installs its packages with the flag and compiles its own modules to System FC in the same way.
It then reads the System FC of every module of the program, from the packages and the executable alike.
It merges them into one program and drops each declaration that the entry of the executable does not reach.
A type keeps its header where a type the program keeps mentions it, because the kind in that header decides the runtime representation, and keeps only the constructors that an expression builds or a case alternative matches.
A type family keeps every equation of the family, because an equation is found by the head of its left side and never by name.
The program is pruned again after it is inlined, so that a constructor whose last use inlining removed emits no info table.
It then lowers the program through GRIN and Lir to one object, `lto/program/program.o` under the build root.
`--keep-core` writes the merged program, as it stands after inlining and the second prune, to `lto/program/core`, beside the `core` file of each module it was merged from.
The link takes this object, the entry object, and the C, Lir and module objects and archives of the packages.
A `--no-link` bundle carries the program object in place of the module objects.
A package build gives each of its executables a program object of its own, under `exe/<name>/lto`.

The program object follows the System FC files and the backend options.
A build whose inputs are unchanged reuses it.
The whole-program build is part of the identity of an installed package, like the optimization level.
Its store entries sit next to the entries of a per-module build.
`--keep-grin` and `--keep-native` have no effect on an install with the flag, because nothing below System FC is generated.

## Primitive bounds checks

The array primitives are unchecked, as in GHC: `indexArray#`, `readWordArray#`, `writeWord8Array#`, and their relatives compile to a load or store at the indexed address.
`--check-prim-bounds` makes every one of them compare the index against the length first and abort with a runtime message on an out-of-bounds access, as GHC does under `-fcheck-prim-bounds`.
The checks are part of the generated code, so the flag is part of the identity of an installed package, like the optimization level, and the manifest records it.

## Artifact reuse

A package is either immutable or local, and the two never mix.

A Hackage release and a core library are immutable for a given compiler.
`aihc install` puts them into the store under `<store>/<target>/<name>-<version>-<fingerprint>`.
The fingerprint is a function of the plan alone: the package name and version, the compiler identity, the target, and the identities of the dependencies.
It reads no sources, so a consumer computes it without them, and nothing ever lists the store.
An existing directory is used as it is.
`--reinstall` builds the named package again and replaces its directory.
The manifest records the flags the entry was built with, and an install that asks for an output the entry lacks, such as `--keep-core`, must pass `--reinstall`.

A package installed from a local directory is mutable.
It builds in place under `<directory>/.aihc-target/<target>/<name>-<version>`, or under `--build-root`.
`--immutable` installs a local package into the store instead, as a core library is published.

In a build directory every unit keeps a stamp beside its artifacts: the digests of the inputs it was built from, the digests of the interfaces it wrote, and the size and modification time of each artifact file.
The inputs of a unit are its sources, the scope and type interfaces of the modules it imports, the instance facts of the units below it, and the instance digest of each dependency package it imports from.
A later build parses the sources, walks the units in dependency order, and reuses a unit whose recorded inputs equal the current digests and whose artifacts are the recorded files.
Everything else is rebuilt in place.
No interface is encoded to learn its digest: source digests come from parsing, and artifact digests are taken from the bytes as they are written.
Each package writes `digests.json` next to its manifest, from which consumers take the digests of its interfaces.

`aihc build` takes a main module, a local Cabal package directory, or a Hackage package name with an optional version, as `aihc install` takes it.
An existing file is a main module; anything else is a package.

For a main module, `--package` constraints resolve through the same plan.
The plan reads the Cabal files of the packages, from the Hackage download cache or from `--workspace DIR`, which holds a package source under `DIR/NAME`.
A package that is absent from the store is built.
The modules of the executable build under `.aihc-target` in the working directory, or under `--build-root`, with the same stamps as a local package.

For a package, every executable whose `buildable` flag is set is built, each from the `main-is`, `other-modules`, `default-extensions`, and `build-depends` of its own stanza.
The `build-depends` resolve through the same plan, with the package itself and its siblings found before `--workspace` and Hackage, so an executable that depends on the library of its package builds that library first.
A local package and its executables build under `<directory>/.aihc-target/<target>`; the executables of a Hackage release build under `.aihc-target` in the working directory, since its source tree is the shared download cache.
The executables are written to `bin` under that directory, or under `--output DIR`, and `--no-link` writes a link bundle directory named after each executable there instead.

The Cabal build hook uses the Git tree hash of the working tree as the compiler identity: the committed sources together with every uncommitted change and every untracked file that is not ignored.
Two checkouts with the same sources share an identity, and a checkout with an uncommitted change has its own, so two compilers built from one commit never share a store entry.
If Git or a commit is absent, the compiler identity is empty.
The compiled program contains this identity as a pure constant.
Host compiler and archiver identities use hashes of their resolved paths.
After a tool update at the same path, users must remove obsolete store entries.
