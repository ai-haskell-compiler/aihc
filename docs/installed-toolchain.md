# Installed runtimes and libraries

`aihc compile` consumes prepared runtime archives and installed library
artifacts. It does not compile either dependency from source while building an
application.

Prepare every runtime variant that applications will select:

```console
aihc prepare-runtime --target llvm --gc semispace --store "$AIHC_STORE"
aihc prepare-runtime --target wasm32-wasip3 --gc semispace --store "$AIHC_STORE"
```

Then install the packages needed by the applications. Package dependencies are
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

Application compilation only selects the installed store, target, and runtime
variant:

```console
aihc compile Main.hs \
  --store "$AIHC_STORE" \
  --target llvm \
  --gc semispace \
  --output program
```

There is no special core-library installation mechanism: `aihc-base` and
`aihc-prim` are ordinary packages, with the latter installed through the
former's package dependency. The store contains installed library interfaces,
whole-program bodies, and library archives for each target.
Each target has one installed entry per package name and version.
Runtime archives use keys for the target and garbage collector. An incomplete store is
an error; application compilation never fills in missing artifacts by rebuilding
source dependencies.

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

## Linking on another host

`aihc build --no-link` stops before the link and writes a bundle directory
instead of an executable. The bundle holds a copy of every link
input, so it is complete on its own: the module objects, the C objects and
archives of the installed packages, and the entry and runtime archives. A
`link.json` manifest lists them in link order with paths relative to the
bundle.

```console
aihc build Main.hs \
  --store "$AIHC_STORE" \
  --target apple-arm64 \
  --gc semispace \
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

## Optimization level

`aihc build` and `aihc install` take `-O LEVEL`.
The level is 0, 1, 2 or s, and the default is 0.
`-O0`, `-O1`, `-O2` and `-Os` are also accepted.

The level is the level Clang receives.
Clang gets it for the C sources of a package, for the `CFLAGS` of a configure script, and for the LLVM output of the `llvm` target.
The GRIN passes, the Lir lowering, and the object backends of `apple-arm64` and `linux-amd64` do not read the level.
`-O2` and `-Os` also compile the whole program at once, as `--lto` does.
`-O0` and `-O1` compile each module to its own object.
See "Whole-program compilation" below.
The runtime and entry archives are compiled once for each target at `-O2`, whatever level a program names.

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
It merges them into one program and drops each value declaration that the entry of the executable does not reach.
Type, synonym, and axiom declarations stay.
It then lowers the program through GRIN and Lir to one object, `lto/program/program.o` under the build root.
The link takes this object, the C objects and archives of the packages, and the entry and runtime archives.
A `--no-link` bundle carries the program object in place of the module objects.
A package build gives each of its executables a program object of its own, under `exe/<name>/lto`.

The program object follows the System FC files and the backend options.
A build whose inputs are unchanged reuses it.
The whole-program build is part of the identity of an installed package, like the optimization level.
Its store entries sit next to the entries of a per-module build.
`--keep-grin` and `--keep-native` have no effect on an install with the flag, because nothing below System FC is generated.

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

The Cabal build hook uses the current Git commit hash as the compiler identity.
If Git or a commit is absent, the compiler identity is empty.
Uncommitted compiler changes do not change this identity.
The compiled program contains this identity as a pure constant.
Host compiler and archiver identities use hashes of their resolved paths.
After a tool update at the same path, users must remove obsolete store entries.
