# Installed runtimes and libraries

`aihc compile` consumes prepared runtime archives and installed library
artifacts. It does not compile either dependency from source while building an
application.

Prepare every runtime variant that applications will select:

```console
aihc prepare-runtime --target llvm --gc calloc --store "$AIHC_STORE"
aihc prepare-runtime --target llvm --gc semispace --store "$AIHC_STORE"
aihc prepare-runtime --target wasm32-wasip3 --gc calloc --store "$AIHC_STORE"
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
  --gc calloc \
  --output program
```

There is no special core-library installation mechanism: `aihc-base` and
`aihc-prim` are ordinary packages, with the latter installed through the
former's package dependency. The store contains content-addressed library
interfaces and whole-program bodies, target-specific library archives, and
runtime archives keyed by target and garbage collector. An incomplete store is
an error; application compilation never fills in missing artifacts by rebuilding
source dependencies.
