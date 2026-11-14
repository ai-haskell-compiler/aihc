[![User guide](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/publish-docs-site.yml?branch=main&event=workflow_run&label=user%20guide)](https://docs.aihc.app/)
[![Generated Reports](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/generated-reports-update.yml?label=reports)](https://github.com/ai-haskell-compiler/aihc/actions/workflows/generated-reports-update.yml)
[![Self-Compile](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/self-compile.yml?branch=main&label=Self-Compile)](https://github.com/ai-haskell-compiler/aihc/actions/workflows/self-compile.yml)
[![Discord](https://img.shields.io/discord/1555935190487142460?label=discord&logo=discord&logoColor=white&color=5865F2)](https://discord.gg/uGWkhMCZrZ)


# AI-written Haskell Compiler (aihc)

Can chatgpt, Claude Opus and Qwen-Coder write a Haskell compiler? Probably not but let's find out. We'll need:

| Stage | Progress | Notes |
| --- | --- | --- |
| Preprocessing | ●●●●● done | see [aihc-cpp](https://github.com/ai-haskell-compiler/aihc-cpp) |
| Parsing | ●●●●● done | see [aihc-parser](https://github.com/ai-haskell-compiler/aihc-parser) |
| Name resolution | <!-- AUTO-GENERATED: START resolve-progress --> ●●●●● `124/124` (`100.00%`) <!-- AUTO-GENERATED: END resolve-progress --> | fixture pass rate |
| Type checking | <!-- AUTO-GENERATED: START tc-progress --> ●●●●○ `413/418` (`98.80%`) <!-- AUTO-GENERATED: END tc-progress --> | fixture pass rate |
| Desugaring | <!-- AUTO-GENERATED: START desugar-progress --> ●●●●○ `1117/1153` (`96.87%`) <!-- AUTO-GENERATED: END desugar-progress --> | fixture pass rate |
| Code generation | <!-- AUTO-GENERATED: START codegen-progress --> ●●●●● `274/274` (`100.00%`) <!-- AUTO-GENERATED: END codegen-progress --> | machine code, LLVM IR, WASM |
| `ghc-prim` shim | <!-- AUTO-GENERATED: START ghc-prim-progress --> ○○○○○ `828/5013` (`16.52%`) <!-- AUTO-GENERATED: END ghc-prim-progress --> | exports implemented |
| `base` implementation | <!-- AUTO-GENERATED: START base-progress --> ●○○○○ `2293/10061` (`22.79%`) <!-- AUTO-GENERATED: END base-progress --> | exports implemented |
| Self-host | <!-- AUTO-GENERATED: START self-hosting-progress --> ●●●●○ `131/132` (`99.24%`) <!-- AUTO-GENERATED: END self-hosting-progress --> | packages that install, see [below](#self-hosting) |

## Getting started

Build the compiler and run the hello-world example:

```console
% cabal run exe:aihc -- build examples/hello-world/Main.hs
Plan: 3 packages, 1 executable
  aihc-rts-1.0.2  aihc-prim-0.13.0  aihc-base-4.21.2.0  Main (executable)
✔ Main (executable)  2 modules  0.2 s
executable: examples/hello-world/Main
% ./examples/hello-world/Main
Hello, world!
```

The [user guide](https://ai-haskell-compiler.github.io/aihc/) has the full instructions.

## Latest News

<!-- AUTO-GENERATED: START latest-news -->
**[AIHC weekly highlights: a compiler that builds itself and learns from conflicts](https://blog.aihc.app/posts/2026-11-13-aihc-highlights/)** (13 Nov 2026)

Default-configuration self-compilation, a single-capability STM fix, PubGrub dependency solving, and precise n+k parser support.

Read all posts at [blog.aihc.app](https://blog.aihc.app/).
<!-- AUTO-GENERATED: END latest-news -->

## Performance

<!-- AUTO-GENERATED: START perf-highlights -->
Each number is the AIHC value divided by the GHC value, as a geometric mean over 13 benchmarks. Lower is better. 1.00× is parity.

| Metric | Native | LLVM | Wasm |
| --- | ---: | ---: | ---: |
| Compile time `-O0` | 0.12× | 0.14× | 0.30× |
| Artifact size `-Os` | 0.10× | 0.09× | 0.52× |
| Runtime `-O1` | 1.05× | 1.16× | 1.51× |
| Runtime `-O2` | 0.94× | 1.09× | 0.82× |

Machine [`apple-m4-pro-542f1e`](https://perf.aihc.app/timeline.html?machine=apple-m4-pro-542f1e), commit [`df6a01f74`](https://github.com/ai-haskell-compiler/aihc/commit/df6a01f7420d5f583f86d81089a11285e4bd51c7) (2026-11-13). Get all results at [perf.aihc.app](https://perf.aihc.app/).
<!-- AUTO-GENERATED: END perf-highlights -->


## Self hosting

AIHC can now compile itself ("self hosting").
The [Self-Compile workflow](https://github.com/ai-haskell-compiler/aihc/actions/workflows/self-compile.yml) passed on `linux-amd64` with the native backend at `-O2`.
It uses the minimal compiler configuration, with the `hackage` and `pretty-ui` flags disabled.
The compiler built by AIHC compiles itself again and produces an identical executable.
Expand the details to see the required packages and their installation status.

<!-- AUTO-GENERATED: START self-hosting-details -->
<details>
<summary>Self-compile packages: 131 install, 1 fail, 0 wait for a dependency</summary>

Each package of [the self-hosting package list](docs/self-hosting-packages.md), in dependency order.

| Package | Version | Status |
| ------- | ------- | ------ |
| OneTuple | 0.4.3 | ✅ installs |
| array | 0.5.8.0 | ✅ installs |
| assoc | 1.1.1 | ✅ installs |
| atomic-counter | 0.1.2.4 | ✅ installs |
| base-orphans | 0.9.4 | ✅ installs |
| character-ps | 0.1 | ✅ installs |
| colour | 2.3.7 | ✅ installs |
| ansi-terminal-types | 1.1.3 | ✅ installs |
| ansi-terminal | 1.1.5 | ✅ installs |
| deepseq | 1.5.2.0 | ✅ installs |
| bytestring | 0.12.2.0 | ✅ installs |
| appar | 0.1.8 | ✅ installs |
| base16-bytestring | 1.0.2.0 | ✅ installs |
| base64-bytestring | 1.2.1.0 | ✅ installs |
| containers | 0.7 | ✅ installs |
| binary | 0.8.9.3 | ✅ installs |
| cereal | 0.5.8.3 | ✅ installs |
| commutative-semigroups | 0.2.0.2 | ✅ installs |
| cryptohash-md5 | 0.11.101.0 | ✅ installs |
| cryptohash-sha256 | 0.11.102.1 | ✅ installs |
| data-default | 0.8.0.2 | ✅ installs |
| data-default-class | 0.2.0.0 | ✅ installs |
| dlist | 1.0 | ✅ installs |
| ghc-bignum | 1.3 | ✅ installs |
| groups | 0.5.3 | ✅ installs |
| half | 0.3.3 | ✅ installs |
| integer-gmp | 1.1 | ✅ installs |
| integer-logarithms | 1.0.5 | ✅ installs |
| network-byte-order | 0.1.9 | ✅ installs |
| parser-combinators | 1.3.1 | ✅ installs |
| primes | 0.2.1.0 | ✅ installs |
| ram | 0.22.1 | ✅ installs |
| splitmix | 0.1.3.2 | ✅ installs |
| stm | 2.5.3.1 | ✅ installs |
| tagged | 0.8.11 | ✅ installs |
| terminal-size | 0.3.4 | ✅ installs |
| text | 2.1.4 | ✅ installs |
| blaze-builder | 0.4.4.1 | ✅ installs |
| mime-types | 0.1.2.2 | ✅ installs |
| prettyprinter | 1.7.2 | ✅ installs |
| prettyprinter-ansi-terminal | 1.1.4 | ✅ installs |
| th-abstraction | 0.7.2.0 | ✅ installs |
| th-compat | 0.1.7 | ✅ installs |
| time | 1.14 | ✅ installs |
| cookie | 0.5.1 | ✅ installs |
| time-hourglass | 0.3.0 | ✅ installs |
| transformers | 0.6.3.0 | ✅ installs |
| StateVar | 1.2.2 | ✅ installs |
| contravariant | 1.5.6 | ✅ installs |
| distributive | 0.6.3 | ✅ installs |
| indexed-traversable | 0.1.5 | ✅ installs |
| comonad | 5.0.10 | ✅ installs |
| bifunctors | 5.6.3 | ✅ installs |
| mtl | 2.3.2 | ✅ installs |
| exceptions | 0.10.12 | ✅ installs |
| os-string | 2.0.11 | ✅ installs |
| filepath | 1.5.5.0 | ✅ installs |
| aihc-cpp | 2.0.0.0 | ✅ installs |
| ech-config | 0.0.2 | ✅ installs |
| hashable | 1.5.1.0 | ✅ installs |
| case-insensitive | 1.2.2.0 | ✅ installs |
| data-fix | 0.3.4 | ✅ installs |
| http-types | 0.12.6 | ✅ installs |
| parsec | 3.1.18.0 | ✅ installs |
| network-uri | 2.6.4.2 | ✅ installs |
| primitive | 0.9.1.0 | ✅ installs |
| cborg | 0.2.10.0 | ✅ installs |
| integer-conversion | 0.1.1 | ✅ installs |
| random | 1.3.1 | ✅ installs |
| QuickCheck | 2.18.0.0 | ✅ installs |
| scientific | 0.3.10.0 | ✅ installs |
| megaparsec | 9.8.3 | ✅ installs |
| aihc-cabal-syntax | 2.0.0.0 | ✅ installs |
| aihc-parser | 5.0.0.0 | ✅ installs |
| text-short | 0.1.6.1 | ✅ installs |
| base16 | 2.0 | ✅ installs |
| base64 | 2.1 | ✅ installs |
| crypton | 2.2.6 | ✅ installs |
| crypton-asn1-types | 0.4.1 | ✅ installs |
| crypton-asn1-encoding | 0.10.0 | ✅ installs |
| crypton-asn1-parse | 0.10.0 | ✅ installs |
| crypton-pem | 0.3.0 | ✅ installs |
| crypton-x509 | 1.10.1 | ✅ installs |
| hpke | 0.4.0 | ✅ installs |
| these | 1.2.1 | ✅ installs |
| strict | 0.5.1 | ✅ installs |
| time-compat | 1.9.9 | ✅ installs |
| text-iso8601 | 0.1.1.2 | ✅ installs |
| transformers-compat | 0.7.2 | ✅ installs |
| unix | 2.8.8.0 | ✅ installs |
| directory-ospath-streaming | 0.3 | ✅ installs |
| file-io | 0.1.6 | ✅ installs |
| directory | 1.3.11.0 | ✅ installs |
| aihc-hackage | 0.1.0.0 | ✅ installs |
| crypton-x509-store | 1.10.0 | ✅ installs |
| network | 3.2.12.0 | ✅ installs |
| crypton-socks | 0.6.2 | ✅ installs |
| iproute | 1.8.0 | ✅ installs |
| crypton-x509-validation | 1.10.1 | ✅ installs |
| process | 1.6.30.0 | ✅ installs |
| crypton-x509-system | 1.10.0 | ✅ installs |
| optparse-applicative | 0.18.1.0 | ✅ installs |
| tar | 0.6.4.0 | ✅ installs |
| unix-time | 0.6.0 | ✅ installs |
| unordered-containers | 0.2.21 | ✅ installs |
| async | 2.2.6 | ✅ installs |
| semigroupoids | 6.0.2 | ✅ installs |
| uuid-types | 1.0.6.1 | ✅ installs |
| vector-stream | 0.1.0.1 | ✅ installs |
| vector | 0.13.2.0 | ✅ installs |
| indexed-traversable-instances | 0.1.2.1 | ✅ installs |
| monoid-subclasses | 1.2.6.1 | ✅ installs |
| monoidmap-internal | 0.5.1.0 | ✅ installs |
| semialign | 1.4 | ✅ installs |
| serialise | 0.2.6.1 | ✅ installs |
| wherefrom-compat | 0.2.0.0 | ✅ installs |
| nothunks | 0.3.2 | ✅ installs |
| monoidmap | 0.1.3.1 | ✅ installs |
| aihc-resolve | 0.1.0.0 | ✅ installs |
| aihc-tc | 0.1.0.0 | ✅ installs |
| witherable | 0.5 | ✅ installs |
| aeson | 2.2.5.1 | ✅ installs |
| aihc-package-plan | 0.1.0.0 | ✅ installs |
| zlib | 0.7.1.1 | ✅ installs |
| streaming-commons | 0.2.3.1 | ✅ installs |
| http-client | 0.7.19 | ✅ installs |
| tls | 2.5.1 | ✅ installs |
| crypton-connection | 0.4.6 | ✅ installs |
| http-client-tls | 0.4.0 | ✅ installs |
| aihc-http | 0.1.0.0 | ✅ installs |
| aihc-hackage-fetch | 0.1.0.0 | ✅ installs |
| aihc | 0.1.0.0 | ❌ fails (linker command failed with exit code 1 (use -v to see invocation)) |

</details>
<!-- AUTO-GENERATED: END self-hosting-details -->

### Self-compile performance

<!-- AUTO-GENERATED: START self-compile-perf -->
The [Self-Compile Performance workflow](https://github.com/ai-haskell-compiler/aihc/actions/workflows/self-compile-perf.yml) compiles aihc 5 times.
Each build starts with an empty package store.
Before the time starts, the build installs all dependencies of aihc.
Thus, each time is for the local packages of this repository and the `aihc` executable.
GHC compiles at its default level `-O1`.
Each aihc build uses the native backend for `linux-amd64`.
The `-O2` stage1 compiles both stage2 builds.
Lower is better.

| Stage | Compiler | Level | Build time | Relative to stage0 | Executable size |
| --- | --- | --- | ---: | ---: | ---: |
| stage0 | GHC 9.12.4 | `-O1` | 3m 04s | 1.00× | 91.5 MB |
| stage1 | aihc stage0 | `-O2` | 8m 48s | 2.87× | 143.2 MB |
| stage2 | aihc stage1 | `-O2` | 28m 10s | 9.18× | 143.2 MB |
| stage1 | aihc stage0 | `-O1` | 1m 21s | 0.44× | 236.6 MB |
| stage2 | aihc stage1 | `-O1` | 11m 40s | 3.80× | 236.6 MB |

Machine `ubuntu24 (8 cores)`, commit [`eb4ea411b`](https://github.com/ai-haskell-compiler/aihc/commit/eb4ea411bbb432c0e916992dc5b187eb9c304ddd) (2026-11-08).
<!-- AUTO-GENERATED: END self-compile-perf -->

## FAQ

### Can AIHC compile itself?

Yes. AIHC can compile itself and produce an identical executable on the next compile.
See [Self hosting](#self-hosting) for the current status.

### Is AIHC compatible with GHC?

Not fully.
AIHC aims at compiling any Haskell code that GHC accepts, but some programs do not compile yet.
The progress table above and [Self hosting](#self-hosting) show how far along that work is.

### Which architectures does AIHC support?

`apple-arm64`, `linux-amd64`, and `wasm32`.
Use `--target` to select one.

### Does AIHC support Template Haskell and quasi-quotes?

Not yet.
AIHC parses the syntax but does not run splices or quasi-quoters.
Support is planned.

### Which Hackage packages does AIHC install?

See [Self hosting](#self-hosting) for the packages that install today.

### What kind of garbage collector does AIHC use?

A generational, incremental collector with precise roots.
Young objects are bump allocated in a nursery and copied on survival.
Old objects are collected with an incremental mark and sweep, so each pause has a bound that does not depend on the live data.
The same collector runs on all backends.
See [the GC design](docs/gc-design.md) for the details.

### How fast is the code that AIHC makes?

Slower than GHC.
See [Performance](#performance) for the current numbers.

### Is there a binary release?

No.
Build the compiler from source with `cabal build exe:aihc`.

### Did humans write any of the code?

AI agents wrote the compiler code.
Humans wrote the prompts and reviewed the results.

### How do I run the test suite?

Run `just check`.
For a hermetic build environment, run `nix flake check`.

### What is the license?

AIHC is in the public domain.
See [LICENSE](LICENSE).
