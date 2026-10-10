[![User guide](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=user%20guide)](https://ai-haskell-compiler.github.io/aihc/)
[![API docs](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=API%20docs)](https://ai-haskell-compiler.github.io/aihc/api/)
[![Generated Reports](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/generated-reports-update.yml?label=reports)](https://github.com/ai-haskell-compiler/aihc/actions/workflows/generated-reports-update.yml)
[![Self-Compile](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/self-compile.yml?branch=main&label=Self-Compile)](https://github.com/ai-haskell-compiler/aihc/actions/workflows/self-compile.yml)
[![Discord](https://img.shields.io/discord/1555935190487142460?label=discord&logo=discord&logoColor=white&color=5865F2)](https://discord.gg/uGWkhMCZrZ)


# AI-written Haskell Compiler (aihc)

Can chatgpt, Claude Opus and Qwen-Coder write a Haskell compiler? Probably not but let's find out. We'll need:

| Stage | Progress | Notes |
| --- | --- | --- |
| Preprocessing | ●●●●● done | see [aihc-cpp](https://github.com/ai-haskell-compiler/aihc-cpp) |
| Parsing | ●●●●● done | see [aihc-parser](https://github.com/ai-haskell-compiler/aihc-parser) |
| Name resolution | <!-- AUTO-GENERATED: START resolve-progress --> ●●●●● `122/122` (`100.00%`) <!-- AUTO-GENERATED: END resolve-progress --> | fixture pass rate |
| Type checking | <!-- AUTO-GENERATED: START tc-progress --> ●●●●○ `376/381` (`98.68%`) <!-- AUTO-GENERATED: END tc-progress --> | fixture pass rate |
| Desugaring | <!-- AUTO-GENERATED: START desugar-progress --> ●●●●○ `866/917` (`94.43%`) <!-- AUTO-GENERATED: END desugar-progress --> | fixture pass rate |
| Code generation | <!-- AUTO-GENERATED: START codegen-progress --> ●●●●● `237/237` (`100.00%`) <!-- AUTO-GENERATED: END codegen-progress --> | machine code, LLVM IR, WASM |
| `ghc-prim` shim | <!-- AUTO-GENERATED: START ghc-prim-progress --> ○○○○○ `785/5013` (`15.66%`) <!-- AUTO-GENERATED: END ghc-prim-progress --> | exports implemented |
| `base` implementation | <!-- AUTO-GENERATED: START base-progress --> ●○○○○ `2173/10061` (`21.60%`) <!-- AUTO-GENERATED: END base-progress --> | exports implemented |
| Self-host | <!-- AUTO-GENERATED: START self-hosting-progress --> ●●●●● `77/77` (`100.00%`) <!-- AUTO-GENERATED: END self-hosting-progress --> | packages that install, see [below](#self-hosting) |

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
**[AIHC this week: the compiler compiles itself, then tests the result](https://blog.aihc.app/posts/2026-10-09-aihc-highlights/)** (09 Oct 2026)

An identical self-built compiler, incremental GC defects, faster inliner decisions with a runtime tradeoff, and Hackage downloads on WebAssembly.

Read all posts at [blog.aihc.app](https://blog.aihc.app/).
<!-- AUTO-GENERATED: END latest-news -->

## Performance

<!-- AUTO-GENERATED: START perf-highlights -->
Each number is the AIHC value divided by the GHC value, as a geometric mean over 7 benchmarks. Lower is better. 1.00× is parity.

| Metric | Native | LLVM | Wasm |
| --- | ---: | ---: | ---: |
| Compile time `-O0` | 0.25× | 0.20× | 0.36× |
| Artifact size `-Os` | 0.71× | 0.96× | 0.57× |
| Runtime `-O1` | 23.3× | 21.6× | 38.0× |
| Runtime `-O2` | 3.91× | 3.30× | 4.39× |

Machine [`intel-i7-8705g-de9b72`](https://perf.aihc.app/timeline.html?machine=intel-i7-8705g-de9b72), commit [`f08dbf4fa`](https://github.com/ai-haskell-compiler/aihc/commit/f08dbf4fad1c991db7fe5c56db854b099b7d8001) (2026-10-10). Get all results at [perf.aihc.app](https://perf.aihc.app/).
<!-- AUTO-GENERATED: END perf-highlights -->


## Self hosting

AIHC can now compile itself ("self hosting").
The [Self-Compile workflow](https://github.com/ai-haskell-compiler/aihc/actions/workflows/self-compile.yml) passed on `linux-amd64` with the native backend at `-O2`.
It uses the minimal compiler configuration, with the `hackage` and `pretty-ui` flags disabled.
The compiler built by AIHC compiles itself again and produces an identical executable.
Expand the details to see the required packages and their installation status.

<!-- AUTO-GENERATED: START self-hosting-details -->
<details>
<summary>Self-compile packages: 77 install, 0 fail, 0 wait for a dependency</summary>

Each package of [the self-hosting package list](docs/self-hosting-packages.md), in dependency order.

| Package | Version | Status |
| ------- | ------- | ------ |
| OneTuple | 0.4.3 | ✅ installs |
| array | 0.5.8.0 | ✅ installs |
| assoc | 1.1.1 | ✅ installs |
| base-orphans | 0.9.4 | ✅ installs |
| character-ps | 0.1 | ✅ installs |
| colour | 2.3.7 | ✅ installs |
| ansi-terminal-types | 1.1.3 | ✅ installs |
| ansi-terminal | 1.1.5 | ✅ installs |
| deepseq | 1.5.2.0 | ✅ installs |
| bytestring | 0.12.2.0 | ✅ installs |
| containers | 0.8.1 | ✅ installs |
| binary | 0.8.9.3 | ✅ installs |
| cryptohash-sha256 | 0.11.102.1 | ✅ installs |
| dlist | 1.0 | ✅ installs |
| integer-logarithms | 1.0.5 | ✅ installs |
| parser-combinators | 1.3.1 | ✅ installs |
| splitmix | 0.1.3.2 | ✅ installs |
| stm | 2.5.3.1 | ✅ installs |
| tagged | 0.8.11 | ✅ installs |
| terminal-size | 0.3.4 | ✅ installs |
| text | 2.1.4 | ✅ installs |
| prettyprinter | 1.7.2 | ✅ installs |
| prettyprinter-ansi-terminal | 1.1.4 | ✅ installs |
| th-abstraction | 0.7.2.0 | ✅ installs |
| th-compat | 0.1.7 | ✅ installs |
| time | 1.14 | ✅ installs |
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
| hashable | 1.5.1.0 | ✅ installs |
| case-insensitive | 1.2.1.0 | ✅ installs |
| data-fix | 0.3.4 | ✅ installs |
| parsec | 3.1.18.0 | ✅ installs |
| network-uri | 2.6.4.2 | ✅ installs |
| primitive | 0.9.1.0 | ✅ installs |
| integer-conversion | 0.1.1 | ✅ installs |
| random | 1.3.1 | ✅ installs |
| QuickCheck | 2.18.0.0 | ✅ installs |
| scientific | 0.3.9.0 | ✅ installs |
| megaparsec | 9.8.3 | ✅ installs |
| aihc-cabal-syntax | 2.0.0.0 | ✅ installs |
| aihc-parser | 5.0.0.0 | ✅ installs |
| aihc-resolve | 0.1.0.0 | ✅ installs |
| aihc-tc | 0.1.0.0 | ✅ installs |
| text-short | 0.1.6.1 | ✅ installs |
| these | 1.2.1 | ✅ installs |
| strict | 0.5.1 | ✅ installs |
| time-compat | 1.9.9 | ✅ installs |
| text-iso8601 | 0.1.1.2 | ✅ installs |
| transformers-compat | 0.7.2 | ✅ installs |
| unix | 2.8.8.0 | ✅ installs |
| file-io | 0.2.0 | ✅ installs |
| directory | 1.3.11.0 | ✅ installs |
| aihc-hackage | 0.1.0.0 | ✅ installs |
| process | 1.6.30.0 | ✅ installs |
| optparse-applicative | 0.18.1.0 | ✅ installs |
| unordered-containers | 0.2.21 | ✅ installs |
| async | 2.2.6 | ✅ installs |
| semigroupoids | 6.0.2 | ✅ installs |
| uuid-types | 1.0.6.1 | ✅ installs |
| vector-stream | 0.1.0.1 | ✅ installs |
| vector | 0.13.2.0 | ✅ installs |
| indexed-traversable-instances | 0.1.2.1 | ✅ installs |
| semialign | 1.4 | ✅ installs |
| witherable | 0.5 | ✅ installs |
| aeson | 2.2.5.1 | ✅ installs |
| aihc-package-plan | 0.1.0.0 | ✅ installs |
| aihc | 0.1.0.0 | ✅ installs |

</details>
<!-- AUTO-GENERATED: END self-hosting-details -->

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
