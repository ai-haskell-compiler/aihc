[![User guide](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=user%20guide)](https://ai-haskell-compiler.github.io/aihc/)
[![API docs](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=API%20docs)](https://ai-haskell-compiler.github.io/aihc/api/)
[![Generated Reports](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/generated-reports-update.yml?label=reports)](https://github.com/ai-haskell-compiler/aihc/actions/workflows/generated-reports-update.yml)
[![Discord](https://img.shields.io/badge/discord-join%20chat-5865F2?logo=discord&logoColor=white)](https://discord.gg/uGWkhMCZrZ)


# AI-written Haskell Compiler (aihc)

Can chatgpt, Claude Opus and Qwen-Coder write a Haskell compiler? Probably not but let's find out. We'll need:

| Stage | Progress | Notes |
| --- | --- | --- |
| Preprocessing | ●●●●● done | see [aihc-cpp](https://github.com/ai-haskell-compiler/aihc-cpp) |
| Parsing | ●●●●● done | see [aihc-parser](https://github.com/ai-haskell-compiler/aihc-parser) |
| Name resolution | <!-- AUTO-GENERATED: START resolve-progress --> ●●●●● `122/122` (`100.00%`) <!-- AUTO-GENERATED: END resolve-progress --> | fixture pass rate |
| Type checking | <!-- AUTO-GENERATED: START tc-progress --> ●●●●○ `372/377` (`98.67%`) <!-- AUTO-GENERATED: END tc-progress --> | fixture pass rate |
| Desugaring | <!-- AUTO-GENERATED: START desugar-progress --> ●●●●○ `845/896` (`94.30%`) <!-- AUTO-GENERATED: END desugar-progress --> | fixture pass rate |
| Code generation | <!-- AUTO-GENERATED: START codegen-progress --> ●●●●● `234/234` (`100.00%`) <!-- AUTO-GENERATED: END codegen-progress --> | machine code, LLVM IR, WASM |
| `ghc-prim` shim | <!-- AUTO-GENERATED: START ghc-prim-progress --> ○○○○○ `785/5013` (`15.66%`) <!-- AUTO-GENERATED: END ghc-prim-progress --> | exports implemented |
| `base` implementation | <!-- AUTO-GENERATED: START base-progress --> ●○○○○ `2156/10061` (`21.43%`) <!-- AUTO-GENERATED: END base-progress --> | exports implemented |
| Self-host | <!-- AUTO-GENERATED: START self-hosting-progress --> ●●●●○ `68/77` (`88.31%`) <!-- AUTO-GENERATED: END self-hosting-progress --> | packages that install, see [below](#self-hosting) |

## Latest News

<!-- AUTO-GENERATED: START latest-news -->
**[AIHC over four weeks: from parser decisions to smaller programs](https://blog.aihc.app/posts/2026-10-02-aihc-highlights/)** (02 Oct 2026)

A 28-day retrospective on parser costs, list fusion, worker/wrapper correctness, whole-program memory, and the route to aeson installation.

Read all posts at [blog.aihc.app](https://blog.aihc.app/).
<!-- AUTO-GENERATED: END latest-news -->

## Performance

<!-- AUTO-GENERATED: START perf-highlights -->
Each number is the AIHC value divided by the GHC value, as a geometric mean over 6 benchmarks. Lower is better. 1.00× is parity.

| Metric | Native | LLVM | Wasm |
| --- | ---: | ---: | ---: |
| Compile time `-O0` | 0.21× | 0.17× | 0.34× |
| Artifact size `-Os` | 0.50× | 0.66× | 0.55× |
| Runtime `-O1` | 15.9× | 26.1× | 29.0× |
| Runtime `-O2` | 3.63× | 4.78× | 5.40× |

Machine [`intel-i7-8705g-de9b72`](https://perf.aihc.app/timeline.html?machine=intel-i7-8705g-de9b72), commit [`49b86af5a`](https://github.com/ai-haskell-compiler/aihc/commit/49b86af5a2dd3d9c24ca96d09515278c4e2577d1) (2026-10-03). Get all results at [perf.aihc.app](https://perf.aihc.app/).
<!-- AUTO-GENERATED: END perf-highlights -->


## Self hosting

AIHC compiling itself ("self hosting") is the next milestone. Expand the details to see exactly what packages are required and which fail to install.

<!-- AUTO-GENERATED: START self-hosting-details -->
<details>
<summary>Self-compile packages: 68 install, 3 fail, 6 wait for a dependency</summary>

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
| containers | 0.8 | ✅ installs |
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
| bifunctors | 5.6.3 | ❌ fails (FC generation failed: Data.Bifunctor.Tannen: kind still has a meta variable) |
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
| aihc-parser | 4.0.0.0 | ✅ installs |
| aihc-resolve | 0.1.0.0 | ❌ fails (unbound term name ‘DeclRules’) |
| aihc-tc | 0.1.0.0 | ⏸️ needs `aihc-resolve` |
| text-short | 0.1.6.1 | ✅ installs |
| these | 1.2.1 | ✅ installs |
| strict | 0.5.1 | ✅ installs |
| time-compat | 1.9.9 | ✅ installs |
| text-iso8601 | 0.1.1.2 | ✅ installs |
| transformers-compat | 0.7.2 | ✅ installs |
| unix | 2.8.8.0 | ✅ installs |
| file-io | 0.2.0 | ✅ installs |
| directory | 1.3.11.0 | ✅ installs |
| aihc-hackage | 0.1.0.0 | ❌ fails (unbound term name ‘os’) |
| process | 1.6.30.0 | ✅ installs |
| optparse-applicative | 0.18.1.0 | ✅ installs |
| unordered-containers | 0.2.21 | ✅ installs |
| async | 2.2.6 | ✅ installs |
| semigroupoids | 6.0.2 | ⏸️ needs `bifunctors` |
| uuid-types | 1.0.6.1 | ✅ installs |
| vector-stream | 0.1.0.1 | ✅ installs |
| vector | 0.13.2.0 | ✅ installs |
| indexed-traversable-instances | 0.1.2.1 | ✅ installs |
| semialign | 1.4 | ⏸️ needs `semigroupoids` |
| witherable | 0.5 | ✅ installs |
| aeson | 2.2.5.1 | ⏸️ needs `semialign` |
| aihc-package-plan | 0.1.0.0 | ⏸️ needs `aeson`, `aihc-hackage` |
| aihc | 0.1.0.0 | ⏸️ needs `aeson`, `aihc-hackage`, `aihc-package-plan`, `aihc-resolve`, `aihc-tc` |

</details>
<!-- AUTO-GENERATED: END self-hosting-details -->

## Useful Commands

Run the full test suite:

```
just check
```

Run the full test suite in a hermetic build environment (slower than `just check`):

```bash
nix flake check
```
