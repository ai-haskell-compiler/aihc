[![User guide](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=user%20guide)](https://ai-haskell-compiler.github.io/aihc/)
[![API docs](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=API%20docs)](https://ai-haskell-compiler.github.io/aihc/api/)
[![Generated Reports](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/generated-reports-update.yml?label=reports)](https://github.com/ai-haskell-compiler/aihc/actions/workflows/generated-reports-update.yml)

# AI-written Haskell Compiler (aihc)

Can gpt, Claude Opus and Qwen-Coder write a Haskell compiler? Probably not but let's find out. We'll need preprocessing, parsing, name resolution, type checking, desugaring, and code generation. Progress will be tracked as a percentage of stackage for each component.

Find more information here:
- [**aihc-cpp** README](https://github.com/ai-haskell-compiler/aihc-cpp#readme)
- [**aihc-parser** README](https://github.com/ai-haskell-compiler/aihc-parser#readme) - [Supported extensions](https://github.com/ai-haskell-compiler/aihc-parser/blob/main/docs/aihc-parser-supported-extensions.md)
- [**aihc-resolve** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-resolve#readme) - [Supported extensions](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/aihc-resolve-supported-extensions.md)
- [**aihc-tc** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-tc#readme) - [Supported extensions](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/aihc-tc-supported-extensions.md)

## Progress

| Name               | Progress                                                                                                                             |
| ------------------ | -----------------------------------------------------------------------------------------------------------------------------------: |
| aihc-prim / ghc-prim | <!-- AUTO-GENERATED: START ghc-prim-progress --> `770/5013` (`15.36%`) ○○○○○ <!-- AUTO-GENERATED: END ghc-prim-progress -->                    |
| aihc-base / base   | <!-- AUTO-GENERATED: START base-progress --> `2111/10061` (`20.98%`) ●○○○○ <!-- AUTO-GENERATED: END base-progress -->                             |
| Self-compile       | <!-- AUTO-GENERATED: START self-hosting-progress --> `47/118` (`39.83%`) ●○○○○ <!-- AUTO-GENERATED: END self-hosting-progress --> |
| &nbsp; | &nbsp; |
| TypeCheck Tests    | <!-- AUTO-GENERATED: START tc-progress --> `644/649` (`99.22%`) ●●●●○ <!-- AUTO-GENERATED: END tc-progress -->                                |
| Resolve Tests      | <!-- AUTO-GENERATED: START resolve-progress --> `118/118` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END resolve-progress -->                      |

<!-- AUTO-GENERATED: START self-hosting-details -->
<details>
<summary>Self-compile packages: 47 install, 13 fail, 58 wait for a dependency</summary>

Each package of [the self-hosting package list](docs/self-hosting-packages.md), in dependency order.

| Package | Version | Status |
| ------- | ------- | ------ |
| OneTuple | 0.4.3 | ✅ installs |
| array | 0.5.8.0 | ✅ installs |
| assoc | 1.1.1 | ✅ installs |
| atomic-counter | 0.1.2.4 | ✅ installs |
| base-orphans | 0.9.4 | ✅ installs |
| basement | 0.0.16 | ❌ fails (readCreateProcess: /nix/store/4jmqqwmv1gp4aipw0facwcxjdi283dcf-hsc2hs-0.68.10/bin/hsc2hs "--version" (exit 1): failed) |
| byteorder | 1.0.4 | ✅ installs |
| character-ps | 0.1 | ✅ installs |
| colour | 2.3.7 | ✅ installs |
| ansi-terminal-types | 1.1.3 | ✅ installs |
| ansi-terminal | 1.1.5 | ✅ installs |
| deepseq | 1.5.2.0 | ✅ installs |
| bytestring | 0.12.2.0 | ✅ installs |
| appar | 0.1.8 | ✅ installs |
| base64-bytestring | 1.2.1.0 | ✅ installs |
| containers | 0.8 | ✅ installs |
| binary | 0.8.9.3 | ✅ installs |
| cereal | 0.5.8.3 | ✅ installs |
| cryptohash-sha256 | 0.11.102.1 | ✅ installs |
| data-default | 0.8.0.2 | ❌ fails (unsolved constraint Monoid (Proxy a)) |
| data-default-class | 0.2.0.0 | ⏸️ needs `data-default` |
| dlist | 1.0 | ✅ installs |
| ghc-bignum | 1.3 | ✅ installs |
| hourglass | 0.2.12 | ✅ installs |
| integer-gmp | 1.1 | ❌ fails (module ‘GHC.Num.Primitives’ not found) |
| integer-logarithms | 1.0.5 | ✅ installs |
| libffi | 0.2.1 | ❌ fails (readCreateProcess: /nix/store/4jmqqwmv1gp4aipw0facwcxjdi283dcf-hsc2hs-0.68.10/bin/hsc2hs "--version" (exit 1): failed) |
| memory | 0.18.0 | ⏸️ needs `basement` |
| asn1-types | 0.3.4 | ⏸️ needs `memory` |
| asn1-encoding | 0.9.6 | ⏸️ needs `asn1-types` |
| asn1-parse | 0.9.5 | ⏸️ needs `asn1-encoding`, `asn1-types` |
| crypton | 1.0.6 | ⏸️ needs `basement`, `integer-gmp`, `memory` |
| parser-combinators | 1.3.1 | ✅ installs |
| pem | 0.2.4 | ⏸️ needs `basement`, `memory` |
| pretty | 1.1.3.6 | ✅ installs |
| splitmix | 0.1.3.2 | ✅ installs |
| stm | 2.5.3.1 | ✅ installs |
| tagged | 0.8.10 | ✅ installs |
| terminfo | 0.4.1.7 | ❌ fails ('term.h' file not found) |
| text | 2.1.4 | ✅ installs |
| blaze-builder | 0.4.4.1 | ✅ installs |
| mime-types | 0.1.2.2 | ✅ installs |
| prettyprinter | 1.7.2 | ✅ installs |
| prettyprinter-ansi-terminal | 1.1.4 | ✅ installs |
| th-abstraction | 0.7.2.0 | ✅ installs |
| th-compat | 0.1.7 | ❌ fails (kind mismatch: expected r, got r) |
| time | 1.14 | ❌ fails (readCreateProcess: /nix/store/4jmqqwmv1gp4aipw0facwcxjdi283dcf-hsc2hs-0.68.10/bin/hsc2hs "--version" (exit 1): failed) |
| cookie | 0.5.1 | ⏸️ needs `data-default-class`, `time` |
| transformers | 0.6.3.0 | ✅ installs |
| StateVar | 1.2.2 | ❌ fails (unsolved constraint HasSetter t a) |
| contravariant | 1.5.6 | ⏸️ needs `StateVar` |
| crypton-x509 | 1.7.7 | ⏸️ needs `asn1-encoding`, `asn1-parse`, `asn1-types`, `crypton`, `memory`, `pem` |
| distributive | 0.6.3 | ✅ installs |
| indexed-traversable | 0.1.5 | ❌ fails (not exported: term name ‘build’) |
| comonad | 5.0.10 | ⏸️ needs `indexed-traversable` |
| bifunctors | 5.6.3 | ⏸️ needs `comonad` |
| mtl | 2.3.2 | ✅ installs |
| exceptions | 0.10.12 | ✅ installs |
| filepath | 1.4.301.0 | ✅ installs |
| aihc-cpp | 2.0.0.0 | ✅ installs |
| os-string | 2.0.11 | ✅ installs |
| hashable | 1.5.1.0 | ❌ fails (Lir backend failed: LlvmLirLintErrors [LintError {lintErrorSymbol = Just (Symbol {unSymbol = "hashable__m1__d5__d1__d0_D) |
| case-insensitive | 1.2.1.0 | ⏸️ needs `hashable` |
| data-fix | 0.3.4 | ⏸️ needs `hashable` |
| http-types | 0.12.6 | ⏸️ needs `case-insensitive` |
| parsec | 3.1.18.0 | ✅ installs |
| network-uri | 2.6.4.2 | ⏸️ needs `th-compat` |
| primitive | 0.9.1.0 | ✅ installs |
| integer-conversion | 0.1.1 | ✅ installs |
| random | 1.3.1 | ✅ installs |
| QuickCheck | 2.18.0.0 | ✅ installs |
| scientific | 0.3.8.1 | ⏸️ needs `hashable` |
| megaparsec | 9.8.3 | ⏸️ needs `case-insensitive`, `scientific` |
| aihc-parser | 4.0.0.0 | ⏸️ needs `megaparsec` |
| aihc-resolve | 0.1.0.0 | ⏸️ needs `aihc-parser` |
| aihc-tc | 0.1.0.0 | ⏸️ needs `aihc-parser`, `aihc-resolve` |
| text-short | 0.1.6.1 | ⏸️ needs `hashable` |
| these | 1.2.1 | ⏸️ needs `hashable` |
| strict | 0.5.1 | ⏸️ needs `hashable`, `these` |
| time-compat | 1.9.9 | ⏸️ needs `hashable`, `time` |
| text-iso8601 | 0.1.1.2 | ⏸️ needs `time`, `time-compat` |
| transformers-compat | 0.7.2 | ❌ fails (unbound term name ‘<=##’) |
| unix | 2.8.8.0 | ⏸️ needs `time` |
| directory-ospath-streaming | 0.3 | ⏸️ needs `unix` |
| file-io | 0.1.6 | ⏸️ needs `unix` |
| directory | 1.3.10.1 | ⏸️ needs `file-io`, `time`, `unix` |
| Cabal-syntax | 3.16.1.0 | ⏸️ needs `directory`, `time` |
| crypton-x509-store | 1.6.14 | ⏸️ needs `asn1-encoding`, `asn1-types`, `crypton`, `crypton-x509`, `directory`, `pem`, `unix` |
| network | 3.2.9.0 | ⏸️ needs `directory` |
| crypton-socks | 0.6.2 | ⏸️ needs `network` |
| iproute | 1.7.15 | ⏸️ needs `network` |
| crypton-x509-validation | 1.6.14 | ⏸️ needs `asn1-encoding`, `asn1-types`, `crypton`, `crypton-x509`, `crypton-x509-store`, `data-default`, `iproute`, `memory`, `pem` |
| process | 1.6.30.0 | ⏸️ needs `directory`, `unix` |
| Cabal | 3.16.1.0 | ⏸️ needs `Cabal-syntax`, `directory`, `process`, `time`, `unix` |
| crypton-x509-system | 1.6.8 | ⏸️ needs `crypton-x509`, `crypton-x509-store`, `directory`, `pem`, `process` |
| haskeline | 0.8.5.0 | ⏸️ needs `directory`, `process`, `terminfo`, `unix` |
| optparse-applicative | 0.18.1.0 | ⏸️ needs `process`, `transformers-compat` |
| tar | 0.6.4.0 | ⏸️ needs `directory`, `directory-ospath-streaming`, `file-io`, `time` |
| unix-time | 0.5.0 | ❌ fails (readCreateProcess: /nix/store/4jmqqwmv1gp4aipw0facwcxjdi283dcf-hsc2hs-0.68.10/bin/hsc2hs "--version" (exit 1): failed) |
| unordered-containers | 0.2.21 | ⏸️ needs `hashable` |
| async | 2.2.6 | ⏸️ needs `hashable`, `unordered-containers` |
| semigroupoids | 6.0.2 | ⏸️ needs `bifunctors`, `comonad`, `contravariant`, `hashable`, `transformers-compat`, `unordered-containers` |
| tls | 1.9.0 | ⏸️ needs `asn1-encoding`, `asn1-types`, `async`, `crypton`, `crypton-x509`, `crypton-x509-store`, `crypton-x509-validation`, `data-default-class`, `memory`, `network`, `unix-time` |
| crypton-connection | 0.4.5 | ⏸️ needs `crypton-socks`, `crypton-x509-store`, `crypton-x509-system`, `data-default`, `network`, `tls` |
| uuid-types | 1.0.6.1 | ⏸️ needs `hashable` |
| vector-stream | 0.1.0.1 | ✅ installs |
| vector | 0.13.2.0 | ✅ installs |
| indexed-traversable-instances | 0.1.2.1 | ⏸️ needs `indexed-traversable`, `unordered-containers` |
| semialign | 1.4 | ⏸️ needs `hashable`, `indexed-traversable`, `indexed-traversable-instances`, `semigroupoids`, `these`, `unordered-containers` |
| witherable | 0.5 | ⏸️ needs `hashable`, `indexed-traversable`, `indexed-traversable-instances`, `unordered-containers` |
| aeson | 2.2.5.1 | ⏸️ needs `data-fix`, `hashable`, `indexed-traversable`, `network-uri`, `scientific`, `semialign`, `strict`, `text-iso8601`, `text-short`, `these`, `time`, `time-compat`, `unordered-containers`, `uuid-types`, `witherable` |
| zlib | 0.7.1.1 | ❌ fails (readCreateProcess: /nix/store/4jmqqwmv1gp4aipw0facwcxjdi283dcf-hsc2hs-0.68.10/bin/hsc2hs "--version" (exit 1): failed) |
| streaming-commons | 0.2.3.1 | ⏸️ needs `async`, `directory`, `network`, `process`, `unix`, `zlib` |
| http-client | 0.7.19 | ⏸️ needs `async`, `case-insensitive`, `cookie`, `http-types`, `iproute`, `network`, `network-uri`, `streaming-commons`, `time` |
| http-client-tls | 0.3.6.4 | ⏸️ needs `case-insensitive`, `crypton`, `crypton-connection`, `data-default`, `http-client`, `http-types`, `memory`, `network`, `network-uri`, `tls` |
| aihc-hackage | 0.1.0.0 | ⏸️ needs `Cabal`, `Cabal-syntax`, `directory`, `http-client`, `http-client-tls`, `http-types`, `tar`, `time`, `zlib` |
| aihc-package-plan | 0.1.0.0 | ⏸️ needs `Cabal-syntax`, `aeson`, `aihc-hackage`, `aihc-parser`, `directory`, `time` |
| aihc | 0.1.0.0 | ⏸️ needs `Cabal-syntax`, `aeson`, `aihc-hackage`, `aihc-package-plan`, `aihc-parser`, `aihc-resolve`, `aihc-tc`, `async`, `directory`, `haskeline`, `libffi`, `megaparsec`, `optparse-applicative`, `process`, `unix` |

</details>
<!-- AUTO-GENERATED: END self-hosting-details -->

The "Self-compile" row counts the packages of the `aihc` executable that aihc can compile, with `aihc` itself as the last package. `aihc plan bin/aihc --executable aihc` finds the packages, and [docs/self-hosting-packages.md](docs/self-hosting-packages.md) lists them. A package waits when one of its dependencies does not install.

The `aihc-prim` and `aihc-base` rows count the exports of GHC's `ghc-prim` and `base` that aihc provides with the same name and signature (up to type variable names, `forall` prefixes and kind synonyms; a type without a standalone kind signature matches any kind). Compatibility also runs the other way: a module that exists in `ghc-prim` or `base` may only export names that GHC exports from it too, so code written against aihc keeps compiling with GHC. The `dev-spec` test suite fails on any new divergence; `cabal run aihc-dev -- core-libs-progress --divergences` lists the current ones. Modules that only aihc defines are free to export anything.

## Useful Commands

Run the full test suite:

```
just check
```

Run the full test suite in a hermetic build environment (slower than `just check`):

```bash
nix flake check
```
