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
| aihc-prim / ghc-prim | <!-- AUTO-GENERATED: START ghc-prim-progress --> `724/5013` (`14.44%`) ○○○○○ <!-- AUTO-GENERATED: END ghc-prim-progress -->                    |
| aihc-base / base   | <!-- AUTO-GENERATED: START base-progress --> `2055/10061` (`20.43%`) ●○○○○ <!-- AUTO-GENERATED: END base-progress -->                             |
| Self-compile       | <!-- AUTO-GENERATED: START self-hosting-progress --> `37/129` (`28.68%`) ●○○○○ <!-- AUTO-GENERATED: END self-hosting-progress --> |
| &nbsp; | &nbsp; |
| TypeCheck Tests    | <!-- AUTO-GENERATED: START tc-progress --> `604/609` (`99.17%`) ●●●●○ <!-- AUTO-GENERATED: END tc-progress -->                                |
| Resolve Tests      | <!-- AUTO-GENERATED: START resolve-progress --> `111/111` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END resolve-progress -->                      |

<!-- AUTO-GENERATED: START self-hosting-details -->
<details>
<summary>Self-compile packages: 37 install, 13 fail, 79 wait for a dependency</summary>

Each package of [the self-hosting package list](docs/self-hosting-packages.md), in dependency order.

| Package | Version | Status |
| ------- | ------- | ------ |
| OneTuple | 0.4.3 | ✅ installs |
| array | 0.5.8.0 | ✅ installs |
| assoc | 1.1.1 | ✅ installs |
| atomic-counter | 0.1.2.4 | ❌ fails (kind mismatch: expected 'Lifted, got 'Unlifted) |
| base-orphans | 0.9.4 | ✅ installs |
| basement | 0.0.16 | ❌ fails (not exported: term name ‘plusWord8#’) |
| byteorder | 1.0.4 | ❌ fails (unsolved constraint Show (Word8, Word8, Word8, Word8)) |
| character-ps | 0.1 | ✅ installs |
| colour | 2.3.7 | ✅ installs |
| ansi-terminal-types | 1.1.3 | ✅ installs |
| ansi-terminal | 1.1.5 | ✅ installs |
| deepseq | 1.5.2.0 | ✅ installs |
| bytestring | 0.12.2.0 | ✅ installs |
| appar | 0.1.8 | ✅ installs |
| base16-bytestring | 1.0.2.0 | ✅ installs |
| base64-bytestring | 1.2.1.0 | ✅ installs |
| containers | 0.7 | ❌ fails (FC generation failed: Utils.Containers.Internal.TypeError: cannot infer the invisible kind argument Unique 8 for TypeErr) |
| binary | 0.8.9.3 | ⏸️ needs `containers` |
| cereal | 0.5.8.3 | ⏸️ needs `containers` |
| cryptohash-sha256 | 0.11.102.1 | ✅ installs |
| data-default | 0.8.0.2 | ⏸️ needs `containers` |
| data-default-class | 0.2.0.0 | ⏸️ needs `data-default` |
| dlist | 1.0 | ✅ installs |
| ghc-bignum | 1.3 | ✅ installs |
| half | 0.3.3 | ⏸️ needs `binary` |
| integer-gmp | 1.1 | ❌ fails (module ‘GHC.Num.Primitives’ not found) |
| integer-logarithms | 1.0.5 | ✅ installs |
| libffi | 0.2.1 | ❌ fails ('ffi.h' file not found) |
| memory | 0.18.0 | ⏸️ needs `basement` |
| crypton | 1.0.6 | ⏸️ needs `basement`, `integer-gmp`, `memory` |
| hpke | 0.0.0 | ⏸️ needs `crypton`, `memory` |
| network-byte-order | 0.1.8 | ✅ installs |
| old-locale | 1.0.0.7 | ✅ installs |
| old-time | 1.1.1.0 | ✅ installs |
| parser-combinators | 1.3.1 | ✅ installs |
| pretty | 1.1.3.6 | ✅ installs |
| splitmix | 0.1.3.2 | ✅ installs |
| stm | 2.5.3.1 | ✅ installs |
| tagged | 0.8.10 | ✅ installs |
| terminfo | 0.4.1.7 | ✅ installs |
| text | 2.1.4 | ⏸️ needs `binary` |
| blaze-builder | 0.4.4.1 | ⏸️ needs `text` |
| mime-types | 0.1.2.2 | ⏸️ needs `containers`, `text` |
| prettyprinter | 1.7.2 | ⏸️ needs `text` |
| prettyprinter-ansi-terminal | 1.1.4 | ⏸️ needs `prettyprinter`, `text` |
| th-abstraction | 0.7.2.0 | ⏸️ needs `containers` |
| th-compat | 0.1.7 | ❌ fails (kind mismatch: expected r, got r) |
| time | 1.14 | ✅ installs |
| cookie | 0.5.1 | ⏸️ needs `data-default-class`, `text` |
| time-hourglass | 0.3.0 | ❌ fails (unbound term name ‘localTimeUnwrap’) |
| transformers | 0.6.3.0 | ✅ installs |
| StateVar | 1.2.2 | ❌ fails (unsolved constraint HasSetter t a) |
| contravariant | 1.5.6 | ⏸️ needs `StateVar` |
| distributive | 0.6.3 | ✅ installs |
| indexed-traversable | 0.1.5 | ⏸️ needs `containers` |
| comonad | 5.0.10 | ⏸️ needs `containers`, `indexed-traversable` |
| bifunctors | 5.6.3 | ⏸️ needs `comonad`, `containers`, `th-abstraction` |
| mtl | 2.3.2 | ✅ installs |
| exceptions | 0.10.12 | ✅ installs |
| filepath | 1.4.301.0 | ✅ installs |
| aihc-cpp | 2.0.0.0 | ⏸️ needs `containers`, `text` |
| ech-config | 0.0.1 | ✅ installs |
| os-string | 2.0.11 | ✅ installs |
| hashable | 1.5.1.0 | ⏸️ needs `containers`, `text` |
| case-insensitive | 1.2.1.0 | ⏸️ needs `hashable`, `text` |
| data-fix | 0.3.4 | ⏸️ needs `hashable` |
| http-types | 0.12.6 | ⏸️ needs `case-insensitive`, `text` |
| parsec | 3.1.18.0 | ⏸️ needs `text` |
| network-uri | 2.6.4.2 | ⏸️ needs `parsec`, `th-compat` |
| primitive | 0.9.1.0 | ✅ installs |
| cborg | 0.2.10.0 | ⏸️ needs `containers`, `half`, `text` |
| integer-conversion | 0.1.1 | ⏸️ needs `text` |
| random | 1.3.1 | ✅ installs |
| QuickCheck | 2.18.0.0 | ⏸️ needs `containers` |
| scientific | 0.3.8.1 | ⏸️ needs `binary`, `containers`, `hashable`, `text` |
| megaparsec | 9.8.2 | ⏸️ needs `case-insensitive`, `containers`, `scientific`, `text` |
| aihc-parser | 4.0.0.0 | ⏸️ needs `containers`, `megaparsec`, `prettyprinter`, `text` |
| aihc-resolve | 0.1.0.0 | ⏸️ needs `aihc-parser`, `containers`, `text` |
| aihc-tc | 0.1.0.0 | ⏸️ needs `aihc-parser`, `aihc-resolve`, `containers`, `text` |
| text-short | 0.1.6.1 | ⏸️ needs `binary`, `hashable`, `text` |
| base16 | 1.0 | ⏸️ needs `text`, `text-short` |
| base64 | 1.0 | ⏸️ needs `text`, `text-short` |
| crypton-asn1-types | 0.4.1 | ⏸️ needs `base16`, `time-hourglass` |
| crypton-asn1-encoding | 0.10.0 | ⏸️ needs `crypton-asn1-types`, `time-hourglass` |
| crypton-asn1-parse | 0.10.0 | ⏸️ needs `crypton-asn1-types` |
| crypton-pem | 0.3.0 | ⏸️ needs `base64`, `text` |
| crypton-x509 | 1.8.0 | ⏸️ needs `containers`, `crypton`, `crypton-asn1-encoding`, `crypton-asn1-parse`, `crypton-asn1-types`, `crypton-pem`, `memory`, `time-hourglass` |
| these | 1.2.1 | ⏸️ needs `binary`, `hashable` |
| strict | 0.5.1 | ⏸️ needs `binary`, `hashable`, `text`, `these` |
| time-compat | 1.9.9 | ⏸️ needs `hashable` |
| text-iso8601 | 0.1.1.2 | ⏸️ needs `integer-conversion`, `text`, `time-compat` |
| transformers-compat | 0.7.2 | ❌ fails (unbound term name ‘<=##’) |
| unix | 2.8.8.0 | ❌ fails (invalid redefinition of function 'write') |
| directory-ospath-streaming | 0.3 | ⏸️ needs `atomic-counter`, `unix` |
| file-io | 0.1.6 | ⏸️ needs `unix` |
| directory | 1.3.10.1 | ⏸️ needs `file-io`, `unix` |
| Cabal-syntax | 3.16.1.0 | ⏸️ needs `binary`, `containers`, `directory`, `parsec`, `text` |
| crypton-x509-store | 1.8.0 | ⏸️ needs `containers`, `crypton`, `crypton-asn1-encoding`, `crypton-asn1-types`, `crypton-pem`, `crypton-x509`, `directory`, `unix` |
| network | 3.2.9.0 | ⏸️ needs `directory` |
| crypton-socks | 0.6.2 | ⏸️ needs `cereal`, `network` |
| iproute | 1.7.15 | ⏸️ needs `byteorder`, `containers`, `network` |
| crypton-x509-validation | 1.8.0 | ⏸️ needs `containers`, `crypton`, `crypton-asn1-encoding`, `crypton-asn1-types`, `crypton-pem`, `crypton-x509`, `crypton-x509-store`, `data-default`, `iproute`, `memory`, `time-hourglass` |
| process | 1.6.30.0 | ⏸️ needs `directory`, `unix` |
| Cabal | 3.16.1.0 | ⏸️ needs `Cabal-syntax`, `containers`, `directory`, `parsec`, `process`, `unix` |
| crypton-x509-system | 1.8.0 | ⏸️ needs `containers`, `crypton-pem`, `crypton-x509`, `crypton-x509-store`, `directory`, `process` |
| haskeline | 0.8.5.0 | ⏸️ needs `containers`, `directory`, `process`, `unix` |
| optparse-applicative | 0.18.1.0 | ⏸️ needs `prettyprinter`, `prettyprinter-ansi-terminal`, `process`, `text`, `transformers-compat` |
| tar | 0.6.4.0 | ⏸️ needs `containers`, `directory`, `directory-ospath-streaming`, `file-io` |
| unix-time | 0.4.17 | ⏸️ needs `binary` |
| unordered-containers | 0.2.21 | ⏸️ needs `hashable` |
| async | 2.2.6 | ⏸️ needs `hashable`, `unordered-containers` |
| semigroupoids | 6.0.2 | ⏸️ needs `bifunctors`, `comonad`, `containers`, `contravariant`, `hashable`, `transformers-compat`, `unordered-containers` |
| uuid-types | 1.0.6.1 | ⏸️ needs `binary`, `hashable`, `text` |
| vector-stream | 0.1.0.1 | ✅ installs |
| vector | 0.13.2.0 | ❌ fails (FC generation failed: Data.Vector.Unboxed.Base: data-family instance MVector does not match the type TcTyCon (TyConInter) |
| indexed-traversable-instances | 0.1.2.1 | ⏸️ needs `indexed-traversable`, `unordered-containers`, `vector` |
| semialign | 1.4 | ⏸️ needs `containers`, `hashable`, `indexed-traversable`, `indexed-traversable-instances`, `semigroupoids`, `these`, `unordered-containers`, `vector` |
| serialise | 0.2.6.1 | ⏸️ needs `cborg`, `containers`, `half`, `hashable`, `strict`, `text`, `these`, `unordered-containers`, `vector` |
| witherable | 0.5 | ⏸️ needs `containers`, `hashable`, `indexed-traversable`, `indexed-traversable-instances`, `unordered-containers`, `vector` |
| aeson | 2.2.5.1 | ⏸️ needs `QuickCheck`, `containers`, `data-fix`, `hashable`, `indexed-traversable`, `integer-conversion`, `network-uri`, `scientific`, `semialign`, `strict`, `text`, `text-iso8601`, `text-short`, `th-abstraction`, `these`, `time-compat`, `unordered-containers`, `uuid-types`, `vector`, `witherable` |
| zlib | 0.7.1.1 | ❌ fails (unbound type name ‘ST’) |
| streaming-commons | 0.2.3.1 | ⏸️ needs `async`, `directory`, `network`, `process`, `text`, `unix`, `zlib` |
| http-client | 0.7.19 | ⏸️ needs `async`, `blaze-builder`, `case-insensitive`, `containers`, `cookie`, `http-types`, `iproute`, `mime-types`, `network`, `network-uri`, `streaming-commons`, `text` |
| tls | 2.2.2 | ⏸️ needs `cereal`, `crypton`, `crypton-asn1-encoding`, `crypton-asn1-types`, `crypton-x509`, `crypton-x509-store`, `crypton-x509-validation`, `data-default`, `hpke`, `memory`, `network`, `serialise`, `unix-time`, `zlib` |
| crypton-connection | 0.4.5 | ⏸️ needs `containers`, `crypton-socks`, `crypton-x509-store`, `crypton-x509-system`, `data-default`, `network`, `tls` |
| http-client-tls | 0.3.6.4 | ⏸️ needs `case-insensitive`, `containers`, `crypton`, `crypton-connection`, `data-default`, `http-client`, `http-types`, `memory`, `network`, `network-uri`, `text`, `tls` |
| aihc-hackage | 0.1.0.0 | ⏸️ needs `Cabal`, `Cabal-syntax`, `containers`, `directory`, `http-client`, `http-client-tls`, `http-types`, `tar`, `text`, `zlib` |
| aihc-package-plan | 0.1.0.0 | ⏸️ needs `Cabal-syntax`, `aeson`, `aihc-cpp`, `aihc-hackage`, `aihc-parser`, `containers`, `directory`, `text` |
| aihc | 0.1.0.0 | ⏸️ needs `Cabal-syntax`, `aeson`, `aihc-cpp`, `aihc-hackage`, `aihc-package-plan`, `aihc-parser`, `aihc-resolve`, `aihc-tc`, `async`, `binary`, `containers`, `directory`, `haskeline`, `libffi`, `megaparsec`, `optparse-applicative`, `prettyprinter`, `process`, `text`, `unix`, `vector` |

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
