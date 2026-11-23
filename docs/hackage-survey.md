# Hackage survey

This file tells which Hackage packages compile with GHC and with aihc.
`scripts/hackage-survey.sh` adds packages to the two tables below.
On each run, the script takes the most downloaded Hackage packages that are
not in the tables. For each new package, the script compiles the latest
release with `cabal build` and GHC. If GHC compiles it, the script compiles
the same release with `aihc install NAME-VERSION --lint`.

Run the script with `nix run .#hackage-survey`. Give the names of packages
as arguments to survey those packages instead of the download list.
The script does not compile a package that is in a table again. To compile
a package again, remove its row.

## Summary

- Packages that compile with GHC: 535
- Packages that compile with aihc: 449
- Packages that do not compile with GHC: 398

## Packages that compile with GHC

| Package | Version | AIHC | Error |
| ------- | ------- | ---- | ----- |
| BNFC | 2.9.6.3 | pass |  |
| Cabal | 3.18.2.0 | pass |  |
| Cabal-syntax | 3.18.2.0 | pass |  |
| Chart-diagrams | 1.9.5.1 | fail | timed out after 900 seconds |
| ChasingBottoms | 1.3.1.17 | pass |  |
| Diff | 2.0.1 | pass |  |
| GLFW-b | 3.3.9.1 | pass |  |
| Glob | 0.10.2 | pass |  |
| HDBC | 2.4.0.5 | pass |  |
| HDBC-odbc | 2.6.0.0 | pass |  |
| HPDF | 1.8 | pass |  |
| HStringTemplate | 0.8.8 | pass |  |
| HTTP | 4000.5.0 | pass |  |
| HUnit | 1.6.2.0 | pass |  |
| HaTeX | 3.23.0.2 | pass |  |
| HaXml | 1.25.14 | pass |  |
| HsOpenSSL | 0.11.7.11 | pass |  |
| JuicyPixels | 3.3.9 | pass |  |
| ListLike | 4.7.8.4 | pass |  |
| MissingH | 1.6.0.3 | pass |  |
| MonadRandom | 0.6.2.1 | pass |  |
| OneTuple | 0.4.3 | pass |  |
| OpenGLRaw | 3.3.4.1 | pass |  |
| QuickCheck | 2.19.0.0 | pass |  |
| RSA | 2.4.1 | pass |  |
| Rasterific | 0.7.5.4 | pass |  |
| SHA | 1.6.4.4 | pass |  |
| StateVar | 1.2.2 | pass |  |
| TCache | 1.0.0.1 | pass |  |
| Yampa | 0.15.1 | pass |  |
| acid-state | 0.17.0 | pass |  |
| active | 0.2.1.1 | pass |  |
| ad | 4.5.6 | pass |  |
| adjunctions | 4.4.4 | pass |  |
| aeson | 2.3.2.0 | pass |  |
| aeson-pretty | 0.8.11 | pass |  |
| aeson-value-parser | 0.19.7.2 | pass |  |
| aivika | 6.2 | pass |  |
| alex | 3.5.4.2 | pass |  |
| amqp | 0.24.0 | pass |  |
| ansi-terminal | 1.1.5 | pass |  |
| ansi-terminal-game | 1.9.4.0 | pass |  |
| ansi-wl-pprint | 1.1.1 | pass |  |
| apecs | 0.10.1 | fail | apecs-0.10.1/src/Apecs/Experimental/Reactive.hs:77:26: error: unsolved constraint Has w m (Elem s) |
| arbtt | 0.12.0.3 | pass |  |
| arch-hs | 0.16.2 | fail | arch-hs-0.16.2/.aihc-target/llvm/splices/polysemy/Polysemy.Final/package/src/Polysemy/Internal/Strategy.hs:60:38: error: unsolved constraint Coercible (Strategy |
| arithmoi | 0.13.4.0 | pass |  |
| async | 2.2.6 | pass |  |
| atomic-primops | 0.8.8 | pass |  |
| attoparsec | 0.14.4 | pass |  |
| authenticate | 1.3.5.2 | pass |  |
| auto-update | 0.2.8 | pass |  |
| aws | 0.25.3 | pass |  |
| barbies | 2.1.1.0 | pass |  |
| base-compat | 0.15.0 | pass |  |
| base-compat-batteries | 0.15.0 | pass |  |
| base-orphans | 0.9.4 | pass |  |
| base-prelude | 1.6.1.1 | pass |  |
| base16 | 2.0 | pass |  |
| base64 | 2.1 | pass |  |
| base64-bytestring | 1.2.1.0 | pass |  |
| basement | 0.0.16 | pass |  |
| beam-postgres | 0.6.4.0 | fail | pqueue-1.7.0.0/src/Data/PQueue/Min.hs:32:3: error: pattern synonym Empty has the scrutinee type Seq and cannot be bundled with MinQueue |
| bearriver | 0.15.1 | fail | dunai-0.14.2/src/Data/MonadicStreamFunction/Util.hs:41:17: error: unhandled syntax: term name ‘EProc’ |
| bifunctors | 5.6.3 | pass |  |
| binary | 0.8.9.3 | pass |  |
| bitvec | 1.1.6.0 | pass |  |
| blaze-builder | 0.4.4.1 | pass |  |
| blaze-html | 0.9.2.0 | pass |  |
| blaze-markup | 0.8.3.0 | pass |  |
| bloodhound | 1.0.0.0 | pass |  |
| bluefin | 0.10.1.0 | pass |  |
| bound | 2.0.7 | pass |  |
| brick | 3.0 | fail | brick-3.0/.aihc-target/llvm/splices/brick/Brick.Widgets.List/package/src/Brick/Main.hs:81:8: error: module ‘Graphics.Vty.CrossPlatform’ not found |
| bson | 0.4.0.1 | pass |  |
| bv-sized | 1.0.8 | fail | parameterized-utils-2.3.1.0/src/Data/Parameterized/NatRepr/Internal.hs:71:25: error: unsolved constraint 0 ~ + t0 1 |
| bytes | 0.17.5 | pass |  |
| byteslice | 0.2.15.0 | pass |  |
| bytestring | 0.12.2.0 | pass |  |
| c2hs | 0.28.8 | pass |  |
| cabal-debian | 5.5.1 | fail | timed out after 900 seconds |
| cabal-doctest | 1.0.13 | pass |  |
| cabal-gild | 1.8.4.2 | pass |  |
| cabal-rpm | 2.3.2 | pass |  |
| cabal2nix | 2.21.3 | fail | hpack-0.39.6/src/Hpack/Syntax/Defaults.hs:29:22: error: unsolved constraint M1 D ('MetaData "ParseGithub" "Hpack.Syntax.Defaults" "hpack-0.39.6" 'False) (M1 C ( |
| cachix-api | 1.14.0 | fail | servant-0.20.3.0/src/Servant/API/ResponseHeaders.hs:81:29: error: unsolved constraint NFData (ResponseHeader h x) |
| call-alloy | 0.6.0.3 | pass |  |
| call-stack | 0.4.0 | pass |  |
| case-insensitive | 1.2.2.0 | pass |  |
| cassava | 0.5.4.1 | pass |  |
| cereal | 0.5.8.3 | pass |  |
| cgi | 3001.5.2.0 | pass |  |
| cgrep | 9.2.3 | pass |  |
| charset | 0.3.12 | pass |  |
| chart-svg | 0.8.3.3 | fail | In Color-0.4.1: FC lint failed: |
| chronos | 1.1.7.0 | pass |  |
| citeproc | 0.14.2 | pass |  |
| classy-prelude | 1.5.0.3 | pass |  |
| classy-prelude-conduit | 1.5.0 | pass |  |
| clientsession | 0.9.3.0 | pass |  |
| clock | 0.8.4 | pass |  |
| cmark | 0.6.1 | pass |  |
| cmdargs | 0.10.22 | pass |  |
| cndict | 0.10.0 | pass |  |
| cobot-io | 0.1.5.8 | fail | timed out after 900 seconds |
| colour | 2.3.7 | pass |  |
| commonmark | 0.3.2 | pass |  |
| commonmark-extensions | 0.2.8 | pass |  |
| comonad | 5.0.10 | pass |  |
| comonad-transformers | 4.0 | pass |  |
| comonads-fd | 4.0 | pass |  |
| composition-prelude | 3.0.1.0 | pass |  |
| concurrent-output | 1.10.21 | pass |  |
| conduit | 1.3.6.1 | pass |  |
| conduit-extra | 1.3.8 | pass |  |
| constraints | 0.14.4 | pass |  |
| constraints-extras | 0.4.0.2 | pass |  |
| containers | 0.8.1 | pass |  |
| contravariant | 1.5.6 | pass |  |
| cookie | 0.5.1 | pass |  |
| copilot | 4.8.2 | fail | fp-ieee-0.1.0.7/src/Numeric/Floating/IEEE/Internal/Classify.hs:6:36: error: not exported: term name ‘isDoubleFinite’ |
| copilot-c99 | 4.8.2 | pass |  |
| copilot-core | 4.8.2 | pass |  |
| copilot-language | 4.8.2 | fail | fp-ieee-0.1.0.7/src/Numeric/Floating/IEEE/Internal/Classify.hs:6:36: error: not exported: term name ‘isDoubleFinite’ |
| copilot-libraries | 4.8.2 | fail | fp-ieee-0.1.0.7/src/Numeric/Floating/IEEE/Internal/Classify.hs:6:36: error: not exported: term name ‘isDoubleFinite’ |
| copilot-theorem | 4.8.2 | fail | fp-ieee-0.1.0.7/src/Numeric/Floating/IEEE/Internal/Classify.hs:6:36: error: not exported: term name ‘isDoubleFinite’ |
| core-program | 0.7.0.0 | fail | network-info-0.2.1/cbits/network-unix.c:14:13: fatal error: 'net/if.h' file not found |
| cpphs | 1.20.10 | pass |  |
| cql-io | 2.0.0 | fail | network-info-0.2.1/cbits/network-unix.c:14:13: fatal error: 'net/if.h' file not found |
| criterion | 1.6.5.0 | pass |  |
| crypto-api | 0.13.3 | pass |  |
| cryptohash | 0.11.9 | pass |  |
| crypton | 2.2.6 | pass |  |
| crypton-x509 | 1.10.1 | pass |  |
| crypton-x509-validation | 1.10.1 | pass |  |
| cryptonite | 0.30 | pass |  |
| csound-expression | 5.4.4.1 | fail | csound-expression-typed-0.2.9.0/src/Csound/Typed/GlobalState/Opcodes.hs:91:1: error: kind mismatch: expected Type → Type, got Type |
| csound-expression-typed | 0.2.9.0 | fail | csound-expression-typed-0.2.9.0/src/Csound/Typed/GlobalState/Opcodes.hs:91:1: error: kind mismatch: expected Type → Type, got Type |
| data-accessor | 0.2.3.1 | pass |  |
| data-default | 0.8.0.2 | pass |  |
| data-default-class | 0.2.0.0 | pass |  |
| data-fix | 0.3.4 | pass |  |
| dataframe | 3.8.0.0 | fail | The plan needs build tools this compiler cannot run: |
| dbus | 1.4.3 | fail | timed out after 900 seconds |
| debian | 4.1.2 | fail | timed out after 900 seconds |
| deepseq | 1.5.2.0 | pass |  |
| deferred-folds | 0.9.18.10 | pass |  |
| dejafu | 2.4.0.8 | pass |  |
| dependent-sum | 0.7.2.0 | pass |  |
| deriving-compat | 0.6.8 | pass |  |
| dhall | 1.42.3 | pass |  |
| diagrams-contrib | 1.4.8 | fail | timed out after 900 seconds |
| diagrams-core | 1.5.1.2 | pass |  |
| diagrams-lib | 1.6 | fail | timed out after 900 seconds |
| diagrams-rasterific | 1.5.0.1 | fail | timed out after 900 seconds |
| diagrams-svg | 1.5.1 | fail | timed out after 900 seconds |
| digestive-functors | 0.8.4.2 | pass |  |
| digit | 0.15 | pass |  |
| dimensional | 1.6.2 | pass |  |
| direct-sqlite | 2.3.29 | pass |  |
| directory | 1.3.11.0 | pass |  |
| discord-haskell | 1.19.0 | pass |  |
| distributed-process | 0.7.10 | pass |  |
| distributive | 0.6.3 | pass |  |
| dlist | 1.0 | pass |  |
| dns | 4.2.1 | pass |  |
| dobutokO2 | 0.45.3.0 | pass |  |
| doctemplates | 0.11.1 | pass |  |
| dotenv | 0.12.0.0 | pass |  |
| double-conversion | 2.0.5.0 | pass |  |
| either | 5.0.3 | pass |  |
| email-validate | 2.3.2.21 | pass |  |
| entropy | 0.4.1.11 | pass |  |
| errors | 2.3.0 | pass |  |
| ersatz | 0.6 | pass |  |
| esqueleto | 3.6.0.3 | pass |  |
| exceptions | 0.10.12 | pass |  |
| extra | 1.8.1 | pass |  |
| fast-logger | 3.2.9 | pass |  |
| fb | 2.1.1.2 | pass |  |
| fgl | 5.8.3.1 | pass |  |
| file-embed | 0.0.16.0 | pass |  |
| filepath | 1.5.5.0 | pass |  |
| filestore | 0.6.5.1 | pass |  |
| fingertree | 0.1.6.3 | pass |  |
| fixed-vector | 2.1.1.0 | pass |  |
| flatparse | 0.5.6.1 | pass |  |
| foldl | 1.4.18 | pass |  |
| force-layout | 0.4.1 | pass |  |
| formatting | 7.2.0 | pass |  |
| foundation | 0.0.30 | pass |  |
| free | 5.2 | pass |  |
| fused-effects | 1.1.2.7 | pass |  |
| generic-deriving | 1.14.7 | pass |  |
| generics-sop | 0.5.1.4 | pass |  |
| genvalidity | 1.1.1.0 | pass |  |
| ghc-events | 0.21.0.0 | pass |  |
| ghcid | 1.0.0 | pass |  |
| ghcjs-dom | 0.10.0.0 | pass |  |
| ginger | 0.10.6.0 | pass |  |
| github | 0.30.0.2 | pass |  |
| github-release | 2.0.0.19 | pass |  |
| gloss | 1.13.2.2 | pass |  |
| gnuplot | 0.5.7 | pass |  |
| graphql | 1.5.0.3 | pass |  |
| graphviz | 2999.20.2.1 | pass |  |
| h-raylib | 6.1.0.0 | fail | timed out after 900 seconds |
| hackage-security | 0.6.4.0 | fail | hackage-security-0.6.4.0/src/Hackage/Security/Trusted.hs:40:31: error: unbound term name ‘static’ |
| haddock-library | 1.11.0 | pass |  |
| hakyll | 4.17.0.0 | fail | In text-builder-linear-0.1.4: Template Haskell failed in module Data.Text.Builder.Linear.Dec.Bounded: splice 0: the engine failed: unreachable) |
| half | 0.3.3 | pass |  |
| hamlet | 1.2.0 | pass |  |
| happstack-server | 7.9.5 | pass |  |
| happstack-server-tls | 7.2.1.7 | pass |  |
| happy | 2.2 | pass |  |
| happy-lib | 2.2 | pass |  |
| hashable | 1.5.1.0 | pass |  |
| hashtables | 1.4.2 | pass |  |
| haskeline | 0.8.5.0 | pass |  |
| haskell-src-exts | 1.24.0 | pass |  |
| haskell-src-meta | 0.8.16 | pass |  |
| haskoin-core | 2.0.0 | fail | haskoin-core-2.0.0/src/Haskoin/Block/Common.hs:105:33: error: could not match Headers with t0 → Encoding' Value |
| haskoin-store-data | 3.0.0 | fail | haskoin-core-2.0.1/src/Haskoin/Block/Common.hs:105:33: error: could not match Headers with t0 → Encoding' Value |
| hasql | 2.0.1.0 | pass |  |
| hasql-optparse-applicative | 0.9.1.1 | pass |  |
| hasql-pool | 1.5.1.1 | pass |  |
| hasql-transaction | 1.2.3.1 | pass |  |
| haxr | 3000.11.6.1 | pass |  |
| hedgehog | 1.7 | pass |  |
| hedis | 0.16.4 | pass |  |
| heist | 1.1.1.2 | pass |  |
| hexpat | 0.20.13 | pass |  |
| histogram-fill | 0.9.1.0 | pass |  |
| hjsmin | 0.2.1 | pass |  |
| hledger | 1.52.4 | fail | In text-builder-linear-0.1.4: Template Haskell failed in module Data.Text.Builder.Linear.Dec.Bounded: splice 0: the engine failed: unreachable) |
| hledger-ui | 1.52.4 | fail | In text-builder-linear-0.1.4: Template Haskell failed in module Data.Text.Builder.Linear.Dec.Bounded: splice 0: the engine failed: unreachable) |
| hledger-web | 1.52.4 | fail | In text-builder-linear-0.1.4: Template Haskell failed in module Data.Text.Builder.Linear.Dec.Bounded: splice 0: the engine failed: unreachable) |
| hmatrix | 0.20.2 | fail | built  semigroups-0.20.1 in 0.4 s |
| hmatrix-gsl-stats | 0.4.1.8 | fail | built  semigroups-0.20.1 in 0.5 s |
| hoauth2 | 2.15.2 | pass |  |
| hpack | 0.39.6 | fail | hpack-0.39.6/src/Hpack/Syntax/Defaults.hs:29:22: error: unsolved constraint M1 D ('MetaData "ParseGithub" "Hpack.Syntax.Defaults" "hpack-0.39.6" 'False) (M1 C ( |
| hsc2hs | 0.68.10 | pass |  |
| hslogger | 1.3.2.0 | pass |  |
| hslua | 2.5.0 | pass |  |
| hspec | 2.11.18 | pass |  |
| hspec-core | 2.11.18 | pass |  |
| hspec-discover | 2.11.18 | pass |  |
| hspec-expectations | 0.8.4 | pass |  |
| hspec-meta | 2.11.18 | pass |  |
| http-api-data | 0.7 | pass |  |
| http-client | 0.7.19 | pass |  |
| http-client-tls | 0.4.0 | pass |  |
| http-conduit | 2.3.9.1 | pass |  |
| http-conduit-downloader | 1.1.5 | pass |  |
| http-date | 0.0.13 | pass |  |
| http-media | 0.8.1.1 | pass |  |
| http-reverse-proxy | 0.6.2.0 | pass |  |
| http-streams | 0.8.9.9 | pass |  |
| http-types | 0.12.6 | pass |  |
| http2 | 5.4.13 | pass |  |
| http2-tls | 0.5.9 | pass |  |
| http3 | 0.1.11 | pass |  |
| hvega | 0.12.0.7 | pass |  |
| hw-kafka-client | 5.3.0 | pass |  |
| hw-prim | 0.6.3.2 | pass |  |
| hxt | 9.3.1.22 | pass |  |
| hyphenation | 0.8.3 | pass |  |
| indexed-traversable | 0.1.5 | pass |  |
| indexed-traversable-instances | 0.1.2.1 | pass |  |
| inline-c | 0.9.1.10 | pass |  |
| insert-ordered-containers | 0.3.0.1 | pass |  |
| integer-logarithms | 1.0.5 | pass |  |
| invariant | 0.6.5 | pass |  |
| io-streams | 1.5.2.2 | pass |  |
| ip | 1.7.8 | pass |  |
| iproute | 1.8.0 | pass |  |
| jose | 0.13 | fail | <unknown location in Crypto.JOSE.JWA.JWS>: error: could not match Value with [Char] |
| jose-jwt | 0.11.0 | pass |  |
| jsaddle | 0.9.10.0 | pass |  |
| kan-extensions | 5.2.8 | pass |  |
| katip | 0.8.8.4 | pass |  |
| keiro | 0.25.0.0 | fail | In streamly-core-0.3.3: FC lint failed: |
| keiro-core | 0.25.0.0 | fail | In streamly-core-0.3.3: FC lint failed: |
| keiro-dsl | 0.25.0.0 | fail | In streamly-core-0.3.3: FC lint failed: |
| keiro-pgmq | 0.25.0.0 | fail | In streamly-core-0.3.3: FC lint failed: |
| keter | 2.3.6 | fail | basement-0.0.16/Basement/Types/OffsetSize.hs:231:33: error: could not match Word32 with Word64 |
| keys | 3.12.5 | pass |  |
| kiroku-otel | 0.3.0.1 | fail | In streamly-core-0.3.3: FC lint failed: |
| kiroku-store | 0.13.0.2 | fail | In streamly-core-0.3.3: FC lint failed: |
| language-c | 0.10.2 | pass |  |
| language-c-quote | 0.13.0.2 | pass |  |
| language-docker | 16.0.0 | pass |  |
| language-javascript | 0.7.1.0 | pass |  |
| lens | 5.3.6 | pass |  |
| lens-aeson | 1.2.3 | pass |  |
| lens-family-core | 2.1.3 | pass |  |
| lentil | 1.5.10.0 | pass |  |
| libarchive | 3.0.6.0 | fail | The plan needs build tools this compiler cannot run: |
| lifted-async | 0.11.0 | pass |  |
| lifted-base | 0.2.3.12 | pass |  |
| linear | 1.23.3 | pass |  |
| liquid-fixpoint | 0.9.6.3.7 | fail | liquid-fixpoint-0.9.6.3.7/.aihc-target/llvm/splices/liquid-fixpoint/Language.Fixpoint.Types.Config/package/src/AihcSplice.hs:7:18: error: module ‘Data.Store’ no |
| list-t | 1.0.5.7 | pass |  |
| logict | 0.8.2.0 | pass |  |
| lsp-test | 0.18.0.0 | fail | row-types-1.0.1.2/src/Data/Row/Dictionaries.hs:106:1: error: kind mismatch: expected Row k0, got k0 |
| lucid | 2.11.20260427 | pass |  |
| massiv | 1.0.5.0 | pass |  |
| math-functions | 0.3.4.4 | pass |  |
| megaparsec | 9.8.3 | pass |  |
| memory | 0.18.0 | pass |  |
| microlens | 0.5.0.0 | pass |  |
| microlens-ghc | 0.4.15.2 | pass |  |
| microlens-mtl | 0.2.1.1 | pass |  |
| microlens-platform | 0.4.4.2 | pass |  |
| microlens-th | 0.4.3.18 | pass |  |
| mighttpd2 | 4.0.12 | pass |  |
| mime-mail | 0.5.2 | pass |  |
| mime-types | 0.1.2.2 | pass |  |
| miso | 1.15.0.0 | pass |  |
| mmorph | 1.2.2 | pass |  |
| modern-uri | 0.3.6.1 | pass |  |
| monad-control | 1.0.3.1 | pass |  |
| monad-logger | 0.3.42 | pass |  |
| mongoDB | 2.7.1.4 | pass |  |
| mono-traversable | 1.0.21.0 | pass |  |
| monoid-extras | 0.7.0.2 | pass |  |
| monoid-subclasses | 1.2.6.1 | pass |  |
| monoidal-containers | 0.6.8.0 | pass |  |
| morpheus-graphql | 0.28.5 | fail | timed out after 900 seconds |
| mtl | 2.3.2 | pass |  |
| mwc-random | 0.15.3.0 | pass |  |
| mysql-haskell | 1.5.0 | pass |  |
| network | 3.2.12.0 | pass |  |
| network-byte-order | 0.1.9 | pass |  |
| network-uri | 2.6.4.2 | pass |  |
| ngx-export | 1.7.11.1 | pass |  |
| ngx-export-tools | 1.2.6.3 | pass |  |
| numhask | 0.13.3.0 | pass |  |
| old-time | 1.1.1.0 | pass |  |
| opaleye | 0.10.8.0 | fail | In opaleye-0.10.8.0: FC generation failed: Opaleye.Internal.TypeFamilies: body: cannot infer the invisible kind argument Unique 419699 for K with arguments [TcT |
| optparse-applicative | 0.19.0.0 | pass |  |
| optparse-generic | 1.5.3 | pass |  |
| os-string | 2.0.11 | pass |  |
| pandoc | 3.14 | fail | In text-builder-linear-0.1.4: Template Haskell failed in module Data.Text.Builder.Linear.Dec.Bounded: splice 0: the engine failed: unreachable) |
| pandoc-crossref | 0.3.26 | fail | timed out after 900 seconds |
| pandoc-types | 1.23.1.2 | pass |  |
| parallel | 3.3.0.0 | pass |  |
| parsec | 3.1.18.0 | pass |  |
| parser-combinators | 1.3.1 | pass |  |
| parsers | 0.12.12 | pass |  |
| patat | 0.15.2.0 | fail | /private/tmp/claude-501/-Users-lemmih-coding-ai-haskell-compiler-aihc--claude-worktrees-cool-cartwright-9e650f/ccd7b847-4a56-4111-8600-07df72506c9d/scratchpad/s |
| path | 0.9.6 | fail | path-0.9.6/.aihc-target/llvm/splices/path/OsPath.Windows/package/src/OsPath/Internal/Include.hs:55:59: error: not exported: type name ‘PLATFORM_CHAR’ |
| path-io | 1.8.2 | fail | path-io-1.8.2/.aihc-target/llvm/splices/path/OsPath.Windows/package/src/OsPath/Internal/Include.hs:55:59: error: not exported: type name ‘PLATFORM_CHAR’ |
| persistent | 2.18.1.0 | pass |  |
| persistent-mongoDB | 2.13.1.0 | pass |  |
| persistent-postgresql | 2.14.3.0 | fail | configure: error: Library requirements (PostgreSQL) not met. |
| persistent-sqlite | 2.13.3.1 | fail | In persistent-sqlite-2.13.3.1: Template Haskell failed in module Database.Persist.Sqlite: the engine failed: the shared module refers to the unknown symbol sqli |
| persistent-template | 2.12.0.0 | pass |  |
| phino | 0.0.165 | fail | In phino-0.0.165: GRIN generation failed: value Name {nameText = "$fEqGLOBAL$c==", nameSort = SortValue, nameOrigin = OriginTop (PackageId {packageIdText = "phi |
| pipes | 4.3.16 | pass |  |
| pointed | 5.0.5 | pass |  |
| polysemy | 1.9.2.0 | fail | polysemy-1.9.2.0/.aihc-target/llvm/splices/polysemy/Polysemy.Final/package/src/Polysemy/Internal/Strategy.hs:60:38: error: unsolved constraint Coercible (Strate |
| pontarius-xmpp | 0.5.7.2 | fail | pontarius-xmpp-0.5.7.2/source/Network/Xmpp/Tls.hs:125:1: error: could not match CertificateStore with CertificateStore |
| postgresql-binary | 0.15.0.1 | pass |  |
| postgresql-libpq | 0.11.0.0 | pass |  |
| postgresql-query | 3.10.0 | fail | configure: error: Library requirements (PostgreSQL) not met. |
| postgresql-simple | 0.7.0.1 | pass |  |
| pqi-conformance | 1.0.18.0 | pass |  |
| pretty-show | 1.10 | pass |  |
| pretty-simple | 4.1.4.0 | pass |  |
| prettyprinter | 1.7.2 | pass |  |
| prettyprinter-ansi-terminal | 1.1.4 | pass |  |
| primitive | 0.9.1.0 | pass |  |
| primitive-extras | 0.10.2.2 | pass |  |
| process | 1.6.30.0 | pass |  |
| profunctors | 5.6.3 | pass |  |
| propellor | 5.18 | fail | In propellor-5.18: FC lint failed: |
| proto-lens | 0.7.1.7 | pass |  |
| protolude | 0.3.5 | pass |  |
| psqueues | 0.2.8.3 | pass |  |
| pusher-http-haskell | 2.1.0.27 | pass |  |
| quic | 0.3.22 | pass |  |
| quickcheck-instances | 0.4 | pass |  |
| random | 1.3.1 | pass |  |
| rank2classes | 1.5.5.1 | pass |  |
| ratel | 2.0.0.19 | pass |  |
| rattletrap | 14.1.4 | pass |  |
| reactive-banana | 1.3.2.0 | pass |  |
| rebase | 1.23 | pass |  |
| recursion-schemes | 5.2.3 | pass |  |
| reducers | 3.12.5 | pass |  |
| reflection | 2.1.9 | pass |  |
| reflex | 0.9.4.1 | fail | reflex-0.9.4.1/src/Reflex/Profiled.hs:71:34: error: unbound type name ‘CostCentreStack’ |
| regex-base | 0.94.0.3 | pass |  |
| regex-posix | 0.96.0.2 | pass |  |
| regex-tdfa | 1.3.2.7 | pass |  |
| registry | 0.6.3.3 | pass |  |
| relational-query | 0.12.3.1 | pass |  |
| resource-pool | 0.5.1.0 | pass |  |
| resourcet | 1.3.0 | pass |  |
| retry | 0.9.3.1 | pass |  |
| rio | 0.1.25.0 | pass |  |
| safe | 0.3.21 | pass |  |
| safe-exceptions | 0.1.7.4 | pass |  |
| sbp | 6.5.2 | pass |  |
| sbv | 14.9 | fail | sbv-14.9/.aihc-target/llvm/splices/sbv/Data.SBV.Maybe/package/src/Data/SBV/List.hs:564:1: error: could not match SBV [a] with [SBV a] |
| scientific | 0.3.10.0 | pass |  |
| scotty | 0.30 | pass |  |
| secp256k1-haskell | 3.0.0 | pass |  |
| semialign | 1.4 | pass |  |
| semigroupoids | 6.0.2 | pass |  |
| semigroups | 0.20.1 | pass |  |
| servant | 0.20.3.0 | fail | servant-0.20.3.0/src/Servant/API/ResponseHeaders.hs:81:29: error: unsolved constraint NFData (ResponseHeader h x) |
| servant-client | 0.20.3.0 | fail | In servant-client-core-0.20.3.0: FC lint failed: |
| servant-docs | 0.13.1 | fail | servant-0.20.3.0/src/Servant/API/ResponseHeaders.hs:81:29: error: unsolved constraint NFData (ResponseHeader h x) |
| servant-lucid | 0.9.0.6 | fail | servant-0.20.3.0/src/Servant/API/ResponseHeaders.hs:81:29: error: unsolved constraint NFData (ResponseHeader h x) |
| servant-server | 0.20.3.0 | fail | In servant-server-0.20.3.0: FC lint failed: |
| servant-swagger | 1.2.3 | fail | servant-0.20.3.0/src/Servant/API/ResponseHeaders.hs:81:29: error: unsolved constraint NFData (ResponseHeader h x) |
| servant-swagger-ui | 0.3.5.5.0.1 | fail | In servant-server-0.20.3.0: FC lint failed: |
| shake | 0.19.9 | pass |  |
| shakespeare | 2.2.0 | pass |  |
| shakespeare-css | 1.1.0 | pass |  |
| shakespeare-js | 1.3.0 | pass |  |
| shelly | 1.12.1.1 | pass |  |
| shibuya-kiroku-adapter | 0.8.0.0 | fail | In streamly-core-0.3.3: FC lint failed: |
| simple-sendfile | 0.2.33 | pass |  |
| singletons | 3.0.4 | fail | singletons-3.0.4/src/Data/Singletons.hs:806:1: error: could not match SLambda with SWrappedSing |
| skylighting | 0.15.0.1 | pass |  |
| skylighting-core | 0.15.0.1 | pass |  |
| snap | 1.1.3.3 | pass |  |
| snap-core | 1.0.5.1 | pass |  |
| snap-server | 1.1.2.1 | pass |  |
| some | 1.1 | pass |  |
| split | 0.2.5.1 | pass |  |
| splitmix | 0.1.3.2 | pass |  |
| sqlite-simple | 0.4.19.0 | pass |  |
| statistics | 0.16.5.0 | pass |  |
| stm | 2.5.3.1 | pass |  |
| stm-containers | 1.2.2 | pass |  |
| store | 0.7.22 | fail | <unknown location in Data.Store.Internal>: error: kind mismatch: expected 'BoxedRep 'Lifted, got 'WordRep |
| stratosphere | 1.0.1 | pass |  |
| streaming | 0.2.4.0 | pass |  |
| streaming-commons | 0.2.3.1 | pass |  |
| streamly | 0.11.1 | fail | In streamly-core-0.3.3: FC lint failed: |
| strict | 0.5.1 | pass |  |
| strive | 6.1.0.5 | pass |  |
| swagger2 | 2.9.1 | fail | timed out after 900 seconds |
| syb | 0.7.4 | pass |  |
| system-fileio | 0.3.16.7 | pass |  |
| system-filepath | 0.4.14.1 | pass |  |
| tagged | 0.9 | pass |  |
| tagsoup | 0.14.8 | pass |  |
| tar | 0.7.2.0 | pass |  |
| tasty | 1.5.5 | pass |  |
| tasty-golden | 2.3.6 | pass |  |
| tasty-quickcheck | 0.11.1 | pass |  |
| terminfo | 0.4.1.7 | pass |  |
| test-framework | 0.8.2.3 | pass |  |
| texmath | 0.13.3.5 | pass |  |
| text | 2.1.4 | pass |  |
| text-builder | 1.0.0.5 | pass |  |
| text-icu | 0.8.0.5 | pass |  |
| text-iso8601 | 0.2.0.0 | pass |  |
| text-short | 0.1.6.1 | pass |  |
| text-show | 3.11.4 | fail | src/TextShow/Foreign/Ptr.hs:49:7: error: unbound term name ‘integerFromWord#’ |
| th-abstraction | 0.7.2.0 | pass |  |
| th-compat | 0.1.7 | pass |  |
| th-desugar | 1.19 | fail | th-desugar-1.19/Language/Haskell/TH/Desugar/Util.hs:60:22: error: not exported: type name ‘IP’ |
| th-expand-syns | 0.4.12.0 | pass |  |
| th-lift | 0.8.7 | pass |  |
| th-lift-instances | 0.1.20 | pass |  |
| th-orphans | 0.13.17 | pass |  |
| these | 1.2.1 | pass |  |
| tidal | 1.10.3 | pass |  |
| time | 1.16.0.1 | pass |  |
| time-compat | 1.9.9 | pass |  |
| time-manager | 0.5.2 | pass |  |
| tls | 2.5.1 | pass |  |
| tmp-postgres | 1.34.1.0 | pass |  |
| transformers | 0.6.3.0 | pass |  |
| transformers-base | 0.4.6.1 | pass |  |
| transformers-compat | 0.8 | pass |  |
| tree-diff | 0.4.1 | pass |  |
| trifecta | 2.1.4 | pass |  |
| turtle | 1.6.2 | pass |  |
| typed-process | 0.2.13.0 | pass |  |
| typst | 0.12.1 | fail | In text-builder-linear-0.1.4: Template Haskell failed in module Data.Text.Builder.Linear.Dec.Bounded: splice 0: the engine failed: unreachable) |
| tzdata | 0.2.20260911.0 | pass |  |
| uniplate | 1.6.13 | pass |  |
| unix | 2.8.8.0 | pass |  |
| unix-compat | 0.7.4.1 | pass |  |
| unix-time | 0.6.0 | pass |  |
| unliftio | 0.2.25.1 | pass |  |
| unordered-containers | 0.2.21 | pass |  |
| usb | 1.3.0.6 | pass |  |
| utf8-string | 1.0.2 | pass |  |
| uu-parsinglib | 2.9.2 | pass |  |
| uuagc | 0.9.57 | fail | src-ag/Order.ag:20:8: error: module ‘SequentialComputation’ not found |
| uuid | 1.3.16.1 | pass |  |
| uuid-types | 1.0.6.1 | pass |  |
| validity | 0.12.1.0 | pass |  |
| vault | 0.3.2.0 | pass |  |
| vector | 0.13.2.0 | pass |  |
| vector-algorithms | 0.9.1.0 | pass |  |
| vector-sized | 1.6.1 | pass |  |
| vector-space | 0.19 | pass |  |
| versions | 6.0.9 | pass |  |
| vinyl | 0.14.3 | pass |  |
| void | 0.7.4 | pass |  |
| vty | 6.7 | pass |  |
| wai | 3.2.5 | pass |  |
| wai-app-file-cgi | 3.3.0 | pass |  |
| wai-app-static | 3.2.1 | pass |  |
| wai-extra | 3.1.18 | pass |  |
| wai-logger | 2.5.1 | pass |  |
| wai-test | 3.0.0 | pass |  |
| warp | 3.4.21 | pass |  |
| warp-tls | 3.4.18 | pass |  |
| web-routes | 0.27.16 | pass |  |
| webdriver | 0.15.0.0 | fail | In webdriver-0.15.0.0: Template Haskell failed in module Test.WebDriver.Capabilities.FirefoxOptions: the wasm32-wasip3 target has no C++ standard library: its s |
| websockets | 0.13.0.0 | pass |  |
| wide-word | 0.1.9.0 | pass |  |
| witch | 1.4.0.1 | pass |  |
| witherable | 0.5 | pass |  |
| x509 | 1.7.7 | pass |  |
| x509-validation | 1.6.12 | pass |  |
| xlsx | 1.3.0 | fail | timed out after 900 seconds |
| xml-conduit | 1.10.1.0 | pass |  |
| xmlhtml | 0.2.5.4 | pass |  |
| yaml | 0.11.11.2 | pass |  |
| yesod-test | 1.7.0.3 | fail | basement-0.0.16/Basement/Types/OffsetSize.hs:231:33: error: could not match Word32 with Word64 |
| yi | 0.19.3 | fail | src/Yi/Lexer/common.hsinc:24:34: error: could not match Int with [(Point, Char)] → () → (a, b) |
| yst | 0.7.2 | fail | In text-builder-linear-0.1.4: Template Haskell failed in module Data.Text.Builder.Linear.Dec.Bounded: splice 0: the engine failed: unreachable) |
| zip-archive | 0.5 | pass |  |
| zlib | 0.7.1.1 | pass |  |

## Packages that do not compile with GHC

| Package | Version | Error |
| ------- | ------- | ----- |
| Agda | 2.8.0.2 | src/Data/Set/NonEmpty/Internal.hs:471:37: error: [GHC-83865] • Couldn't match type: Set a |
| Allure | 0.11.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package SDL2_ttf>=2.0.12, not found in the pkg-config database) |
| AspectAG | 0.8.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: AspectAG => base>=4.20.0.0 && <4.21) |
| BlogLiterately | 0.8.8.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: BlogLiterately => base>=4.0 && <4.18) |
| BlogLiterately-diagrams | 0.3.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: BlogLiterately-diagrams => base>=4.3 && <4.18) |
| DRBG | 0.5.5 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: cipher-aes128 => cipher-aes128:setup.Cabal>=1.10 && <3.7) |
| DSH | 0.12.0.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: kure => dlist>=0.6 && <1) |
| Frames | 0.7.4.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: Frames => base>=4.10 && <4.20) |
| GPipe | 2.2.5 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: GPipe => transformers>=0.5.2 && <0.6) |
| HDBC-postgresql | 2.5.0.2 | Error: [Cabal-6666] The program 'pgconfig or pg_config' is required but it could not be found. |
| HaRe | 0.8.4.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: HaRe => base>=4.8.1.0 && <4.10) |
| LambdaHack | 0.11.0.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package SDL2_ttf>=2.0.12, not found in the pkg-config database) |
| MFlow | 0.4.6.0 | Control/Monad/Loc/Transformers.hs:11:1: error: [GHC-61948] Could not find module ‘Control.Monad.Trans.Error’. |
| MonadCatchIO-transformers | 0.3.1.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: MonadCatchIO-transformers => base<4.9) |
| Plot-ho-matic | 0.12.2.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gtk+-3.0-any, not found in the pkg-config database) |
| Spock | 0.14.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: exceptions => mtl==2.3.2/installed-803d, Spock-core => mtl>=2.1 && <2.3) |
| TypeCompose | 0.9.14 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: TypeCompose => base>=4.9 && <4.16) |
| VKHS | 1.9.2 | Hidden/RegexPRCore.hs:34:70: error: [GHC-61689] Module ‘Control.Monad.State’ does not export ‘liftM’. |
| Villefort | 0.1.2.19 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: Villefort => time>=1.6.0 && <1.7) |
| VulkanMemoryAllocator | 0.11.1.0 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| Win32 | 2.14.2.2 | Error: [Cabal-7127] Cannot build the package Win32-2.14.2.2 because none of the components are available to build: the library is marked as 'buildable: False' |
| Workflow | 0.8.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: Workflow => base>=4 && <4.12) |
| X11 | 1.10.3 | configure: error: in `/privatesources/X11-1.10.3/dist-newstyle/build/aarch64-osx/ghc-9.12.4/X11-1.10.3/build': |
| Z-Data | 2.0.1.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: Z-Data => deepseq^>=1.4, pretty => deepseq==1.5.1.0/installed-e519) |
| Z-IO | 2.0.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: Z-IO => primitive>=0.7.3 && <0.8) |
| aeson-injector | 2.0.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: aeson-injector => base>=4.7 && <4.17) |
| alg | 0.2.13.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: alg => base>=4.11 && <4.15) |
| amazonka | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka => base>=4.12 && <4.19) |
| amazonka-autoscaling | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-autoscaling => base>=4.12 && <4.19) |
| amazonka-cloudformation | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-cloudformation => base>=4.12 && <4.19) |
| amazonka-cloudfront | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-cloudfront => base>=4.12 && <4.19) |
| amazonka-cloudhsm | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-cloudhsm => base>=4.12 && <4.19) |
| amazonka-cloudsearch | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-cloudsearch => base>=4.12 && <4.19) |
| amazonka-cloudsearch-domains | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-cloudsearch-domains => base>=4.12 && <4.19) |
| amazonka-cloudtrail | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-cloudtrail => base>=4.12 && <4.19) |
| amazonka-cloudwatch | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-cloudwatch => base>=4.12 && <4.19) |
| amazonka-cloudwatch-logs | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-cloudwatch-logs => base>=4.12 && <4.19) |
| amazonka-codecommit | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-codecommit => base>=4.12 && <4.19) |
| amazonka-codedeploy | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-codedeploy => base>=4.12 && <4.19) |
| amazonka-codepipeline | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-codepipeline => base>=4.12 && <4.19) |
| amazonka-cognito-identity | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-cognito-identity => base>=4.12 && <4.19) |
| amazonka-cognito-sync | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-cognito-sync => base>=4.12 && <4.19) |
| amazonka-config | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-config => base>=4.12 && <4.19) |
| amazonka-core | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-core => base>=4.12 && <4.19) |
| amazonka-datapipeline | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-datapipeline => base>=4.12 && <4.19) |
| amazonka-devicefarm | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-devicefarm => base>=4.12 && <4.19) |
| amazonka-directconnect | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-directconnect => base>=4.12 && <4.19) |
| amazonka-dynamodb | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-dynamodb => base>=4.12 && <4.19) |
| amazonka-ec2 | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-ec2 => base>=4.12 && <4.19) |
| amazonka-ecs | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-ecs => base>=4.12 && <4.19) |
| amazonka-efs | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-efs => base>=4.12 && <4.19) |
| amazonka-elasticache | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-elasticache => base>=4.12 && <4.19) |
| amazonka-elasticbeanstalk | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-elasticbeanstalk => base>=4.12 && <4.19) |
| amazonka-elastictranscoder | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-elastictranscoder => base>=4.12 && <4.19) |
| amazonka-elb | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-elb => base>=4.12 && <4.19) |
| amazonka-emr | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-emr => base>=4.12 && <4.19) |
| amazonka-glacier | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-glacier => base>=4.12 && <4.19) |
| amazonka-iam | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-iam => base>=4.12 && <4.19) |
| amazonka-importexport | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-importexport => base>=4.12 && <4.19) |
| amazonka-iot | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-iot => base>=4.12 && <4.19) |
| amazonka-kinesis | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-kinesis => base>=4.12 && <4.19) |
| amazonka-kms | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-kms => base>=4.12 && <4.19) |
| amazonka-lambda | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-lambda => base>=4.12 && <4.19) |
| amazonka-marketplace-analytics | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-marketplace-analytics => base>=4.12 && <4.19) |
| amazonka-ml | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-ml => base>=4.12 && <4.19) |
| amazonka-opsworks | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-opsworks => base>=4.12 && <4.19) |
| amazonka-rds | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-rds => base>=4.12 && <4.19) |
| amazonka-redshift | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-redshift => base>=4.12 && <4.19) |
| amazonka-route53 | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-route53 => base>=4.12 && <4.19) |
| amazonka-route53-domains | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-route53-domains => base>=4.12 && <4.19) |
| amazonka-s3 | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-s3 => base>=4.12 && <4.19) |
| amazonka-sdb | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-sdb => base>=4.12 && <4.19) |
| amazonka-ses | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-ses => base>=4.12 && <4.19) |
| amazonka-sns | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-sns => base>=4.12 && <4.19) |
| amazonka-sqs | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-sqs => base>=4.12 && <4.19) |
| amazonka-ssm | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-ssm => base>=4.12 && <4.19) |
| amazonka-storagegateway | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-storagegateway => base>=4.12 && <4.19) |
| amazonka-sts | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-sts => base>=4.12 && <4.19) |
| amazonka-support | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-support => base>=4.12 && <4.19) |
| amazonka-swf | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-swf => base>=4.12 && <4.19) |
| amazonka-workspaces | 2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: amazonka-workspaces => base>=4.12 && <4.19) |
| amqp-utils | 0.6.9.0 | cbits/runProcess.c:252:13: error: error: call to undeclared function 'execvpe'; ISO C99 and later do not support implicit function declarations [-Wimplicit-func |
| antiope-athena | 7.5.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, amazonka-core => base>=4.12 && <4.19) |
| antiope-contract | 7.5.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, amazonka-s3 => base>=4.12 && <4.19) |
| antiope-core | 7.5.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, amazonka-core => base>=4.12 && <4.19) |
| antiope-dynamodb | 7.5.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, amazonka-dynamodb => base>=4.12 && <4.19) |
| antiope-messages | 7.5.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, amazonka-core => base>=4.12 && <4.19) |
| antiope-optparse-applicative | 7.5.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, amazonka-s3 => base>=4.12 && <4.19) |
| antiope-s3 | 7.5.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, amazonka-s3 => base>=4.12 && <4.19) |
| antiope-sns | 7.5.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, amazonka-sns => base>=4.12 && <4.19) |
| antiope-sqs | 7.5.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, amazonka-sqs => base>=4.12 && <4.19) |
| apiary | 2.1.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: apiary => wai-extra>=3.0.15 && <3.1) |
| ascii | 1.7.0.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: ascii => base^>=4.16 \|\| ^>=4.17 \|\| ^>=4.18 \|\| ^>=4.19) |
| asn1-data | 0.7.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: asn1-data => base>=3 && <4.13) |
| ats-format | 0.2.0.36 | src/Language/ATS/Lexer.x:554:10: error: [GHC-59692] Duplicate instance declarations: |
| ats-pkg | 3.5.0.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: dhall => aeson>=1.0.0.0 && <2.3) |
| aura | 3.2.10 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: aura => http-client-tls^>=0.3) |
| avro | 0.6.2.1 | src/Data/Avro/Internal/Time.hs:10:1: error: [GHC-87110] Could not load module ‘Data.Time.Format.Internal’. |
| aws-sdk | 0.12.4 | Text/Config/TH.hs:48:5: error: [GHC-83865] • Couldn't match type: [m0 DerivClause] -> m0 Dec |
| b9 | 3.2.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: b9 => lens>=4 && <5) |
| bamboo | 2010.2.25 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: haskell98 => base>=4.7 && <4.8, array => base==4.21.2.0/installed-4943) |
| binary-strict | 0.4.8.6 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: binary-strict => base>=4.12 && <4.15) |
| bioinformatics-toolkit | 0.10.0 | In file included from htslib-1.9/bcf_sr_sort.c:25:0: error: htslib-1.9/config.h:17:1: error: |
| birch-beer | 0.4.2.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: typed-spreadsheet => text<1.3) |
| bishbosh | 0.1.4.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: bishbosh => deepseq^>=1.4.1, bytestring => deepseq==1.5.1.0/installed-e519) |
| blockfrost-client | 0.13.0.0 | src/Blockfrost/Types/Shared/ValidationPurpose.hs:15:20: error: [GHC-76037] Not in scope: type constructor or class ‘ConstructorTagModifier’ |
| boomerang | 1.4.9.4 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: boomerang => template-haskell<2.23) |
| buildbox | 2.2.1.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: buildbox => base>=4.4 && <4.13) |
| cabal-cache | 1.2.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: effectful-zoo => base>=4.17.2.1 && <4.20) |
| cabal-helper | 1.1.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: cabal-helper => template-haskell<2.17 && >=2.11.1.0) |
| cabal-install | 3.18.2.0 | configure: error: required <resolv.h> header not found |
| cabal2arch | 1.1 | Distribution/ArchLinux/PkgBuild.hs:176:10: error: [GHC-76037] Not in scope: type constructor or class ‘Text’ |
| cachix | 1.14.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package nix-store-any, not found in the pkg-config database) |
| cairo | 0.13.12.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package cairo>=1.2.0, not found in the pkg-config database) |
| calamity | 0.12.1.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: calamity => typerep-map>=0.5 && <0.7) |
| call | 0.1.4.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: call => vector>=0.9 && <0.13) |
| candid | 0.5.0.3 | src/Codec/Candid/Types.hs:176:22: error: [GHC-87543] Ambiguous occurrence ‘prettyAnn’. |
| casadi-bindings | 3.4.5.0 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| certificate | 1.3.9 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: certificate => base>=3 && <4.8) |
| chatter | 0.9.1.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: regex-tdfa-text => regex-base<0.94) |
| chp | 2.2.0.1 | Control/Concurrent/CHP/Poison.hs:41:10: error: [GHC-39999] • No instance for ‘Applicative WithPoison’ |
| cipher-aes128 | 0.7.0.6 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: cipher-aes128 => cipher-aes128:setup.Cabal>=1.10 && <3.7) |
| clafer | 0.4.5 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: json-builder => base>=4 && <4.11) |
| clash-ghc | 1.10.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: clash-ghc => ghci>=9.6.0 && <9.15) |
| clash-lib | 1.10.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: clash-lib => ghci>=9.6.0 && <9.15) |
| clash-prelude | 1.10.3 | Error: [Cabal-7107] Could not resolve dependencies: |
| clash-vhdl | 0.7.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: clash-vhdl => wl-pprint-text>=1.1.0.0 && <1.2) |
| classy-prelude-yesod | 1.5.0 | Yesod/Static.hs:414:22: error: [GHC-39999] • Could not deduce ‘ByteArray.ByteArrayAccess (Digest MD5)’ |
| clckwrks | 0.28.0.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: userid => base>=4.6 && <4.21) |
| clckwrks-plugin-page | 0.4.3.26 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: clckwrks-plugin-page => base>=4.3 && <4.17) |
| codex | 0.6.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: codex => zlib>=0.5.4.1 && <0.7) |
| compdata | 0.13.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: compdata => base>=4.16 && <4.20) |
| composite-base | 0.8.3.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: composite-base => template-haskell>=2.11.1.0 && <2.22) |
| composite-ekg | 0.8.2.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: composite-ekg => text>=1.2.2.2 && <1.3) |
| concraft | 0.14.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: concraft => zlib>=0.5 && <0.7) |
| control-monad-exception | 0.11.4 | Control/Monad/Loc/Transformers.hs:11:1: error: [GHC-61948] Could not find module ‘Control.Monad.Trans.Error’. |
| couchdb-conduit | 0.10.6 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: couchdb-conduit => transformers>=0.2 && <0.4) |
| cpsa | 4.4.9 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: cpsa => deepseq>=1.4.8 && <1.5, bytestring => deepseq==1.5.1.0/installed-e519) |
| creatur | 5.9.30 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: creatur => unix^>=2.7.2) |
| cryptocipher | 0.6.2 | Crypto/Cipher/Blowfish/Primitive.hs:47:36: error: [GHC-39999] • No instance for ‘MonadFail (Either String)’ |
| cuda | 0.13.0.0 | Error: setup: |
| darcs | 2.18.5 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: darcs => base>=4.10 && <4.20) |
| darcs-beta | 2.7.99.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: darcs-beta => darcs-beta:setup.Cabal>=1.7 && <1.25) |
| data-lens | 2.11.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: data-lens => containers>=0.3 && <0.6) |
| data-lens-template | 2.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: data-lens-template => template-haskell>=2.11 && <2.12) |
| dataenc | 0.14.0.7 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: dataenc => base>=3.0.0 && <4.8) |
| derive | 2.6.5 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: derive => haskell-src-exts>=1.20 && <1.21) |
| dhall-json | 1.7.12 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: dhall-json => aeson>=1.4.6.0 && <2.2) |
| dhall-lsp-server | 1.1.4 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: dhall-lsp-server => aeson>=1.3.1.1 && <2.3) |
| dhall-nix | 1.1.27 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hnix => template-haskell>=2.13 && <2.22) |
| dhall-text | 1.0.18 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: dhall-text => text>=0.11.1.0 && <1.3) |
| dhscanner-ast | 1.6.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: dhscanner-ast => base>=4.22 && <4.23) |
| dhscanner-bitcode | 2.4.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: dhscanner-bitcode => base>=4.22 && <4.23) |
| dhscanner-kbgen | 2.4.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: dhscanner-kbgen => base>=4.22 && <4.23) |
| diagrams-builder | 0.8.0.6 | Error: [Cabal-7107] Could not resolve dependencies: |
| diagrams-cairo | 1.5 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package pango>=0 && <1.26.0 \|\| >1.26.2, not found in the pkg-config database) |
| diagrams-haddock | 0.4.1.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: diagrams-haddock => base>=4.4 && <4.20) |
| doctest | 0.25.0.2 | Error: [Cabal-7107] Could not resolve dependencies: |
| dynamic-plot | 0.4.2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: half-space => base>=4.10 && <4.19) |
| egison | 5.1.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: exceptions => template-haskell==2.23.0.0/installed-fe91, sweet-egison => template-haskell<2.23) |
| egison-tutorial | 4.1.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: sweet-egison => template-haskell<2.23) |
| eigen | 3.3.7.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: eigen => vector>=0.5 && <0.13) |
| encoding | 0.10.3 | Data/Encoding.hs:104:22: error: [GHC-87543] Ambiguous occurrence ‘toList’. |
| engine-io | 1.2.23 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: engine-io => base>=4.8 && <4.15) |
| enumerator | 0.4.20 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: enumerator => base>=4.0 && <4.11) |
| espial | 0.0.42 | Yesod/Static.hs:414:22: error: [GHC-39999] • Could not deduce ‘ByteArray.ByteArrayAccess (Digest MD5)’ |
| eventstore | 1.4.3 | Database/EventStore/Internal/Control.hs:95:14: error: [GHC-76037] Not in scope: type constructor or class ‘MonadFix’ |
| extensible | 0.9.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: extensible => hashable>=1.2 && <1.5) |
| extensible-effects | 5.0.0.1 | src/Control/Eff/Internal.hs:369:10: error: [GHC-39999] • Could not deduce ‘Applicative b’ |
| extensible-effects-concurrent | 2.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: extensible-effects-concurrent => text>=1.2 && <1.3) |
| fay | 0.24.2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: fay => base>=4.9 && <4.15) |
| fclabels | 2.0.5.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: fclabels => base>=4.5 && <4.19) |
| fltkhs | 0.8.0.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: fltkhs => fltkhs:setup.base>=4.5 && <4.14) |
| freckle-app | 1.27.0.3 | Error: [Cabal-7107] Could not resolve dependencies: |
| free-game | 1.2 | Codec/Picture/RGBA8.hs:80:86: error: [GHC-88464] Variable not in scope: wordToInteger :: Word# -> a0 |
| futhark | 0.29.1 | timed out after 900 seconds |
| gf | 3.12 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: gf => ghc-prim>=0.5.0 && <=0.10.0, base => ghc-prim==0.13.0/installed-b8c9) |
| ghc-exactprint | 1.14.3.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: ghc-exactprint => base>=4.22 && <4.23) |
| ghc-lib | 9.14.1.20251220 | compiler/GHC/ThToHs.hs:66:1: error: [GHC-45102] Ambiguous module name ‘GHC.Boot.TH.Syntax’. |
| ghc-lib-parser | 9.14.1.20251220 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hpc => containers>=0.4.1 && <0.8) |
| ghc-lib-parser-ex | 9.14.2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hpc => containers>=0.4.1 && <0.8) |
| ghc-mod | 5.8.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: ghc-mod => base<4.10 && >=4.6.0.1) |
| ghc-prof | 1.4.1.14 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: ghc-prof => base>=4.6 && <4.20) |
| ghc-tcplugin-api | 0.20.1.1 | Error: [Cabal-7107] Could not resolve dependencies: |
| ghc-typelits-extra | 0.5.4 | Error: [Cabal-7107] Could not resolve dependencies: |
| ghc-typelits-knownnat | 0.8.4 | Error: [Cabal-7107] Could not resolve dependencies: |
| ghc-typelits-natnormalise | 0.9.7 | Error: [Cabal-7107] Could not resolve dependencies: |
| ghcide | 2.15.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: template-haskell => ghc-boot-th==9.12.4/installed-c44e, ghc-boot -bootstrap => ghc-boot-th==9.14. |
| ghcjs-dom-jsffi | 0.9.9.0 | Error: [Cabal-7127] Cannot build the package ghcjs-dom-jsffi-0.9.9.0 because none of the components are available to build: the library is marked as 'buildable: |
| gi-atk | 2.0.29 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package atk>=2.0 && <3, not found in the pkg-config database) |
| gi-cairo | 1.0.31 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package cairo-gobject>=1.0 && <2, not found in the pkg-config database) |
| gi-gdk | 4.0.10 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gtk4-any, not found in the pkg-config database) |
| gi-gio | 2.0.39 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gio-2.0-any, not found in the pkg-config database) |
| gi-girepository | 1.0.30 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gobject-introspection-1.0-any, not found in the pkg-config database) |
| gi-glib | 2.0.31 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package glib-2.0-any, not found in the pkg-config database) |
| gi-gobject | 2.0.32 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gobject-2.0-any, not found in the pkg-config database) |
| gi-gtk | 4.0.12 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gtk4-any, not found in the pkg-config database) |
| gi-gtksource | 5.0.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gtksourceview-5-any, not found in the pkg-config database) |
| gi-javascriptcore | 6.0.5 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package javascriptcoregtk-6.0-any, not found in the pkg-config database) |
| gi-notify | 0.7.30 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package libnotify>=0.7 && <1, not found in the pkg-config database) |
| gi-pango | 1.0.32 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package pango>=1.0 && <2, not found in the pkg-config database) |
| gi-poppler | 0.18.31 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package poppler-glib>=0.18, not found in the pkg-config database) |
| gi-soup | 3.0.4 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package libsoup-3.0-any, not found in the pkg-config database) |
| gi-vte | 2.91.36 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package vte-2.91-any, not found in the pkg-config database) |
| gi-webkit2 | 4.0.32 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package webkit2gtk-4.0-any, not found in the pkg-config database) |
| gio | 0.13.12.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gio-2.0>=2.16.4, not found in the pkg-config database) |
| git-annex | 10.20261113 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| gitit | 0.16.1 | Network/Captcha/ReCaptcha.hs:48:19: error: [GHC-83865] • Couldn't match expected type ‘text-2.1.4:Data.Text.Internal.Lazy.Text’ |
| glib | 0.13.12.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gobject-2.0-any, not found in the pkg-config database) |
| glirc | 2.41 | dist-newstyle/build/aarch64-osx/ghc-9.12.4/glirc-2.41/setup/setup.hs:68:20: error: [GHC-83865] • Couldn't match type: Distribution.Utils.Path.SymbolicPathX |
| gloss-examples | 1.13.0.4 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: repa-algorithms => base>=4.8 && <4.21) |
| gnss-converters | 0.3.52 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: word24 => base>=4.7 && <4.15) |
| groundhog | 0.12.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: groundhog => transformers>=0.2.1 && <0.6) |
| groundhog-postgresql | 0.12 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: groundhog-postgresql => vector>=0.10 && <0.13) |
| gsl-random | 0.5.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: gsl-random => vector>=0.7.0.1 && <0.13.0.0) |
| gtk | 0.15.10 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gtk+-2.0-any, not found in the pkg-config database) |
| gtk-sni-tray | 0.2.1.5 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gtk+-3.0-any, not found in the pkg-config database) |
| gtk3 | 0.15.10 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gtk+-3.0-any, not found in the pkg-config database) |
| hOpenPGP | 3.7.4.1 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| hackport | 0.9.1.0 | configure: error: required <resolv.h> header not found |
| haddock | 2.29.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: haddock => base^>=4.13.0.0 \|\| ^>=4.14.0.0 \|\| ^>=4.15.0.0 \|\| ^>=4.16.0.0 \|\| ^>=4.17.0.0 \|\| ^>=4.18 |
| haddock-api | 2.29.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: haddock-api => base^>=4.16.0) |
| hadolint | 2.15.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: time==1.14/installed-d1ed, language-docker => time>=1.12.2 && <1.14) |
| hapistrano | 0.4.8.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hapistrano => time>=1.5 && <1.11) |
| happstack-authenticate | 2.6.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: userid => base>=4.6 && <4.21) |
| happstack-hsp | 7.3.7.8 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hsx2hs => template-haskell>=2.7 && <2.20) |
| hashed-storage | 0.5.11 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hashed-storage => base>=4 && <4.8) |
| haskell-gi | 0.27.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gobject-2.0>=2.32, not found in the pkg-config database) |
| haskell-gi-base | 0.26.9 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package glib-2.0-any, not found in the pkg-config database) |
| haskell-language-server | 2.15.0.0 | Error: [Cabal-7107] Could not resolve dependencies: |
| haskell-lsp | 0.24.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: haskell-lsp => base>=4.9 && <4.16) |
| haskell-lsp-types | 0.24.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: haskell-lsp-types => base>=4.9 && <4.16) |
| haskell-neo4j-client | 0.3.2.4 | src/Database/Neo4j/Types.hs:159:42: error: [GHC-87543] Ambiguous occurrence ‘<>’. |
| haskell-tools-prettyprint | 1.1.1.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: haskell-tools-prettyprint => base>=4.11 && <4.13) |
| haskell-tools-refactor | 1.1.1.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: haskell-tools-refactor => base>=4.11 && <4.13) |
| haskell98 | 2.0.0.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: haskell98 => base>=4.7 && <4.8) |
| haskoin-node | 2.6.0 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| haskoin-store | 1.15.1 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| haste-compiler | 0.6.0.0 | Error: [Cabal-7107] Could not resolve dependencies: |
| hat | 2.9.4 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hat => haskell-src-exts>=1.18 && <1.19) |
| hbro | 1.7.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package webkitgtk-3.0>=1.8, not found in the pkg-config database) |
| hercules-ci-agent | 0.10.11 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package nix-main>=2.28 && <2.36, not found in the pkg-config database) |
| hie-bios | 0.21.0 | Error: [Cabal-7107] Could not resolve dependencies: |
| highlighting-kate | 0.6.4 | Text/Highlighting/Kate/Common.hs:101:3: error: [GHC-88464] Variable not in scope: |
| hindent | 6.3.0 | compiler/GHC/Tc/Errors/Ppr.hs:44:1: error: [GHC-61948] Could not find module ‘GHC.Internal.TH.Ppr’. |
| hint | 0.9.0.9 | Error: [Cabal-7107] Could not resolve dependencies: |
| hip | 1.5.6.0 | <no location info>: error: Simplifier ticks exhausted |
| hjsonschema | 1.10.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hjsonschema => vector>=0.10 && <0.13) |
| hledger-iadd | 1.3.23 | Data/Encoding.hs:104:22: error: [GHC-87543] Ambiguous occurrence ‘toList’. |
| hledger-lib | 1.52.4 | Data/Encoding.hs:104:22: error: [GHC-87543] Ambiguous occurrence ‘toList’. |
| hlibgit2 | 0.18.0.16 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hlibgit2 => hlibgit2:setup.Cabal>=1.9 && <1.25) |
| hlint | 3.10 | compiler/GHC/Tc/Errors/Ppr.hs:44:1: error: [GHC-61948] Could not find module ‘GHC.Internal.TH.Ppr’. |
| hls-plugin-api | 2.15.0.0 | Error: [Cabal-7107] Could not resolve dependencies: |
| hnix | 0.17.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hnix => template-haskell>=2.13 && <2.22) |
| hoogle | 5.0.19.0 | compiler/GHC/Tc/Errors/Ppr.hs:44:1: error: [GHC-61948] Could not find module ‘GHC.Internal.TH.Ppr’. |
| hopenpgp-tools | 0.26.2 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| hpage | 0.12.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hpage => wxcore>=0.12.1.4 && <0.13) |
| hpc | 0.7.0.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hpc => base>=4.12 && <4.21) |
| hpc-coveralls | 1.0.10 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hpc-coveralls => transformers>=0.4.1 && <0.6) |
| hpqtypes | 1.15.0.0 | Error: .cabal-wrapped: |
| hpqtypes-extras | 1.21.0.0 | Error: .cabal-wrapped: |
| hprotoc | 2.4.17 | Text/ProtocolBuffers/Get.hs:70:68: error: [GHC-61689] Module ‘Control.Monad.Error.Class’ does not export ‘Error’. |
| hruby | 0.5.1.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package ruby-any, not found in the pkg-config database) |
| hsbencher | 1.20.0.5 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hsbencher => base>=4.5 && <4.9) |
| hsdev | 0.3.4.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hsdev => vector>=0.11.0.0 && <0.13) |
| hsignal | 0.2.7.5 | clang: error: unsupported option '-msse2' for target 'arm64-apple-darwin' |
| hsimport | 0.11.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: hsimport => text>=0.11.3.1 && <1.3) |
| htsn-import | 0.2.4 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: groundhog-th => aeson>=0.7 && <2) |
| http-enumerator | 0.7.3.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: http-enumerator => transformers>=0.2 && <0.4) |
| http-proxy | 0.1.2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: http-proxy => warp-tls>=3.3 && <3.4) |
| http2-client | 0.10.0.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: http2-client => base>=4.7 && <4.20) |
| husk-scheme | 3.20 | hs-src/Language/Scheme/Plugins/CPUTime.hs:28:8: error: [GHC-88464] Variable not in scope: |
| idris | 1.3.4 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: idris => vector<0.13) |
| ihaskell | 0.13.0.0 | Error: [Cabal-7107] Could not resolve dependencies: |
| influxdb | 1.9.3.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: influxdb => base>=4.11 && <4.21) |
| intero | 0.1.40 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: intero => ghc>=7.8 && <=8.6.5) |
| irc-client | 1.1.2.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: irc-client => tls>=1.3 && <1.6) |
| iteratee | 0.8.9.6 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: iteratee => transformers>=0.2 && <0.5) |
| json-autotype | 3.1.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: json-autotype => bytestring>=0.9 && <0.11) |
| kioku-cli | 0.13.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: crypton==2.2.6, kioku-core => crypton^>=1.1.4) |
| kioku-core | 0.13.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: kioku-core => crypton^>=1.1.4) |
| kioku-migrate | 0.13.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pg-migrate => crypton>=1.1 && <1.2) |
| language-ats | 1.7.10.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: language-ats => deepseq<1.5) |
| language-puppet | 1.5.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: language-puppet => servant-client>=0.9 && <0.20) |
| launchdarkly-server-sdk | 4.6.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: launchdarkly-server-sdk => zlib>=0.6.2.2 && <0.7) |
| leksah | 0.16.2.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: leksah => base>=4.0.0.0 && <4.11) |
| leksah-server | 0.16.3.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: leksah-server => base>=4.0.0.0 && <4.11) |
| liquidhaskell | 0.9.14.1.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: liquidhaskell-boot => ghc^>=9.14) |
| llvm-extra | 0.13.1 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| llvm-ffi | 21.0.0.2 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| llvm-general | 3.5.1.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: llvm-general => transformers>=0.3.0.0 && <0.5) |
| llvm-tf | 21.0.0.2 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| log-warper | 1.9.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: log-warper => deepseq^>=1.4, pretty => deepseq==1.5.1.0/installed-e519) |
| lorentz | 0.16.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, base-noprelude => base==4.13.0.0) |
| madlang | 4.0.2.16 | Error: [Cabal-7125] Failed to build madlang-4.0.2.16. The failure occurred during the configure step. |
| manifolds | 0.6.1.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: half-space => base>=4.10 && <4.19) |
| matterhorn | 90000.1.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: matterhorn => vty-crossplatform>=0.2.0.0 && <0.5.0.0) |
| mattermost-api | 90000.1.1 | src/Network/Mattermost/Util.hs:98:19: error: [GHC-83865] • Couldn't match expected type ‘TLSSettings’ |
| mmsyn7ukr | 0.17.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: mmsyn7ukr => base>=4.7 && <4.15) |
| moesocks | 1.0.1.0 | src/Network/MoeSocks/Encrypt.hs:20:61: error: [GHC-61689] Module ‘Control.Monad.Except’ does not export ‘liftIO’. |
| morley | 1.20.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pairing => protolude>=0.2 && <0.3) |
| morte | 1.7.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: morte => optparse-applicative<0.15) |
| nested-routes | 10.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: sets => witherable<0.5) |
| network-protocol-xmpp | 0.5.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package libgsasl>=1.1, not found in the pkg-config database) |
| ngx-export-tools-extra | 1.2.13.1 | configure: error: required <resolv.h> header not found |
| nova-cache | 0.12.0.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: nova-cache => base>=4.22 && <5) |
| octane | 0.20.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: octane => base>=4.9 && <4.11) |
| opencascade-hs | 0.8.0.0 | Error: [Cabal-4345] Missing dependencies on foreign libraries: |
| pandoc-citeproc | 0.17.0.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pandoc-citeproc => pandoc-types>=1.20 && <1.22) |
| pandora | 0.5.6 | Pandora/Pattern/Operation/Sum.hs:7:1: error: [GHC-87110] Could not find module ‘Pandora.Pattern.Operation.Unit’. |
| pango | 0.13.12.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package pango>=0 && <1.26.0 \|\| >1.26.2, not found in the pkg-config database) |
| persistent-mysql | 2.13.1.6 | Error: [Cabal-7125] Failed to build mysql-0.2.1 (which is required by persistent-mysql-2.13.1.6). The failure occurred during the configure step. The exception |
| peyotls | 0.1.6.10 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: peyotls => x509-validation>=1.5 && <1.6) |
| phonetic-languages-simplified-examples-array | 0.21.0.0 | Phonetic/Languages/Simplified/DataG/Base.hs:107:30: error: [GHC-39999] • Could not deduce ‘Eq b’ |
| phonetic-languages-simplified-generalized-examples-array | 0.19.0.1 | Phonetic/Languages/Simplified/DataG/Base.hs:107:30: error: [GHC-39999] • Could not deduce ‘Eq b’ |
| pinboard | 0.10.3.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pinboard => vector>=0.10.9 && <0.13) |
| plot | 0.2.3.12 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package pango>=0 && <1.26.0 \|\| >1.26.2, not found in the pkg-config database) |
| potoki | 2.1.4.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: potoki-core => vector>=0.12 && <0.13) |
| potoki-core | 2.3.4.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: potoki-core => vector>=0.12 && <0.13) |
| preamble | 0.0.65 | src/Preamble/Prelude.hs:86:20: error: [GHC-87543] Ambiguous occurrence ‘show’. |
| pred-trie | 0.6.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: sets => witherable<0.5) |
| project-m36 | 1.2.6 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: ghc => base==4.21.2.0/installed-4943, barbies-th => base>=4.12 && <4.20) |
| protocol-buffers | 2.4.17 | Text/ProtocolBuffers/Get.hs:70:68: error: [GHC-61689] Module ‘Control.Monad.Error.Class’ does not export ‘Error’. |
| protocol-buffers-descriptor | 2.4.17 | Text/ProtocolBuffers/Get.hs:70:68: error: [GHC-61689] Module ‘Control.Monad.Error.Class’ does not export ‘Error’. |
| publicsuffix | 0.20200526 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: publicsuffix => base>=4 && <4.15) |
| purescript | 0.15.15 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: purescript => base>=4.16.2.0 && <4.17) |
| reanimate | 1.1.6.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: transformers==0.6.3.0/installed-9834, reanimate-svg => transformers>=0.3 && <0.6) |
| reanimate-svg | 0.13.0.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: reanimate-svg => transformers>=0.3 && <0.6) |
| repa-examples | 3.4.2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: repa-examples => base>=4.8 && <4.21) |
| representable-functors | 3.2.0.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: representable-functors => transformers>=0.2 && <0.4) |
| rest-core | 0.39.0.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: rest-core => base>=4.5 && <4.12) |
| rest-gen | 0.20.0.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: rest-gen => base>=4.5 && <4.12) |
| scion-browser | 0.5.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: scion-browser => zlib>=0.5 && <0.6) |
| sdl2 | 2.5.6.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package sdl2>=2.0.14, not found in the pkg-config database) |
| secp256k1 | 1.1.2 | src/Crypto/Secp256k1.hs:201:12: error: [GHC-87543] Ambiguous occurrence ‘isSuccess’. |
| seonbi | 0.3.4 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: seonbi => warp>=3.2 && <3.4) |
| serverless-haskell | 0.12.6 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, amazonka-s3 => base>=4.12 && <4.19) |
| sessions | 2008.7.18 | Control/Concurrent/Session/Base/Bool.hs:32:6: error: [GHC-50649] Illegal declaration of a type or class operator ‘:$’ |
| shake-ats | 1.10.4.2 | src/Data/Dependency/Error.hs:24:28: error: [GHC-25897] • Couldn't match type ‘ann’ |
| simple-effects | 0.13.0.0 | src/Import.hs:8:1: error: [GHC-61948] Could not find module ‘Control.Monad.Trans.Error’. |
| simple-log | 0.9.12 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: simple-log => text>=0.11.0 && <2.0.0) |
| snaplet-postgresql-simple | 1.2.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: snaplet-postgresql-simple => base>=4 && <4.14) |
| som | 10.1.11 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: som => deepseq^>=1.4) |
| speculation | 1.5.0.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: speculation => stm>=2.1 && <2.5) |
| splot | 0.3.14 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package cairo>=1.2.0, not found in the pkg-config database) |
| squeal-postgresql | 0.9.2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: records-sop => ghc-prim>=0.5 && <0.11, base => ghc-prim==0.13.0/installed-b8c9) |
| srtree | 3.0.0.7 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| stack | 3.11.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: unix==2.8.6.0, ghc-boot => unix==2.8.7.0/installed-f012) |
| stackage-curator | 0.16.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, amazonka-s3 => base>=4.12 && <4.19) |
| stripe-haskell | 2.6.2 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: stripe-core => text>=1.0 && <1.3) |
| stylish-haskell | 0.15.1.0 | compiler/GHC/Tc/Errors/Ppr.hs:44:1: error: [GHC-61948] Could not find module ‘GHC.Internal.TH.Ppr’. |
| syb-with-class | 0.6.1.14 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: syb-with-class => template-haskell>=2.4 && <2.19) |
| sydtest | 0.33.4.0 | src/Test/Syd/OptParse.hs:785:44: error: [GHC-83865] • Couldn't match type ‘Maybe Threads’ with ‘Threads’ |
| taffybar | 7.4.4 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package gtk+-3.0-any, not found in the pkg-config database) |
| telegram-api | 0.7.2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: telegram-api => servant-client-core>=0.12 && <0.13) |
| text-format | 0.3.2.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: text-format => base>=4.3 && <4.19) |
| text-show-instances | 3.9.11 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: deepseq => base==4.21.2.0/installed-4943, hpc => base>=4.12 && <4.21) |
| threepenny-editors | 0.5.6.1 | src/Data/HasEmpty.hs:53:54: error: [GHC-88464] Variable not in scope: (@) :: Proxy t0 -> t1 -> proxy0 c0 |
| tianbar | 1.2.5 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package webkit2gtk-4.0-any, not found in the pkg-config database) |
| timeplot | 1.0.33 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package cairo>=1.2.0, not found in the pkg-config database) |
| tls-extra | 0.6.6 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: tls-extra => tls>=1.1.0 && <1.2.0) |
| too-many-cells | 3.0.1.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package zlib-any, not found in the pkg-config database) |
| tweet-hs | 1.0.2.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: tweet-hs => bytestring<0.12, unix => bytestring==0.12.2.0/installed-814d) |
| type-natural | 1.3.0.2 | Error: [Cabal-7107] Could not resolve dependencies: |
| uhc-light | 1.1.10.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: fclabels => base>=4.5 && <4.19) |
| uniqueness-periods-vector-examples | 0.15.0.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: uniqueness-periods-vector-examples => base>=4.7 && <4.15) |
| usb-safe | 0.14 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: usb-safe => base>=4 && <4.5) |
| vulkan | 3.27 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| vulkan-utils | 0.5.11.0 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| wai-routes | 0.10.4 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: wai-routes => wai-extra>=3.0 && <3.1) |
| waterfall-cad | 0.8.0.0 | Error: [Cabal-4345] Missing dependencies on foreign libraries: |
| waterfall-cad-examples | 0.8.0.0 | Error: [Cabal-4345] Missing dependencies on foreign libraries: |
| webkit | 0.14.2.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package webkit-1.0>=1.8, not found in the pkg-config database) |
| weeder | 2.11.0 | Error: [Cabal-7107] Could not resolve dependencies: |
| werewolf | 1.5.2.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: werewolf => optparse-applicative>=0.11 && <0.14) |
| wl-pprint-extras | 3.5.0.5 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: wl-pprint-extras => semigroupoids>=3 && <6) |
| wl-pprint-terminfo | 3.7.1.4 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: wl-pprint-extras => semigroupoids>=3 && <6) |
| wolf | 0.3.48 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: base==4.21.2.0/installed-4943, amazonka-swf => base>=4.12 && <4.19) |
| wx | 0.92.3.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: wxdirect => process>=1.1 && <1.5) |
| wxcore | 0.92.3.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: wxdirect => process>=1.1 && <1.5) |
| xml-enumerator | 0.4.4.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: xml-enumerator => transformers>=0.2 && <0.3) |
| xmobar | 0.51.1 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: pkg-config package pango>=0 && <1.26.0 \|\| >1.26.2, not found in the pkg-config database) |
| xmonad-contrib | 0.18.2 | configure: error: in `/privatesources/xmonad-contrib-0.18.2/dist-newstyle/tmp/src-58310/X11-1.10.3/dist/build': |
| xmonad-extras | 0.17.3 | configure: error: in `/privatesources/xmonad-extras-0.17.3/dist-newstyle/tmp/src-98399/X11-1.10.3/dist/build': |
| yamlscript | 0.3.7.0 | Error: [Cabal-4345] Missing dependency on a foreign library: |
| yesod | 1.6.2.3 | Data/Encoding.hs:104:22: error: [GHC-87543] Ambiguous occurrence ‘toList’. |
| yesod-auth | 1.6.12.1 | Yesod/Auth/Util/PasswordStore.hs:139:7: error: [GHC-39999] • No instance for ‘memory-0.18.0:Data.ByteArray.Types.ByteArrayAccess |
| yesod-auth-fb | 1.10.1 | Yesod/Auth/Util/PasswordStore.hs:139:7: error: [GHC-39999] • No instance for ‘memory-0.18.0:Data.ByteArray.Types.ByteArrayAccess |
| yesod-auth-hashdb | 1.7.1.7 | Yesod/Auth/Util/PasswordStore.hs:139:7: error: [GHC-39999] • No instance for ‘memory-0.18.0:Data.ByteArray.Types.ByteArrayAccess |
| yesod-bin | 1.6.2.4 | AddHandler.hs:250:71: error: [GHC-83865] • Couldn't match expected type ‘Distribution.Verbosity.Verbosity’ |
| yesod-core | 1.7.0.0 | Data/Encoding.hs:104:22: error: [GHC-87543] Ambiguous occurrence ‘toList’. |
| yesod-form | 1.7.11 | Data/Encoding.hs:104:22: error: [GHC-87543] Ambiguous occurrence ‘toList’. |
| yesod-persistent | 1.6.0.9 | Data/Encoding.hs:104:22: error: [GHC-87543] Ambiguous occurrence ‘toList’. |
| yesod-platform | 1.2.13.3 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: yesod-routes => vector>=0.8 && <0.11) |
| yesod-routes | 1.2.0.7 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: yesod-routes => vector>=0.8 && <0.11) |
| yesod-static | 1.6.1.3 | Yesod/Static.hs:414:22: error: [GHC-39999] • Could not deduce ‘ByteArray.ByteArrayAccess (Digest MD5)’ |
| zeolite-lang | 0.24.1.0 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: zeolite-lang => base>=4.9 && <4.20) |
| zoom-cache | 1.2.1.6 | Error: [Cabal-7107] Could not resolve dependencies: (conflict: zoom-cache => transformers>=0.2 && <0.4) |
