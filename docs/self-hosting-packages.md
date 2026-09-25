# Self-hosting package list

AIHC is self-hosting when it can compile itself. The table below gives every
package that GHC builds for the `aihc` executable, and `aihc` itself last.
A package comes after all of its dependencies.

`scripts/update-self-hosting-packages.sh` writes this file from the cabal plan
of `exe:aihc`. The weekly
[Generated Reports](../.github/workflows/generated-reports-update.yml)
workflow runs it, then installs each package with
`scripts/self-hosting-progress.sh`. The workflow writes the result to the
"Self-compile" row of the README.

The table does not include the boot packages that the aihc core libraries
replace: `base`, `ghc-internal`, `ghc-prim`, `rts`, `system-cxx-std-lib`, and
`template-haskell`. The versions of the other boot packages are the versions
that GHC 9.12.4 ships.

The "Source" column is `hackage` for a Hackage release, `local:PATH` for a
package in this repository, or `git:URL@COMMIT` for a pinned Git commit.

## Packages

| Package | Version | Source | Dependencies |
| ------- | ------- | ------ | ------------ |
| OneTuple | 0.4.3 | hackage |  |
| array | 0.5.8.0 | hackage |  |
| assoc | 1.1.1 | hackage |  |
| atomic-counter | 0.1.2.4 | hackage |  |
| base-orphans | 0.9.4 | hackage |  |
| basement | 0.0.16 | hackage |  |
| byteorder | 1.0.4 | hackage |  |
| character-ps | 0.1 | hackage |  |
| colour | 2.3.7 | hackage |  |
| ansi-terminal-types | 1.1.3 | hackage | colour |
| ansi-terminal | 1.1.5 | hackage | ansi-terminal-types, colour |
| deepseq | 1.5.1.0 | hackage |  |
| bytestring | 0.12.2.0 | hackage | deepseq |
| appar | 0.1.8 | hackage | bytestring |
| base16-bytestring | 1.0.2.0 | hackage | bytestring |
| base64-bytestring | 1.2.1.0 | hackage | bytestring |
| containers | 0.7 | hackage | array, deepseq |
| binary | 0.8.9.3 | hackage | array, bytestring, containers |
| cereal | 0.5.8.3 | hackage | array, bytestring, containers |
| cryptohash-sha256 | 0.11.102.1 | hackage | bytestring |
| data-default | 0.8.0.2 | hackage | containers |
| data-default-class | 0.2.0.0 | hackage | data-default |
| dlist | 1.0 | hackage | deepseq |
| ghc-bignum | 1.3 | hackage |  |
| half | 0.3.3 | hackage | binary, deepseq |
| integer-gmp | 1.1 | hackage | ghc-bignum |
| integer-logarithms | 1.0.5 | hackage | array |
| libffi | 0.2.1 | hackage | bytestring |
| memory | 0.18.0 | hackage | basement, bytestring, deepseq |
| crypton | 1.0.6 | hackage | basement, bytestring, deepseq, integer-gmp, memory |
| hpke | 0.0.0 | hackage | base16-bytestring, bytestring, crypton, memory |
| network-byte-order | 0.1.8 | hackage | bytestring |
| old-locale | 1.0.0.7 | hackage |  |
| old-time | 1.1.1.0 | hackage | old-locale |
| parser-combinators | 1.3.1 | hackage |  |
| pretty | 1.1.3.6 | hackage | deepseq |
| ghc-boot-th | 9.12.4 | hackage | pretty |
| splitmix | 0.1.3.2 | hackage | deepseq |
| stm | 2.5.3.1 | hackage | array |
| tagged | 0.8.10 | hackage | deepseq |
| terminfo | 0.4.1.7 | hackage |  |
| text | 2.1.4 | hackage | array, binary, bytestring, deepseq |
| blaze-builder | 0.4.4.1 | hackage | bytestring, text |
| mime-types | 0.1.2.2 | hackage | bytestring, containers, text |
| prettyprinter | 1.7.2 | hackage | text |
| prettyprinter-ansi-terminal | 1.1.4 | hackage | ansi-terminal, prettyprinter, text |
| th-abstraction | 0.7.2.0 | hackage | containers |
| th-compat | 0.1.7 | hackage |  |
| time | 1.14 | hackage | deepseq |
| cookie | 0.5.1 | hackage | bytestring, data-default-class, deepseq, text, time |
| time-hourglass | 0.3.0 | hackage | deepseq |
| transformers | 0.6.3.0 | hackage |  |
| StateVar | 1.2.2 | hackage | stm, transformers |
| contravariant | 1.5.6 | hackage | StateVar, transformers |
| distributive | 0.6.3 | hackage | tagged, transformers |
| indexed-traversable | 0.1.5 | hackage | array, containers, transformers |
| comonad | 5.0.10 | hackage | containers, distributive, indexed-traversable, tagged, transformers |
| bifunctors | 5.6.3 | hackage | assoc, comonad, containers, tagged, th-abstraction |
| mtl | 2.3.2 | hackage | transformers |
| exceptions | 0.10.12 | hackage | mtl, stm, transformers |
| os-string | 2.0.10 | hackage | bytestring, deepseq, exceptions |
| filepath | 1.5.5.0 | hackage | bytestring, deepseq, exceptions, os-string |
| aihc-cpp | 2.0.0.0 | hackage | bytestring, containers, deepseq, filepath, text |
| ech-config | 0.0.1 | hackage | base16-bytestring, bytestring, filepath, network-byte-order |
| hashable | 1.5.1.0 | hackage | bytestring, containers, deepseq, filepath, os-string, text |
| case-insensitive | 1.2.1.0 | hackage | bytestring, deepseq, hashable, text |
| data-fix | 0.3.4 | hackage | deepseq, hashable |
| http-types | 0.12.6 | hackage | bytestring, case-insensitive, text |
| parsec | 3.1.18.0 | hackage | bytestring, mtl, text |
| network-uri | 2.6.4.2 | hackage | deepseq, parsec, th-compat |
| primitive | 0.9.1.0 | hackage | deepseq, transformers |
| cborg | 0.2.10.0 | hackage | array, bytestring, containers, deepseq, ghc-bignum, half, primitive, text |
| integer-conversion | 0.1.1 | hackage | bytestring, primitive, text |
| random | 1.3.1 | hackage | bytestring, deepseq, mtl, splitmix, transformers |
| QuickCheck | 2.18.0.0 | hackage | containers, deepseq, random, splitmix, transformers |
| scientific | 0.3.8.1 | hackage | binary, bytestring, containers, deepseq, hashable, integer-logarithms, primitive, text |
| megaparsec | 9.8.2 | hackage | array, bytestring, case-insensitive, containers, deepseq, mtl, parser-combinators, scientific, text, transformers |
| aihc-parser | 4.0.0.0 | git:https://github.com/ai-haskell-compiler/aihc-parser.git@46420da73d505555b46c03c00bd7fe44cb72e331 | bytestring, containers, deepseq, megaparsec, prettyprinter, text |
| aihc-resolve | 0.1.0.0 | local:components/aihc-resolve | aihc-parser, containers, deepseq, text |
| aihc-tc | 0.1.0.0 | local:components/aihc-tc | aihc-parser, aihc-resolve, containers, deepseq, text, transformers |
| text-short | 0.1.6.1 | hackage | binary, bytestring, deepseq, hashable, text |
| base16 | 1.0 | hackage | bytestring, deepseq, primitive, text, text-short |
| base64 | 1.0 | hackage | bytestring, deepseq, text, text-short |
| crypton-asn1-types | 0.4.1 | hackage | base16, bytestring, time-hourglass |
| crypton-asn1-encoding | 0.10.0 | hackage | bytestring, crypton-asn1-types, time-hourglass |
| crypton-asn1-parse | 0.10.0 | hackage | bytestring, crypton-asn1-types |
| crypton-pem | 0.3.0 | hackage | base64, bytestring, deepseq, text |
| crypton-x509 | 1.8.0 | hackage | bytestring, containers, crypton, crypton-asn1-encoding, crypton-asn1-parse, crypton-asn1-types, crypton-pem, memory, time-hourglass, transformers |
| these | 1.2.1 | hackage | assoc, binary, deepseq, hashable |
| strict | 0.5.1 | hackage | assoc, binary, bytestring, deepseq, hashable, text, these, transformers |
| time-compat | 1.9.9 | hackage | base-orphans, deepseq, hashable, time |
| text-iso8601 | 0.1.1.2 | hackage | integer-conversion, text, time, time-compat |
| transformers-compat | 0.7.2 | hackage | transformers |
| unix | 2.8.8.0 | hackage | bytestring, filepath, os-string, time |
| directory-ospath-streaming | 0.3 | hackage | atomic-counter, deepseq, filepath, os-string, unix |
| file-io | 0.1.6 | hackage | bytestring, deepseq, filepath, os-string, unix |
| directory | 1.3.10.1 | hackage | file-io, filepath, os-string, time, unix |
| Cabal-syntax | 3.14.2.0 | hackage | array, binary, bytestring, containers, deepseq, directory, filepath, mtl, parsec, pretty, text, time, transformers |
| crypton-x509-store | 1.8.0 | hackage | bytestring, containers, crypton, crypton-asn1-encoding, crypton-asn1-types, crypton-pem, crypton-x509, directory, filepath, mtl, unix |
| network | 3.2.9.0 | hackage | bytestring, deepseq, directory, stm |
| crypton-socks | 0.6.2 | hackage | bytestring, cereal, network |
| iproute | 1.7.15 | hackage | appar, byteorder, bytestring, containers, network |
| crypton-x509-validation | 1.8.0 | hackage | bytestring, containers, crypton, crypton-asn1-encoding, crypton-asn1-types, crypton-pem, crypton-x509, crypton-x509-store, data-default, iproute, memory, mtl, time-hourglass |
| process | 1.6.26.1 | hackage | deepseq, directory, filepath, os-string, unix |
| Cabal | 3.14.2.0 | hackage | Cabal-syntax, array, bytestring, containers, deepseq, directory, filepath, mtl, parsec, pretty, process, time, transformers, unix |
| crypton-x509-system | 1.8.0 | hackage | bytestring, containers, crypton-pem, crypton-x509, crypton-x509-store, directory, filepath, mtl, process |
| haskeline | 0.8.4.1 | hackage | bytestring, containers, directory, exceptions, filepath, process, stm, terminfo, transformers, unix |
| optparse-applicative | 0.18.1.0 | hackage | prettyprinter, prettyprinter-ansi-terminal, process, text, transformers, transformers-compat |
| tar | 0.6.4.0 | hackage | array, bytestring, containers, deepseq, directory, directory-ospath-streaming, file-io, filepath, os-string, time, transformers |
| unix-time | 0.4.17 | hackage | binary, bytestring, old-time |
| unordered-containers | 0.2.21 | hackage | deepseq, hashable |
| async | 2.2.6 | hackage | hashable, stm, unordered-containers |
| semigroupoids | 6.0.2 | hackage | base-orphans, bifunctors, comonad, containers, contravariant, hashable, tagged, transformers, transformers-compat, unordered-containers |
| uuid-types | 1.0.6.1 | hackage | binary, bytestring, deepseq, hashable, random, text |
| vector-stream | 0.1.0.1 | hackage |  |
| vector | 0.13.2.0 | hackage | deepseq, primitive, vector-stream |
| indexed-traversable-instances | 0.1.2.1 | hackage | OneTuple, indexed-traversable, tagged, unordered-containers, vector |
| semialign | 1.4 | hackage | containers, hashable, indexed-traversable, indexed-traversable-instances, semigroupoids, tagged, these, unordered-containers, vector |
| serialise | 0.2.6.1 | hackage | array, bytestring, cborg, containers, half, hashable, primitive, strict, text, these, time, unordered-containers, vector |
| witherable | 0.5 | hackage | base-orphans, containers, hashable, indexed-traversable, indexed-traversable-instances, transformers, unordered-containers, vector |
| aeson | 2.2.5.1 | hackage | OneTuple, QuickCheck, bytestring, character-ps, containers, data-fix, deepseq, dlist, exceptions, hashable, indexed-traversable, integer-conversion, integer-logarithms, network-uri, primitive, scientific, semialign, strict, tagged, text, text-iso8601, text-short, th-abstraction, these, time, time-compat, unordered-containers, uuid-types, vector, witherable |
| zlib | 0.7.1.1 | hackage | bytestring |
| streaming-commons | 0.2.3.1 | hackage | array, async, bytestring, directory, network, process, random, stm, text, transformers, unix, zlib |
| http-client | 0.7.19 | hackage | array, async, base64-bytestring, blaze-builder, bytestring, case-insensitive, containers, cookie, deepseq, exceptions, filepath, http-types, iproute, mime-types, network, network-uri, random, stm, streaming-commons, text, time, transformers |
| tls | 2.2.2 | hackage | base16-bytestring, bytestring, cereal, crypton, crypton-asn1-encoding, crypton-asn1-types, crypton-x509, crypton-x509-store, crypton-x509-validation, data-default, ech-config, hpke, memory, mtl, network, random, serialise, transformers, unix-time, zlib |
| crypton-connection | 0.4.5 | hackage | bytestring, containers, crypton-socks, crypton-x509-store, crypton-x509-system, data-default, network, tls |
| http-client-tls | 0.3.6.4 | hackage | bytestring, case-insensitive, containers, crypton, crypton-connection, data-default, exceptions, http-client, http-types, memory, network, network-uri, text, tls, transformers |
| aihc-hackage | 0.1.0.0 | local:tooling/aihc-hackage | Cabal, Cabal-syntax, bytestring, containers, directory, filepath, http-client, http-client-tls, http-types, tar, text, time, zlib |
| aihc-package-plan | 0.1.0.0 | local:tooling/aihc-package-plan | Cabal-syntax, aeson, aihc-cpp, aihc-hackage, aihc-parser, bytestring, containers, cryptohash-sha256, directory, filepath, text, time, transformers |
| aihc | 0.1.0.0 | local:bin/aihc | Cabal-syntax, aeson, aihc-cpp, aihc-hackage, aihc-package-plan, aihc-parser, aihc-resolve, aihc-tc, array, async, binary, bytestring, containers, cryptohash-sha256, deepseq, directory, filepath, haskeline, libffi, megaparsec, optparse-applicative, prettyprinter, primitive, process, stm, text, transformers, unix, vector |
