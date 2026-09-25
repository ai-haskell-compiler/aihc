# Self-hosting package list

AIHC is self-hosting when it can compile itself. The table below gives every
package that aihc plans for the `aihc` executable, and `aihc` itself last.
A package comes after all of its dependencies.

`scripts/update-self-hosting-packages.sh` writes this file with
`aihc plan bin/aihc --executable aihc`. The weekly
[Generated Reports](../.github/workflows/generated-reports-update.yml)
workflow runs the script, then compiles each package with
`scripts/self-hosting-progress.sh`. The workflow writes the result to the
"Self-compile" row of the README.

The table does not include the aihc core libraries, because they come with
aihc. The "Source" column is `hackage:REVISION` for a Hackage release at a
cabal file revision, or `local:PATH` for a package in this repository.

## Packages

| Package | Version | Source | Dependencies |
| ------- | ------- | ------ | ------------ |
| OneTuple | 0.4.3 | hackage:0 |  |
| array | 0.5.8.0 | hackage:2 |  |
| assoc | 1.1.1 | hackage:2 |  |
| atomic-counter | 0.1.2.4 | hackage:0 |  |
| base-orphans | 0.9.4 | hackage:0 |  |
| basement | 0.0.16 | hackage:0 |  |
| byteorder | 1.0.4 | hackage:0 |  |
| character-ps | 0.1 | hackage:0 |  |
| colour | 2.3.7 | hackage:0 |  |
| ansi-terminal-types | 1.1.3 | hackage:0 | colour |
| ansi-terminal | 1.1.5 | hackage:0 | ansi-terminal-types, colour |
| deepseq | 1.5.2.0 | hackage:0 |  |
| bytestring | 0.12.2.0 | hackage:1 | deepseq |
| appar | 0.1.8 | hackage:0 | bytestring |
| base64-bytestring | 1.2.1.0 | hackage:1 | bytestring |
| containers | 0.8 | hackage:0 | array, deepseq |
| binary | 0.8.9.3 | hackage:0 | array, bytestring, containers |
| cereal | 0.5.8.3 | hackage:0 | array, bytestring, containers |
| cryptohash-sha256 | 0.11.102.1 | hackage:6 | bytestring |
| data-default | 0.8.0.2 | hackage:0 | containers |
| data-default-class | 0.2.0.0 | hackage:0 | data-default |
| dlist | 1.0 | hackage:2 | deepseq |
| ghc-bignum | 1.3 | hackage:0 |  |
| hourglass | 0.2.12 | hackage:1 | deepseq |
| integer-gmp | 1.1 | hackage:0 | ghc-bignum |
| integer-logarithms | 1.0.5 | hackage:0 | array |
| libffi | 0.2.1 | hackage:0 | bytestring |
| memory | 0.18.0 | hackage:1 | basement, bytestring, deepseq |
| asn1-types | 0.3.4 | hackage:0 | bytestring, hourglass, memory |
| asn1-encoding | 0.9.6 | hackage:2 | asn1-types, bytestring, hourglass |
| asn1-parse | 0.9.5 | hackage:0 | asn1-encoding, asn1-types, bytestring |
| crypton | 1.0.6 | hackage:0 | basement, bytestring, deepseq, integer-gmp, memory |
| parser-combinators | 1.3.1 | hackage:0 |  |
| pem | 0.2.4 | hackage:0 | basement, bytestring, memory |
| pretty | 1.1.3.6 | hackage:0 | deepseq |
| splitmix | 0.1.3.2 | hackage:0 | deepseq |
| stm | 2.5.3.1 | hackage:1 | array |
| tagged | 0.8.10 | hackage:1 | deepseq |
| terminfo | 0.4.1.7 | hackage:0 |  |
| text | 2.1.4 | hackage:0 | array, binary, bytestring, deepseq |
| blaze-builder | 0.4.4.1 | hackage:0 | bytestring, text |
| mime-types | 0.1.2.2 | hackage:0 | bytestring, containers, text |
| prettyprinter | 1.7.2 | hackage:0 | text |
| prettyprinter-ansi-terminal | 1.1.4 | hackage:0 | ansi-terminal, prettyprinter, text |
| th-abstraction | 0.7.2.0 | hackage:0 | containers |
| th-compat | 0.1.7 | hackage:0 |  |
| time | 1.14 | hackage:1 | deepseq |
| cookie | 0.5.1 | hackage:0 | bytestring, data-default-class, deepseq, text, time |
| transformers | 0.6.3.0 | hackage:2 |  |
| StateVar | 1.2.2 | hackage:0 | stm, transformers |
| contravariant | 1.5.6 | hackage:0 | StateVar, transformers |
| crypton-x509 | 1.7.7 | hackage:1 | asn1-encoding, asn1-parse, asn1-types, bytestring, containers, crypton, hourglass, memory, pem, transformers |
| distributive | 0.6.3 | hackage:0 | tagged, transformers |
| indexed-traversable | 0.1.5 | hackage:0 | array, containers, transformers |
| comonad | 5.0.10 | hackage:0 | containers, distributive, indexed-traversable, tagged, transformers |
| bifunctors | 5.6.3 | hackage:1 | assoc, comonad, containers, tagged, th-abstraction |
| mtl | 2.3.2 | hackage:1 | transformers |
| exceptions | 0.10.12 | hackage:2 | mtl, stm, transformers |
| filepath | 1.4.301.0 | hackage:1 | bytestring, deepseq, exceptions |
| aihc-cpp | 2.0.0.0 | hackage:0 | bytestring, containers, deepseq, filepath, text |
| os-string | 2.0.11 | hackage:0 | bytestring, deepseq, exceptions |
| hashable | 1.5.1.0 | hackage:0 | bytestring, containers, deepseq, filepath, os-string, text |
| case-insensitive | 1.2.1.0 | hackage:0 | bytestring, deepseq, hashable, text |
| data-fix | 0.3.4 | hackage:2 | deepseq, hashable |
| http-types | 0.12.6 | hackage:0 | bytestring, case-insensitive, text |
| parsec | 3.1.18.0 | hackage:1 | bytestring, mtl, text |
| network-uri | 2.6.4.2 | hackage:1 | deepseq, parsec, th-compat |
| primitive | 0.9.1.0 | hackage:1 | deepseq, transformers |
| integer-conversion | 0.1.1 | hackage:2 | bytestring, primitive, text |
| random | 1.3.1 | hackage:0 | bytestring, deepseq, mtl, splitmix, transformers |
| QuickCheck | 2.18.0.0 | hackage:0 | containers, deepseq, random, splitmix, transformers |
| scientific | 0.3.8.1 | hackage:0 | binary, bytestring, containers, deepseq, hashable, integer-logarithms, primitive, text |
| megaparsec | 9.8.2 | hackage:0 | array, bytestring, case-insensitive, containers, deepseq, mtl, parser-combinators, scientific, text, transformers |
| aihc-parser | 4.0.0.0 | hackage:0 | bytestring, containers, deepseq, megaparsec, prettyprinter, text |
| aihc-resolve | 0.1.0.0 | local:components/aihc-resolve | aihc-parser, containers, deepseq, text |
| aihc-tc | 0.1.0.0 | local:components/aihc-tc | aihc-parser, aihc-resolve, containers, deepseq, text, transformers |
| text-short | 0.1.6.1 | hackage:0 | binary, bytestring, deepseq, hashable, text |
| these | 1.2.1 | hackage:3 | assoc, binary, deepseq, hashable |
| strict | 0.5.1 | hackage:1 | assoc, binary, bytestring, deepseq, hashable, text, these, transformers |
| time-compat | 1.9.9 | hackage:1 | base-orphans, deepseq, hashable, time |
| text-iso8601 | 0.1.1.2 | hackage:0 | integer-conversion, text, time, time-compat |
| transformers-compat | 0.7.2 | hackage:0 | transformers |
| unix | 2.8.8.0 | hackage:1 | bytestring, filepath, time |
| directory-ospath-streaming | 0.3 | hackage:1 | atomic-counter, deepseq, filepath, unix |
| file-io | 0.1.6 | hackage:0 | bytestring, deepseq, filepath, unix |
| directory | 1.3.10.1 | hackage:0 | file-io, filepath, time, unix |
| Cabal-syntax | 3.16.1.0 | hackage:2 | array, binary, bytestring, containers, deepseq, directory, filepath, mtl, parsec, pretty, text, time, transformers |
| crypton-x509-store | 1.6.14 | hackage:1 | asn1-encoding, asn1-types, bytestring, containers, crypton, crypton-x509, directory, filepath, mtl, pem, unix |
| network | 3.2.9.0 | hackage:0 | bytestring, deepseq, directory, stm |
| crypton-socks | 0.6.2 | hackage:0 | bytestring, cereal, network |
| iproute | 1.7.15 | hackage:0 | appar, byteorder, bytestring, containers, network |
| crypton-x509-validation | 1.6.14 | hackage:1 | asn1-encoding, asn1-types, bytestring, containers, crypton, crypton-x509, crypton-x509-store, data-default, hourglass, iproute, memory, mtl, pem |
| process | 1.6.30.0 | hackage:0 | deepseq, directory, filepath, unix |
| Cabal | 3.16.1.0 | hackage:1 | Cabal-syntax, array, bytestring, containers, deepseq, directory, filepath, mtl, parsec, pretty, process, time, transformers, unix |
| crypton-x509-system | 1.6.8 | hackage:0 | bytestring, containers, crypton-x509, crypton-x509-store, directory, filepath, mtl, pem, process |
| haskeline | 0.8.5.0 | hackage:0 | bytestring, containers, directory, exceptions, filepath, process, stm, terminfo, transformers, unix |
| optparse-applicative | 0.18.1.0 | hackage:1 | prettyprinter, prettyprinter-ansi-terminal, process, text, transformers, transformers-compat |
| tar | 0.6.4.0 | hackage:4 | array, bytestring, containers, deepseq, directory, directory-ospath-streaming, file-io, filepath, os-string, time, transformers |
| unix-time | 0.5.0 | hackage:0 | binary, bytestring |
| unordered-containers | 0.2.21 | hackage:2 | deepseq, hashable |
| async | 2.2.6 | hackage:0 | hashable, stm, unordered-containers |
| semigroupoids | 6.0.2 | hackage:1 | base-orphans, bifunctors, comonad, containers, contravariant, hashable, tagged, transformers, transformers-compat, unordered-containers |
| tls | 1.9.0 | hackage:1 | asn1-encoding, asn1-types, async, bytestring, cereal, crypton, crypton-x509, crypton-x509-store, crypton-x509-validation, data-default-class, memory, mtl, network, transformers, unix-time |
| crypton-connection | 0.4.5 | hackage:1 | bytestring, containers, crypton-socks, crypton-x509-store, crypton-x509-system, data-default, network, tls |
| uuid-types | 1.0.6.1 | hackage:0 | binary, bytestring, deepseq, hashable, random, text |
| vector-stream | 0.1.0.1 | hackage:4 |  |
| vector | 0.13.2.0 | hackage:5 | deepseq, primitive, vector-stream |
| indexed-traversable-instances | 0.1.2.1 | hackage:0 | OneTuple, indexed-traversable, tagged, unordered-containers, vector |
| semialign | 1.4 | hackage:0 | containers, hashable, indexed-traversable, indexed-traversable-instances, semigroupoids, tagged, these, unordered-containers, vector |
| witherable | 0.5 | hackage:2 | base-orphans, containers, hashable, indexed-traversable, indexed-traversable-instances, transformers, unordered-containers, vector |
| aeson | 2.2.5.1 | hackage:0 | OneTuple, QuickCheck, bytestring, character-ps, containers, data-fix, deepseq, dlist, exceptions, hashable, indexed-traversable, integer-conversion, integer-logarithms, network-uri, primitive, scientific, semialign, strict, tagged, text, text-iso8601, text-short, th-abstraction, these, time, time-compat, unordered-containers, uuid-types, vector, witherable |
| zlib | 0.7.1.1 | hackage:0 | bytestring |
| streaming-commons | 0.2.3.1 | hackage:0 | array, async, bytestring, directory, network, process, random, stm, text, transformers, unix, zlib |
| http-client | 0.7.19 | hackage:0 | array, async, base64-bytestring, blaze-builder, bytestring, case-insensitive, containers, cookie, deepseq, exceptions, filepath, http-types, iproute, mime-types, network, network-uri, random, stm, streaming-commons, text, time, transformers |
| http-client-tls | 0.3.6.4 | hackage:3 | bytestring, case-insensitive, containers, crypton, crypton-connection, data-default, exceptions, http-client, http-types, memory, network, network-uri, text, tls, transformers |
| aihc-hackage | 0.1.0.0 | local:tooling/aihc-hackage | Cabal, Cabal-syntax, bytestring, containers, directory, filepath, http-client, http-client-tls, http-types, tar, text, time, zlib |
| aihc-package-plan | 0.1.0.0 | local:tooling/aihc-package-plan | Cabal-syntax, aeson, aihc-cpp, aihc-hackage, aihc-parser, bytestring, containers, cryptohash-sha256, directory, filepath, text, time, transformers |
| aihc | 0.1.0.0 | local:bin/aihc | Cabal-syntax, aeson, aihc-cpp, aihc-hackage, aihc-package-plan, aihc-parser, aihc-resolve, aihc-tc, array, async, binary, bytestring, containers, cryptohash-sha256, deepseq, directory, filepath, haskeline, libffi, megaparsec, optparse-applicative, prettyprinter, primitive, process, stm, text, transformers, unix, vector |
