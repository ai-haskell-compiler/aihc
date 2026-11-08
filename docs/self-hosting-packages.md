# Self-hosting package list

AIHC is self-hosting when it can compile itself. The table below gives every
package that aihc plans for the `aihc` executable, and `aihc` itself last.
A package comes after all of its dependencies.

`scripts/update-self-hosting-packages.sh` writes this file with
`aihc plan bin/aihc --executable aihc`.
The plan uses the default flags of aihc and the flags of `cabal.project`.
The weekly
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
| character-ps | 0.1 | hackage:0 |  |
| colour | 2.3.7 | hackage:0 |  |
| ansi-terminal-types | 1.1.3 | hackage:0 | colour |
| ansi-terminal | 1.1.5 | hackage:0 | ansi-terminal-types, colour |
| deepseq | 1.5.2.0 | hackage:0 |  |
| bytestring | 0.12.2.0 | hackage:1 | deepseq |
| appar | 0.1.8 | hackage:0 | bytestring |
| base16-bytestring | 1.0.2.0 | hackage:1 | bytestring |
| base64-bytestring | 1.2.1.0 | hackage:1 | bytestring |
| containers | 0.7 | hackage:0 | array, deepseq |
| binary | 0.8.9.3 | hackage:0 | array, bytestring, containers |
| cereal | 0.5.8.3 | hackage:0 | array, bytestring, containers |
| commutative-semigroups | 0.2.0.2 | hackage:0 | containers |
| cryptohash-md5 | 0.11.101.0 | hackage:6 | bytestring |
| cryptohash-sha256 | 0.11.102.1 | hackage:6 | bytestring |
| data-default | 0.8.0.2 | hackage:0 | containers |
| data-default-class | 0.2.0.0 | hackage:0 | data-default |
| dlist | 1.0 | hackage:2 | deepseq |
| ghc-bignum | 1.3 | hackage:0 |  |
| groups | 0.5.3 | hackage:0 |  |
| half | 0.3.3 | hackage:3 | binary, deepseq |
| integer-gmp | 1.1 | hackage:0 | ghc-bignum |
| integer-logarithms | 1.0.5 | hackage:0 | array |
| network-byte-order | 0.1.9 | hackage:0 | bytestring |
| parser-combinators | 1.3.1 | hackage:0 |  |
| primes | 0.2.1.0 | hackage:0 |  |
| ram | 0.22.1 | hackage:0 | bytestring, deepseq |
| splitmix | 0.1.3.2 | hackage:0 | deepseq |
| stm | 2.5.3.1 | hackage:1 | array |
| tagged | 0.8.11 | hackage:0 | deepseq |
| terminal-size | 0.3.4 | hackage:1 |  |
| text | 2.1.4 | hackage:0 | array, binary, bytestring, deepseq |
| blaze-builder | 0.4.4.1 | hackage:0 | bytestring, text |
| mime-types | 0.1.2.2 | hackage:0 | bytestring, containers, text |
| prettyprinter | 1.7.2 | hackage:0 | text |
| prettyprinter-ansi-terminal | 1.1.4 | hackage:0 | ansi-terminal, prettyprinter, text |
| th-abstraction | 0.7.2.0 | hackage:0 | containers |
| th-compat | 0.1.7 | hackage:0 |  |
| time | 1.14 | hackage:1 | deepseq |
| cookie | 0.5.1 | hackage:0 | bytestring, data-default-class, deepseq, text, time |
| time-hourglass | 0.3.0 | hackage:0 | deepseq |
| transformers | 0.6.3.0 | hackage:2 |  |
| StateVar | 1.2.2 | hackage:0 | stm, transformers |
| contravariant | 1.5.6 | hackage:0 | StateVar, transformers |
| distributive | 0.6.3 | hackage:0 | tagged, transformers |
| indexed-traversable | 0.1.5 | hackage:0 | array, containers, transformers |
| comonad | 5.0.10 | hackage:0 | containers, distributive, indexed-traversable, tagged, transformers |
| bifunctors | 5.6.3 | hackage:1 | assoc, comonad, containers, tagged, th-abstraction |
| mtl | 2.3.2 | hackage:1 | transformers |
| exceptions | 0.10.12 | hackage:2 | mtl, stm, transformers |
| os-string | 2.0.11 | hackage:0 | bytestring, deepseq, exceptions |
| filepath | 1.5.5.0 | hackage:0 | bytestring, deepseq, exceptions, os-string |
| aihc-cpp | 2.0.0.0 | hackage:0 | bytestring, containers, deepseq, filepath, text |
| ech-config | 0.0.2 | hackage:0 | base16-bytestring, bytestring, filepath, network-byte-order |
| hashable | 1.5.1.0 | hackage:0 | bytestring, containers, deepseq, filepath, os-string, text |
| case-insensitive | 1.2.2.0 | hackage:0 | bytestring, deepseq, hashable, text |
| data-fix | 0.3.4 | hackage:2 | deepseq, hashable |
| http-types | 0.12.6 | hackage:0 | bytestring, case-insensitive, text |
| parsec | 3.1.18.0 | hackage:1 | bytestring, mtl, text |
| network-uri | 2.6.4.2 | hackage:1 | deepseq, parsec, th-compat |
| primitive | 0.9.1.0 | hackage:1 | deepseq, transformers |
| cborg | 0.2.10.0 | hackage:3 | array, bytestring, containers, deepseq, ghc-bignum, half, primitive, text |
| integer-conversion | 0.1.1 | hackage:2 | bytestring, primitive, text |
| random | 1.3.1 | hackage:0 | bytestring, deepseq, mtl, splitmix, transformers |
| QuickCheck | 2.18.0.0 | hackage:0 | containers, deepseq, random, splitmix, transformers |
| scientific | 0.3.10.0 | hackage:0 | binary, bytestring, containers, deepseq, hashable, integer-logarithms, primitive, text |
| megaparsec | 9.8.3 | hackage:0 | array, bytestring, case-insensitive, containers, deepseq, mtl, parser-combinators, scientific, text, transformers |
| aihc-cabal-syntax | 2.0.0.0 | hackage:0 | bytestring, containers, megaparsec, parser-combinators, text |
| aihc-parser | 5.0.0.0 | hackage:0 | bytestring, containers, deepseq, megaparsec, prettyprinter, text |
| text-short | 0.1.6.1 | hackage:0 | binary, bytestring, deepseq, hashable, text |
| base16 | 2.0 | hackage:0 | bytestring, deepseq, primitive, text, text-short |
| base64 | 2.1 | hackage:0 | bytestring, deepseq, text, text-short |
| crypton | 2.2.6 | hackage:1 | base16, bytestring, deepseq, integer-gmp, primitive, ram, text |
| crypton-asn1-types | 0.4.1 | hackage:0 | base16, bytestring, time-hourglass |
| crypton-asn1-encoding | 0.10.0 | hackage:0 | bytestring, crypton-asn1-types, time-hourglass |
| crypton-asn1-parse | 0.10.0 | hackage:0 | bytestring, crypton-asn1-types |
| crypton-pem | 0.3.0 | hackage:0 | base64, bytestring, deepseq, text |
| crypton-x509 | 1.10.1 | hackage:0 | bytestring, containers, crypton, crypton-asn1-encoding, crypton-asn1-parse, crypton-asn1-types, crypton-pem, ram, time-hourglass, transformers |
| hpke | 0.4.0 | hackage:0 | base16-bytestring, bytestring, crypton, ram |
| these | 1.2.1 | hackage:3 | assoc, binary, deepseq, hashable |
| strict | 0.5.1 | hackage:1 | assoc, binary, bytestring, deepseq, hashable, text, these, transformers |
| time-compat | 1.9.9 | hackage:1 | base-orphans, deepseq, hashable, time |
| text-iso8601 | 0.1.1.2 | hackage:0 | integer-conversion, text, time, time-compat |
| transformers-compat | 0.7.2 | hackage:0 | transformers |
| unix | 2.8.8.0 | hackage:1 | bytestring, filepath, os-string, time |
| directory-ospath-streaming | 0.3 | hackage:1 | atomic-counter, deepseq, filepath, os-string, unix |
| file-io | 0.1.6 | hackage:0 | bytestring, deepseq, filepath, os-string, unix |
| directory | 1.3.11.0 | hackage:0 | file-io, filepath, os-string, time, unix |
| aihc-hackage | 0.1.0.0 | local:tooling/aihc-hackage | aihc-cabal-syntax, bytestring, containers, directory, filepath, text |
| crypton-x509-store | 1.10.0 | hackage:0 | bytestring, containers, crypton, crypton-asn1-encoding, crypton-asn1-types, crypton-pem, crypton-x509, directory, filepath, mtl, unix |
| network | 3.2.12.0 | hackage:0 | bytestring, deepseq, directory, stm |
| crypton-socks | 0.6.2 | hackage:0 | bytestring, cereal, network |
| iproute | 1.8.0 | hackage:0 | appar, bytestring, network |
| crypton-x509-validation | 1.10.1 | hackage:0 | bytestring, containers, crypton, crypton-asn1-encoding, crypton-asn1-types, crypton-pem, crypton-x509, crypton-x509-store, data-default, iproute, mtl, ram, time-hourglass |
| process | 1.6.30.0 | hackage:0 | deepseq, directory, filepath, os-string, unix |
| crypton-x509-system | 1.10.0 | hackage:0 | bytestring, containers, crypton-pem, crypton-x509, crypton-x509-store, directory, filepath, mtl, process |
| optparse-applicative | 0.18.1.0 | hackage:1 | prettyprinter, prettyprinter-ansi-terminal, process, text, transformers, transformers-compat |
| tar | 0.6.4.0 | hackage:4 | array, bytestring, containers, deepseq, directory, directory-ospath-streaming, file-io, filepath, os-string, time, transformers |
| unix-time | 0.6.0 | hackage:0 | binary, bytestring |
| unordered-containers | 0.2.21 | hackage:2 | deepseq, hashable |
| async | 2.2.6 | hackage:0 | hashable, stm, unordered-containers |
| semigroupoids | 6.0.2 | hackage:1 | base-orphans, bifunctors, comonad, containers, contravariant, hashable, tagged, transformers, transformers-compat, unordered-containers |
| uuid-types | 1.0.6.1 | hackage:0 | binary, bytestring, deepseq, hashable, random, text |
| vector-stream | 0.1.0.1 | hackage:4 |  |
| vector | 0.13.2.0 | hackage:5 | deepseq, primitive, vector-stream |
| indexed-traversable-instances | 0.1.2.1 | hackage:0 | OneTuple, indexed-traversable, tagged, unordered-containers, vector |
| monoid-subclasses | 1.2.6.1 | hackage:0 | bytestring, commutative-semigroups, containers, primes, text, vector |
| monoidmap-internal | 0.5.1.0 | hackage:0 | containers, deepseq, monoid-subclasses |
| semialign | 1.4 | hackage:0 | containers, hashable, indexed-traversable, indexed-traversable-instances, semigroupoids, tagged, these, unordered-containers, vector |
| serialise | 0.2.6.1 | hackage:5 | array, bytestring, cborg, containers, half, hashable, primitive, strict, text, these, time, unordered-containers, vector |
| wherefrom-compat | 0.2.0.0 | hackage:5 |  |
| nothunks | 0.3.2 | hackage:0 | bytestring, containers, stm, text, time, vector, wherefrom-compat |
| monoidmap | 0.1.3.1 | hackage:0 | containers, deepseq, groups, monoid-subclasses, monoidmap-internal, nothunks |
| aihc-resolve | 0.1.0.0 | local:components/aihc-resolve | aihc-parser, containers, deepseq, monoidmap, text |
| aihc-tc | 0.1.0.0 | local:components/aihc-tc | aihc-parser, aihc-resolve, containers, deepseq, text, transformers |
| witherable | 0.5 | hackage:2 | base-orphans, containers, hashable, indexed-traversable, indexed-traversable-instances, transformers, unordered-containers, vector |
| aeson | 2.2.5.1 | hackage:0 | OneTuple, QuickCheck, bytestring, character-ps, containers, data-fix, deepseq, dlist, exceptions, hashable, indexed-traversable, integer-conversion, integer-logarithms, network-uri, primitive, scientific, semialign, strict, tagged, text, text-iso8601, text-short, th-abstraction, these, time, time-compat, unordered-containers, uuid-types, vector, witherable |
| aihc-package-plan | 0.1.0.0 | local:tooling/aihc-package-plan | aeson, aihc-cabal-syntax, aihc-cpp, aihc-hackage, aihc-parser, bytestring, containers, cryptohash-sha256, deepseq, directory, filepath, text, time, transformers |
| zlib | 0.7.1.1 | hackage:1 | bytestring |
| streaming-commons | 0.2.3.1 | hackage:0 | array, async, bytestring, directory, network, process, random, stm, text, transformers, unix, zlib |
| http-client | 0.7.19 | hackage:0 | array, async, base64-bytestring, blaze-builder, bytestring, case-insensitive, containers, cookie, deepseq, exceptions, filepath, http-types, iproute, mime-types, network, network-uri, random, stm, streaming-commons, text, time, transformers |
| tls | 2.5.1 | hackage:0 | base16-bytestring, bytestring, cereal, crypton, crypton-asn1-encoding, crypton-asn1-types, crypton-x509, crypton-x509-store, crypton-x509-validation, data-default, ech-config, hpke, mtl, network, ram, random, serialise, transformers, unix-time, zlib |
| crypton-connection | 0.4.6 | hackage:3 | bytestring, containers, crypton-socks, crypton-x509-store, crypton-x509-system, data-default, network, tls |
| http-client-tls | 0.4.0 | hackage:0 | base16, bytestring, case-insensitive, containers, cryptohash-md5, crypton-connection, data-default, exceptions, http-client, http-types, network, network-uri, text, tls, transformers |
| aihc-http | 0.1.0.0 | local:tooling/aihc-http | bytestring, http-client, http-client-tls, http-types |
| aihc-hackage-fetch | 0.1.0.0 | local:tooling/aihc-hackage-fetch | aeson, aihc-hackage, aihc-http, bytestring, containers, directory, filepath, tar, time, zlib |
| aihc | 0.1.0.0 | local:bin/aihc | aihc-cabal-syntax, aihc-cpp, aihc-hackage, aihc-hackage-fetch, aihc-package-plan, aihc-parser, aihc-resolve, aihc-tc, array, async, binary, bytestring, containers, cryptohash-sha256, deepseq, directory, filepath, megaparsec, monoidmap, optparse-applicative, prettyprinter, primitive, process, stm, terminal-size, text, transformers, unix, vector |
