0.2.0.4
=======

Added `custom-setup` to `.cabal` file

0.2.0.3
=======

Add a `PackageImport` to avoid conflicting module names for `Crypto.Hash.SHA1`
introduced by the `cryptohash-sha1` package.

0.2.0.0
=======

In order to support dynamic linking, `executableHash` and
`injectedExecutableHash` now need to be run in TH splices.  Before
this change, the library was assuming that it would be statically
linked, such that the file-embed dummy bytestring would be present in
the executable.

See [this github issue](https://github.com/fpco/executable-hash/issues/1).

0.1.1.1
=======

First public version
