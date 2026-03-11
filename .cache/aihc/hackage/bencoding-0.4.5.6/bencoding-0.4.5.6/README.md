### Synopsis

[BEncode][bencode] is [JSON][json-ref]-like format used in bittorrent
protocol but might be used anywhere else.

### Description

This package implements fast seamless encoding/decoding to/from
bencode format for many native datatypes. To achieve
[more performance][cmp] we use
[bytestring builders][bytestring-builder] and hand optimized
[attoparsec][attoparsec] parser so this library is considered as
replacement for BEncode and AttoBencode packages.

#### Format

Bencode is pretty similar to JSON: it has dictionaries(JSON objects),
lists(JSON arrays), strings and integers. However bencode has a few
advantages:

* Compactness: no spaces in between any values — nor lists nor dicts
  nor anywhere else.
* Dictionaries always sorted lexicographically by the keys. This allow
  us to test data on equality without decoding from raw bytestring.
  Moreover this allow to hash encoded data (this property is heavily
  used by core bittorrent protocol).
* All strings prefixed with its length. This simplifies and speed up
  string parsing.

Hovewer there are some disadvantages comparing with JSON:

* Bencode is certainly less human readable.
* Bencode is rarely used, except bittorrent protocol of course.

### Documentation

For documentation see package [hackage][hackage] page.

### Build Status

[![build](https://github.com/sergv/bencoding/actions/workflows/ci.yaml/badge.svg)](https://github.com/sergv/bencoding/actions/workflows/ci.yaml)

### Maintainer <serg.foo@gmail.com>

Feel free to report bugs and suggestions via [issue tracker][issues]
or the mail.


[cmp]: http://htmlpreview.github.com/?https://raw.github.com/wiki/cobit/bencoding/comparison.html

[bencode]: https://wiki.theory.org/BitTorrentSpecification#Bencoding
[json-ref]: http://www.json.org/
[attoparsec]: http://hackage.haskell.org/package/attoparsec-0.10.4.0
[bytestring-builder]: http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Builder.html

[hackage]: http://hackage.haskell.org/package/bencoding
[issues]: https://github.com/sergv/bencoding/issues
