# Base32

![Build Status](https://github.com/emilypi/base32/workflows/ci/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/base32.svg)](https://hackage.haskell.org/package/base32)

Padded and unpadded base32 and base32hex encoding and decoding for `Text` and `ByteString` values.

For the companion optics and pattern synonyms, see [base32-lens](https://hackage.haskell.org/package/base32-lens).

### Summary

The following types are supported for both padded and unpadded std and extended hex alphabets:

- `Data.ByteString`
- `Data.ByteString.Lazy`
- `Data.ByteString.Short`
- `Data.Text`
- `Data.Text.Lazy`
- `Data.Text.Short`

Additionally this library has

- Better performance than `memory` for encode and decode 3-4x.
- Optics for handling more complex structures with Base32 representations via the `base32-lens` package
- Checks for both validity and correctness of Base32 and Base32hex encodings

There are no dependencies aside from those bundled with GHC, `text-short`.
