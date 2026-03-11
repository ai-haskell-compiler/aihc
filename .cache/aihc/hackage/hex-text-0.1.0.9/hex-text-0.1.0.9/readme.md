# hex-text

`hex-text` is a small library for converting between `ByteString`s and their
representations as hexidecimal numbers encoded as `Text`.

## Motivation

When using Stripe for payments, Stripe sends a signature as a hexidecimal `Text`
value. The `cryptonite` package can be used to verify the signature, but it
requires `ByteString` values, not `Text`.

## Example usage

A `ByteString` is a list of bytes. A byte is a number between 0 and 255,
represented by the `Word8` type. In a fixed-width hexidecimal representation,
the lowest byte 0 is represented by the hex string `00`, and the greatest byte
255 is represented by the hex string `ff`. So, for example, the `ByteString`
consisting of bytes \[ 1, 2, 3, 253, 254, 255 \] is represented as
`010203fdfeff`.

```haskell
λ> import Text.Hex (encodeHex)
λ> import Data.ByteString (pack)

λ> (encodeHex . pack) [1, 2, 3, 253, 254, 255]
"010203fdfeff"
```
