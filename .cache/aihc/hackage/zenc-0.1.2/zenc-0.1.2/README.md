# Text.Encoding.Z

## The Z-encoding

`zEncodeString` is a name-encoding and decoding function. It encodes any ascii
string into a string that is acceptable as a C name. This is code was originally
part of GHC and used right before emitting a symbol name into the compiled C or
asm code.  This library was created as this encoding is useful when working with
GHC compiled code or generally when C-compatible name mangling is desired.

`zDecodeString` is the inverse operation.
