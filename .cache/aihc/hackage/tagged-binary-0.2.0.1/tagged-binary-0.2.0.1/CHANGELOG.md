0.2.0.1
-------
<https://github.com/mstksg/tagged-binary/releases/tag/v0.2.0.1>

*   Compatibility with *base-4.10* and ghc 8.2.

0.2.0.0
-------
<https://github.com/mstksg/tagged-binary/releases/tag/v0.2.0.0>

*   Removed `Default` instance on `TagFingerprint` in favor of just a normal
    `emptyTagFP` function.  This is a breaking change, and is the reason for
    the "major" version bump.
*   Fixed bug on decoding badly formatted binary due to unexpected laziness.
*   Removed dependency on *data-default* and *spoon* packages.
*   Removed upper version bounds for all dependencies except for *base*.

0.1.2.0
-------
<https://github.com/mstksg/tagged-binary/releases/tag/v0.1.2.0>

*   Tagged `ByteStrings` now lead with a "TagLead" byte sequence that
    signifies that a `Tagged` is coming.  For now, it is statically 0xfe 0xff,
    which are two bytes that are never found in any valid UTF-8 encoded
    string.
*   So that all `TagFingerprint`s should be of the same length,
    `TagFingerprint`s contain an `MD5Digest`, generated from the name of the
    type.  Its `Default` instance is the `MD5Digest` of an empty string.

0.1.0.0
-------
<https://github.com/mstksg/tagged-binary/releases/tag/v0.1.0.0>

*   Initial version.

