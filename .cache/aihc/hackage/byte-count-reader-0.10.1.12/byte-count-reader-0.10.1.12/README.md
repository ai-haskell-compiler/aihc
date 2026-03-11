# Byte Count Reader
-------------------

[![Build Status](https://app.travis-ci.com/danielrolls/byte-count-reader.svg?branch=master)](https://app.travis-ci.com/danielrolls/byte-count-reader)
[![Hackage](https://img.shields.io/hackage/v/byte-count-reader.svg)][hackage]
[![Stackage](https://www.stackage.org/package/byte-count-reader/badge/nightly?label=stackage)](https://www.stackage.org/package/byte-count-reader)

This library is for reading strings describing a number of bytes like 2Kb and 0.5 MiB.

The following are examples of strings that are accepted:
- 1b
- 2 KiB
- 3.5 MB
- 10 &nbsp;&nbsp; gib
- 100tb

[hackage]: http://hackage.haskell.org/package/byte-count-reader "Hackage"
