# pandoc-lua-marshal

[![GitHub CI][]][1]
[![Hackage][]][2]
[![Stackage Lts][]][3]
[![Stackage Nightly][]][4]
[![MIT license]][5]

Use pandoc types in Lua.

[GitHub CI]: https://img.shields.io/github/actions/workflow/status/pandoc/pandoc-lua-marshal/ci.yml?branch=main&logo=github
[1]: https://github.com/tarleb/pandoc-lua-marshal/actions
[Hackage]: https://img.shields.io/hackage/v/pandoc-lua-marshal.svg?logo=haskell
[2]: https://hackage.haskell.org/package/pandoc-lua-marshal
[Stackage Lts]: http://stackage.org/package/pandoc-lua-marshal/badge/lts
[3]: https://stackage.org/lts/package/pandoc-lua-marshal
[Stackage Nightly]: https://stackage.org/package/pandoc-lua-marshal/badge/nightly
[4]: https://stackage.org/nightly/package/pandoc-lua-marshal
[MIT license]: https://img.shields.io/badge/license-MIT-blue.svg
[5]: LICENSE

## Description

This package provides functions to marshal and unmarshal pandoc
document types to and from Lua.

The values of most types are pushed to pandoc as "userdata"
objects that wrap a stable pointer to the Haskell value; these
objects come with methods to access and modify their properties.

Sequences are pushed as normal Lua tables, but are augmented with
convenience functions.
