# atom-conduit

This [Haskell][hsk] library implements a streaming parser/renderer for the [Atom 1.0 syndication format][atom], based on [conduit][cdt]s.

Parsers are as much lenient as possible. E.g. unexpected tags are simply ignored.


[atom]: http://tools.ietf.org/html/rfc4287
[cdt]: https://hackage.haskell.org/package/conduit
[hsk]: https://haskell.org
