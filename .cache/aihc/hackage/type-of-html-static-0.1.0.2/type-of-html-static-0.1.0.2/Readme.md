# Type of html static

This little companion library of
[https://hackage.haskell.org/package/type-of-html](Type of html)
provides just two little functions:
`static :: Document a => a -> Q Exp`.
Using this template haskell function on any part of your html
document will escape, render and lift to a Symbol at compile time.

It increases performance a lot by avoiding any runtime computation.
By producing Proxy Symbol, it will fuse at compiletime with adjacent
elements in your document.

`optimize :: Q Exp -> Q Exp` takes a quoted definition of a html
document and tries to convert all literals to compile time escaped
Proxy Symbol.  It is less powerfull than `static` but more convenient.
