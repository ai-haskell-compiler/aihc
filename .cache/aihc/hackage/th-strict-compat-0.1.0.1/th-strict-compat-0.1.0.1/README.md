# th-strict-compat

Compatibility shim for Bang and Strict in Template Haskell.

[As the document says, as of template-haskell-2.11.0.0, `Strict` has been replaced by `Bang`.](http://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH-Syntax.html#t:Strict)  
So now `Strict` is the alias of `Bang`, but [`Strict`'s data constructors (http://hackage.haskell.org/package/template-haskell-2.10.0.0/docs/Language-Haskell-TH-Syntax.html#t:Strict)] are not provided anymore.  
So this package provides these values for both versions of template-haskell.

- `isStrict`
- `notStrict`
- `unpacked`
