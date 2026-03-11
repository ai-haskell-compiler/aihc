# haskell-src-exts-simple

## Synopsis

This package provides a shim for haskell-src-exts, exposing the same
AST but without annotations, using ghc's pattern synonyms.

This package is useful for synthesizing and manipulating HSE ASTs
that don't carry source location information. It may also aid
in porting packages from haskell-src-exts 1.17 to 1.18, but it
is *not* a drop-in replacement for the old annotation-free AST.
See the Compatibility section for details.

## Versioning

To be able to track the haskell-src-exts version numbers conveniently,
the first *three* components of the version will be treated as the
major version, followed by the minor version as usual. This deviates
from the PVP.

## Compatibility

Since version 1.18, haskell-src-exts-simple has followed the changes in
haskell-src-exts, without any attempt to keep backward compatibility.

### Comparison to haskell-src-exts 1.17 (as of version 1.18):

haskell-src-exts-simple provides a simplified view of the annotated
AST in haskell-src-exts, so there are differences to the unannotated
AST provided by earlier versions of haskell-src-exts:

* some constructors carried `SrcLoc` information, which is no longer
  available, notably
  - most constructors of the `Decl`, `ModulePragma`, `ImportDecl`,
   `IPBind`, `ClassDecl`, `InstDecl`, `QualConDecl`, `GadtDecl`
    datatypes
  - the `Lambda`, `ExpTypeSig` and `Proc` constructors of the `Exp`
    type (and a few XML related ones)
  - the `Generator` constructor of `Stmt`
  - the `Alt` constructor of `Alt`
  - the `PatTypeSig` constructor of `Pat`
* the second, fourth and fifth fields of the `Module` constructor have
  been combined into a single field of type `ModuleHead`

(this list is probably incomplete)

### ghc compatibility

* haskell-src-exts-simple requires ghc-8.0 or later
