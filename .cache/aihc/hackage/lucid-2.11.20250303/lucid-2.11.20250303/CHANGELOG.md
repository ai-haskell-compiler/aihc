## Upcoming

## 2.11.20250303

* Support GHC 9.12

## 2.11.20230408

* Don't expect Control.Monad to be re-exported from mtl anymore

## 2.11.1

* Use explicit imports for mtl, avoids the mtl-2.3 issue
* Added `minlength` attribute.
* Added `loading` attribute.

## 2.11.0

* Change internal attributes to `Seq Attribute`. This preserves
  ordering. Attributes are merged in a left-biased way, preserving the
  key order as first encountered.

## 2.10.0

* Change internal attributes representation from HashMap to Map. This
  introduces stable ordering, at a negligible performance cost for
  realistic element sizes. This may affect some test suites.
* doctype no longer accepts attributes. You can use `with` with
  `doctypeHtml` now, if needed.

## 2.9.12.1

* Allow different orderings of attributes in test-suite

## 2.9.12

* Add MonadFix instance

## 2.9.11

* Add GHC-8.6 support
* row-fluid and container-fluid instead of camelCase

## 2.9.10

* Drop GHC-7.8 and older (pre-AMP) support
* Generalise type-signatures to require only `Applicative` or `Functor`,
  when that's enough

## 2.9.9

* Add `commuteHtmlT` to commute `HtmlT m a` into `m (HtmlT n a)`.
* Add `MonadError e m => MonadError e (HtmlT m)` and
  `MonadWriter w m => MonadWriter w (HtmlT m)` instances

## 2.9.8.1

* Improve performance by adding `INLINE` pragmas to `Monad` etc. combinators.

## 2.9.8

* Add `integrity_`, `crossorigin_` attributes
* Add `classes_` smart attribute constructor
* Add `ToHtml (HtmlT m a)` instance

## 2.9.7

* Add `Semigroup (HtmlT m a)` instance
* Add `MonadState` and `MonadReader` instances

## 2.9.6

* Fix compilation of benchmarks
* Add @athanclark's version of `relaxHtmlT`
* Add a utility to generalize the underlying monad from Identity: `relaxHtmlT`

## 2.9.5

* Add ToHtml instance for ByteString (both)
* Add `MFunctor HtmlT` instance, i.e. `hoist` from @mmorph@.

## 2.9.1

* Small performance tweaks.
* Make svg_ an element.

## 2.6

* Restrict monoid instance's a to ~ () (means you can use mempty
  without inference errors)

## 2.2

* Export renderToFile from top-level Lucid module.

## 2.1

* Add some extra HTML tags.

## 2.0

* Use variadic HTML terms.
* Add lazy Text instance for ToHtml.

## 1.0

* Initial version.
