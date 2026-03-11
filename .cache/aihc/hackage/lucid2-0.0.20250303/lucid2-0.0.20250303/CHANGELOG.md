## 0.0.20250303

* Support GHC 9.12

## 0.0.20240424

* Add `dialog_` element. <https://github.com/chrisdone/lucid/issues/147>

## 0.0.20230706

* Inline the `blaze-builder` modules in-use into the package, to drop
  dependency on
  `blaze-builder`. <https://github.com/chrisdone/lucid/issues/143>

## 0.0.20221012

* Fix `commuteHtmlT` in favor of newly added `commuteHtmlT2`

## 0.0.20220526

This release adds some extra functions for running different monad
stacks, prompted by Joe Vargas.

* Add `generalizeHtmlT`, `commuteHtmlT` and `hoistHtmlT`.
* Deprecate the accidentally exported `relaxHtmlT = undefined`.

## 0.0.20220509

* Use explicit imports for mtl, avoiding mtl-2.3 incompatibility.

## New in lucid2

lucid2 is a new package published to Hackage and maintained under `lucid2/` in this repository alongside `lucid1`. Releases are made under the [Immutable Publishing Policy](https://chrisdone.com/posts/ipp/), and users had asked for many breaking changes, therefore we needed a completely new namespace to work under, hence, "lucid2". Many things have also been dropped in the process, to simplify the codebase.

This upgrade is **entirely optional**.

People using `lucid` **will not have to do anything**. They can continue using that package indefinitely, it will be maintained alongside `lucid2`, to keep it building with GHCs and things. You can even use both packages in the same codebase with `-XPackageImports`.

Changed:

* All `on*` attributes and `style_` do not escape their values anymore. Be
  careful. Though you were probably being careful with these anyway,
  as they are inherently dangerous for XXS.
* The `Attribute` type has been replaced by `Attributes`, which is a `Monoid` instance. This makes it really easy to write code like `if X then class_ "foo" else mempty`.
* The `class_` and `style_` attributes combine with a space and `;` between them when there are duplicates, e.g. `[class_ "x",class_ "y"]` produces `class="x y"`. These are special cases, the rest of the attributes do not have special combining behavior and will be simple concatenation.

Renamed:

* makeAttribute is renamed to makeAttributes.
* Added makeAttributesRaw.

Dropped:

* Eq/Ord/Show instances for Attribute.
* Drop the `Lucid.Bootstrap` module entirely.
* Dropped the mmorph dependency (breaking changes often, not
  reliable), instead we provide a simple `hoist` function of the right
  type.
* Drop MonadError.
* Drop MonadWriter.
* Drop hashable.
* Drop XML elements.

Dependencies:

* We now only depend on blaze-builder, and the rest are libraries that
  come with GHC, which are held to a slightly higher standard of not
  breaking changes.
