# 0.7

* COMPILER ASSISTED BREAKING CHANGE. `Parser` is now a type synonym
  for `ParserT Identity`.

* COMPILER ASSISTED BREAKING CHANGE. `runParser` renamed to `parse`.

* COMPILER ASSISTED BREAKING CHANGE. `Text`, `text` and `text'` are
  now named `TextLazy`, `textLazy` and `textLazy`. The previous names
  are now used with strict `Text`.

* Introduced `ParserT`, `parse`, `parseM`, `runParserT`, `parserT`,
  `ParserState`, `initialParserState`, `pTextLazy`.

* Fixed exhaustivenes checks for `Node` patterns.

* New dependencies: `exceptions`, `mmorph`, `mtl`.


# 0.6.2

* `encode` now renders attributes in alphabetical order.


# 0.6.1

* Documentation improvements.

* Compatible with GHC 8.8.


# 0.6

* COMPILER ASSISTED BREAKING CHANGE. `pFail` was removed in favour of
  `fail` from `Monad` of `MonadFail`.

* Added instances for `Parser`: `Semigroup`, `Monoid`, `Selective`,
  `MonadFix`, `MonadZip`.

* Documentation improvements.

* Added dependency on `selective`.

* Removed unnecessary test dependencies.

# 0.5

* COMPILER ASSISTED BREAKING CHANGE. `element` now returns `[Node]`.
  This makes the library safer. It's not possible to construct a
  malformed `Element` anymore using the names exported by the library.
  It is also ergonomic, since most functions with which you would want
  to use a manually constructed `Element` expect a `[Node]` anyway.

* COMPILER ASSISTED BREAKING CHANGE. `element'` now returns
  `Either String Node`.

* COMPILER ASSISTED BREAKING CHANGE. `text` now returns `[Node]`.

* COMPILER ASSISTED BREAKING CHANGE. Removed `IsString Node` instance.

* COMPILER ASSISTED BREAKING CHANGE. Use lazy `Text` inside `Text`
  nodes. This improves `Text` concatenation performance, performed
  internally by `Xmlbf`, and makes more intelligent use of memory when
  dealing with long texts.

* COMPILER ASSISTED BREAKING CHANGE. Removed `pRead`. You are encouraged
  to use `pFail` or `mzero` if you want to write a failing parser.

* BREAKING CHANGE. `pText` now skips empty text nodes.

* Added `node`.

* Added `pFail`.

* Added `text'`.

* Added `pChildren`.

* Added `pAnyElement`.

* Added `pName`.

* Added `NFData` instance for `Node`.

* `encode` doesn't render self-closing tags anymore. Instead, each element has
  its corresponding closing tag.


# 0.4.1

* Generalized type of `pRead`.


# 0.4

* BREAKING CHANGE. `pElement` now skips leading whitespace before an element.

* Fixed nested element parsing (#6)


# 0.3

* BREAKING CHANGE. Renamed `df` and `dfM` to `dfpos` and `dfposM` respectively.

* Added `dfpre` and `dfpreM`.

* Improved `Show` instance for `Node`.

* Added `element'`.


# 0.2

* `Text` constructor hidden in favor of a `text` function plus a `Text` pattern
  synonym, just like with `element` and `Element`.

* Documentation: Render `Element` pattern synonym.


# 0.1

* First version.
