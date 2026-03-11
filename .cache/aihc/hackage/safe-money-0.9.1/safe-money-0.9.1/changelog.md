# Version 0.9.1

* Add approximation method `HalfAwayFromZero`, also known as “kaufmännisches Runden”
  in German speaking countries. See issue #55.
  
* Fix compilation with GHC 9. See issue #59.

* Added `exchangeRate'`. See issue #61.

# Version 0.9

* _BREAKING CHANGE, POSSIBLY REQUIRING HUMAN INTERVENTION_. Changed
  `UnitScale "BSD" "cent"` from `(1, 1)` to `(100, 1)`, changed
  `UnitScale "HTG" "centime"` from `(1, 1)` to `(100, 1)`, changed
  `UnitScale "MDL" "leu"` from `(100, 1)` to `(1, 1)`, and changed
  `UnitScale "SBD" "dollar"` from `(100, 1)` to `(1, 1)`.  The scales in
  question were wrong before. If you relied on these specific scales,
  for example, for serializing and storing amounts in a database, you
  will need to rescale your amounts.

# Version 0.8.1

* Fix decimal rendering of some small cent values (e.g., `0.02`). See issue #45.

* Fixed `xxxToDecimal` docs. See issue #45.

* Add `someXxxToDecimal`. See issue #44.


# Version 0.8

* _COMPILER ASSISTED BREAKING CHANGE_. Introduced `DecimalConf`, which all
  of the `xxxToDecimal` and `xxxFromDecimal` are now using. This concentrates
  all decimal rendering and parsing settings in a single place, and more
  importantly, it makes all of the `xxxToDecimal` return a `Text` value
  directly, rather than a `Maybe Text`. In turn, `Separators` has been
  introduced for configuring decimal and thousands separators. See issue #30.

* _COMPILER ASSISTED BREAKING CHANGE_. The `Scale` type family was renamed to
  `UnitScale`.

* _COMPILER ASSISTED BREAKING CHANGE_. Previous instances of the `Scale` type
  family where the `currency` and the `unit` were the same (e.g., `Scale "USD"
  "USD"`), intended to convey a canonical scale for the currency, are now
  written as `CurrencyScale currency` (e.g., `CurrencyScale "USD"`).

* _COMPILER ASSISTED BREAKING CHANGE_. A new datatype called `Scale` has been
  introduced. This datatype replaces term-level uses of `Rational` to convey the
  scale of a discrete monetary amount. The rationale for this, beyond improved
  type errors, is that checking for the scale's validity can be done at a single
  place (namely, the new `scaleFromRational`), rather than everywhere a
  term-level scale was expected as input. See issue #43.

* Added new approximation method `HalfEven`, also known as “Bankers rounding”.
  See issue #42.

* New currencies: VES (see issue #33), UYW.


# Version 0.7.1

* Fixed compilation with GHC 8.6.

* Fixed an issue when rendering decimal values smaller than the smallest
  possible fractional part. See issue #39.


# Version 0.7

* _IMPORTANT_. All of the changes in this release are fully backwards
  compatible. What happens in this release is that many instances
  previously exported by `safe-money` now live in different libraries so
  that downstream packages can avoid using Cabal flags to specify
  dependencies.

* _COMPILER ASSISTED BREAKING CHANGE_. `FromJSON` and `ToJSON` (from
  `aeson`) instances are not exported from the `safe-money` library
  anymore. Instead, you are now expected to depend on the vetted
  `safe-money-aeson` library and import `Money.Aeson` to get the
  relevant instances. These instances are exactly the same ones
  previously exported by `safe-money`. As a consequence of this change,
  the `aeson` Cabal flag in `safe-money` and the optional dependency on
  `aeson` were removed, which simplifies downstream packaging.

* _COMPILER ASSISTED BREAKING CHANGE_. `Store` (from `store`) instances
  are not exported from the `safe-money` library anymore. Instead, you
  are now expected to depend on the vetted `safe-money-store` library
  and import `Money.Store` to get the relevant instances. These
  instances are exactly the same ones previously exported by
  `safe-money`. As a consequence of this change, the `store` Cabal flag
  in `safe-money` and the optional dependency on `serialise` were
  removed, which simplifies downstream packaging.

* _COMPILER ASSISTED BREAKING CHANGE_. `Serialise` (from `serialise`)
  instances are not exported from the `safe-money` library anymore.
  Instead, you are now expected to depend on the vetted
  `safe-money-serialise` library and import `Money.Serialise` to get the
  relevant instances. These instances are exactly the same ones
  previously exported by `safe-money`. As a consequence of this change,
  the `serialise` Cabal flag in `safe-money` and the optional dependency
  on `serialise` were removed, which simplifies downstream packaging.

* _COMPILER ASSISTED BREAKING CHANGE_. `Serialize` (from `cereal`)
  instances are not exported from the `safe-money` library anymore.
  Instead, you are now expected to depend on the vetted
  `safe-money-cereal` library and import `Money.Serialize` to get the
  relevant instances. These instances are exactly the same ones
  previously exported by `safe-money`. As a consequence of this change,
  the `cereal` Cabal flag in `safe-money` and the optional dependency on
  `cereal` were removed, which simplifies downstream packaging.

* _COMPILER ASSISTED BREAKING CHANGE_. `FromXml` and `ToXml` (from
  `xmlbf`) instances are not exported from the `safe-money` library
  anymore. Instead, you are now expected to depend on the vetted
  `safe-money-xmlbf` library and import `Money.Aeson` to get the
  relevant instances. These instances are exactly the same ones
  previously exported by `safe-money`. As a consequence of this change,
  the `xmlbf` Cabal flag in `safe-money` and the optional dependency on
  `xmlbf` were removed, which simplifies downstream packaging.

* _COMPILER ASSISTED BREAKING CHANGE_. `hashable` and `vector-space` are
  now mandatory dependencies of `safe-money`. `Hashable`, `VectorSpace`
  and `AdditiveGroup` instances for the various `safe-money` datatypes
  are now exported from `Money`. The `hashable` and `vector-space` Cabal
  flags are now gone.

* `Arbitrary` instances (from the `QuickCheck` packages) for the various
  `safe-money` datatypes are now exported from `Money`. `QuickCheck` is
  now a mandatory dependency of `safe-money`.


# Version 0.6

* _COMPILER ASSISTED BREAKING CHANGE_. `denseToDecimal` now takes a positive
  `Rational` rather than a `Proxy scale`.

* _COMPILER ASSISTED BREAKING CHANGE_. `denseFromDecimal` and
  `discreteFromDecimal` now take a positive `Rational` scale, like their
  `xxxToDecimal` counterparts.

* _COMPILE ASSISTED BREAKING CHANGE_. `binary`, `deepseq` and `text` are now
  mandatory dependencies since they are included with the standard GHC
  distribution. Thus, the tags for disabling them have been removed.

* _COMPILE ASSISTED BREAKING CHANGE_. `Text` replaced the use of `String` in the
  public API. For example, `denseCurrency` now returns `Text`, instead of
  `String`. This change doesn't break backwards compatibilility with binary
  serializations.

* Introduced a new function `discreteToDecimal`.

* Added tests to ensure backwards compatibility of serializations.


# Version 0.5

* _COMPILER ASSISTED BREAKING CHANGE_. The `round`, `floor`, `ceiling` and
  `truncate` functions were replaced by a single `discreteFromDense` function
  taking an argument of type `Approximation` (`Round`, `Floor`, `Ceiling` or
  `Truncate`) as an argument.

* _COMPILER ASSISTED BREAKING CHANGE_. The `fromDiscrete` function was
  renamed to `denseFromDiscrete`.

* _COMPILER ASSISTED BREAKING CHANGE_. The `fromExchangeRate` function was
  renamed to `exchangeRateToRational`.

* _COMPILER ASSISTED BREAKING CHANGE_. The `flipExchangeRate` function was
  renamed to `exchangeRateRecip`.

* _COMPILER ASSISTED BREAKING CHANGE_. The `Dense` is not an instance of
  `Fractional` anymore because `recip` and `/` could potentially crash.

* Introduced new functions for rendering and parsing decimal reperesentations
  of monetary amounts: `denseCurrency`, `discreteCurrency`, `denseFromDecimal`,
  `denseToDecimal`, `discreteFromDecimal`, `exchangeRateFromDecimal`,
  `exchangeRateToDecimal`.

* Introduced optional `AdditiveGroup` and `VectorSpace` group instances for
  `Dense` and `Discrete`. These type-classes come from the `vector-space`
  library and they can be enabled or disabled via the `vector-space` Cabal flag,
  which is enabled by default.

* Introduced `discrete` constructor which behaves just like `fromInteger`.

* Introduced the `dense'` constructor as an unsafe but convenient version of
  `dense`.

* The `ErrScaleNonCanonical` type is now exported.

* The `Money.Internal` module is now exposed, but hidden from the Haddock
  documentation.

* New scale: `"BTC" "millibitcoin"`.

* Added many tests.


# Version 0.4.1

* `ExchangeRate` is now a `Category`.


# Version 0.4

* **BREAKING CHANGE REQUIRING HUMAN INTERVENTION**. The JSON serializations for
  all of `Dense`, `SomeDense`, `Discrete`, `SomeDiscrete`, `ExchangeRate` and
  `SomeExchangeRate` changed. The `FromJSON` instances are backwards compatible
  with the old serializations, but the `ToJSON` instances will only generate the
  new format, which is the same as the old format except the leading strings
  `"Dense"`, `"Discrete"` and `"ExchangeRate"`, respectively, are not present in
  the rendered JSON array anymore. So, if you were manually relying on the
  `ToJSON` instance, please update your code.

* _COMPILER ASSISTED BREAKING CHANGE_. Changed the `Rep` suffix for a `Some`
  prefix Everywhere.  For example, `DenseRep` was renamed to `SomeDense`.

* _COMPILER ASSISTED BREAKING CHANGE_. Replaced the
  `someDenseAmountNumerator` and `someDenseAmountDenominator` `Integers` with a
  single `someDenseAmount` `Rational` number. Similarly for `someDiscreteScale`
  and `someExchangeRateRate`. The `mkSomeDense`, `someDiscreteScale` and
  `mkSomeDense` also take a `Rational` now.

* _COMPILER ASSISTED BREAKING CHANGE_. The `truncate`, `floor`, `celing` and
  `round` functions now return just `0` as a reminder if there's no significant
  reminder, instead of `Nothing`.

* Added instances for `serialise`.

* Added instances for `xmlbf`.

* Fixed `Show` instances so that surrounding parentheses are included when
  necessary.

* New currencies: Ripple, Litecoin, Ada, Monero.


# Version 0.3

* _COMPILER ASSISTED BREAKING CHANGE_. The `Data.Money` module was renamed to
  `Money`.

* _COMPILER ASSISTED BREAKING CHANGE_. The `Data.Money.Internal` module is not
  exposed anymore. All of its contents are now exported from the `Money` module.

* _COMPILER ASSISTED BREAKING CHANGE_. Renamed `discreteRep` to
  `toDiscreteRep`.

* _COMPILER ASSISTED BREAKING CHANGE_. Renamed `denseRep` to `toDenseRep`.

* _COMPILER ASSISTED BREAKING CHANGE_. Renamed `exchangeRateRep` to
  `toExchangeRateRep`.

* _COMPILER ASSISTED BREAKING CHANGE_. Renamed Iceleandic currency `"eyir"` to
  `"eyrir"`

* Remove upper bound constraints from all dependencies except `base`.

* Made dependency on `store` optional for the test suite, so that it can run on
  GHCJS.


# Version 0.2

* Cabal flags are now manual (`aeson`, `binary`, `bytes`, `cereal`, `deepseq`,
  `hashable`).

* Backwards compatibility with GHC 7.10.

* Fix `Store` instances and test them.


# Version 0.1

* Initial release.
