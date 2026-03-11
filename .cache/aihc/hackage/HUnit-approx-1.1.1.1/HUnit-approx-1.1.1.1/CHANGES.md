`HUnit-approx` changelog
========================

1.1.1.1
-------

* Drop support for GHCs lower than 7.2.

1.1.1
-----

* Use `HasCallStack` to provide better locations for errors.

1.1.0.1
-------

* Remove `-main-is` for compatibility with cabal 2.0.

1.1
---

* `HUnit` is Safe only in GHC 7.10 and above; this modifies Safety settings
accordingly.

1.0
---

* Initial release: assertApproxEquals, and assertion and test combinators.
