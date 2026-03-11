# 0.1.2 (2025-07-15)

* Fix `qiAdd`, `qiSub`, `qiMul`, `qiDiv` when one of the arguments is rational, restoring behaviour of `quadratic-irrational-0.0.6`.

# 0.1.1 (2020-04-15)

* Support `arithmoi-0.11`.

# 0.1.0 (2019-04-26)

* Allow imaginary square roots, e. g., `qi 1 1 (-5) 1`.
* Remove `Ord QI` instance: complex values cannot be ordered.
* Roots of 0 are reduced: `qi a b 0 d` becomes `qi a 0 2 d`.
* Remove `qiZero` and `qiOne`.

# 0.0.6 (2018-08-29)

* Support GHC up to 8.6.1.

# 0.0.5 (2014-03-28)

* Add an `Ord` instance.

# 0.0.4 (2014-03-27)

* Make the description more precise.
* Add continuedFractionApproximate for rational partial evaluations of
  continued fractions.

# 0.0.3 (2014-03-26)

* Add a more verbose description of the library.

# 0.0.2 (2014-03-25)

* Add doctests.
* Fix qiModify potentially constructing `qi 1 0 5 1` instead of the equivalent
  but simpler `qi 1 0 0 1`.
* Add lenses.

# 0.0.1 (2014-03-24)

* Initial release.
