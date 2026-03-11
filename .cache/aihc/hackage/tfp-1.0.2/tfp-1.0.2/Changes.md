1.0:

* Change representation of decimals to an inherently normalized form
  that is symmetric with respect to positive and negative numbers.

* singularize module names

* separate Decimal and general representation

* use Proxys instead of plain types for data functions
  This is also consistent with new Nat kind,
  where types of kind Nat have no data values.

* Ord: make infix operators classes and prefix functions type functions
  It seems to be more natural to me to write
     x :<: y   and   GTT x y ~ True

* Num, Bool, Ord: remove T suffixes from functions
  Use qualification instead.
