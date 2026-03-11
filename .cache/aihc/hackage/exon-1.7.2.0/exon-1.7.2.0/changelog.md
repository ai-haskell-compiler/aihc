# 1.6.0.0

* Replace `flatparse` with `parsec` for less friction when updating GHC.

# 1.5.0.0

* Use `exonProcess` for `intercalate`
* Add `intercalate1` for `NonEmpty`

# 1.2.0.0

* Add class `ExonExpression`, allowing customization of how interpolated expressions are converted to builders.
  The method `exonExpression` is provided with the function `exonBuilder` as an argument to avoid having to depend on
  the class manually.
* Change multi-method classes to use the default-method implementation style for the overlappable instance.

# 1.1.0.0

* Add `exonConcat` to `ExonAppend`. The function is pulled out of `exonBuild` to allow using a different way of folding
  the segment list.

# 1.0.0.0

* Rewrite the main logic, splitting it across multiple classes.
* Automatically unwrap `Generic` newtype segments.
* Use builders like `Text.Builder`, unwrapping `Generic` newtypes of the result type.
* Don't treat whitespace as special in the default quoter.

# 0.2.0.0

* Add `exonws`, a variant that preserves whitespace.
* Add `exonWith`, a constructor for custom quasiquoters.

# 0.1.1.0

* Add `Exon` instance for `String -> String`, used by `showsPrec`.
