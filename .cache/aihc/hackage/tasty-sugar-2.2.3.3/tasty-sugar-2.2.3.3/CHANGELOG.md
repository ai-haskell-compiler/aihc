# Revision history for tasty-sugar

## 2.2.3.3 -- 2026-01-03

 * Fix to support matching multiple parameter values in a single candidate file.

## 2.2.3.2 -- 2025-12-15

 * Widen tasty-hedgehog constraints to allow earlier v1.3.
 * Internally replace uses of head to assure completeness.

## 2.2.3.1 -- 2025-11-01

 * Bump constraints for parallel to allow 3.4.* [from Vekhir].
 * Bump constraints for microlens to allow 0.5.*.

## 2.2.3.0 -- 2025-10-26

 * Support GHC 8.10 through GHC 9.12.

## 2.2.2.1 -- 2025-07-26

 * Bump constraints to allow optparse-applicative 0.19.

## 2.2.2.0 -- 2024-09-20

 * Bump constraints to use kvitable 1.1.

## 2.2.1.0 -- 2023-06-28

 * Allow optparse-applicative 0.18, tasty 1.5, and hedgehog 1.4 packages.

## 2.2.0.0 -- 2023-05-03

 * Added `rangedParamAdjuster` helper function.
 * Added `sweetAdjuster` field to `CUBE`.
 * The `findSugarIn` function is now monadic with a MonadIO constraint (ato
   support sweetAdjuster functionality).
 * Exports `candidateToPath`, which was added in a previous version but not made
   publicly available.
 * The `--showsearch` output is switched to sorted order and provides a (correct)
   total count.
 * Parameter names are included in the test names along with the value.

## 2.1.0.0 -- 2023-03-20

 * Now supports the ability for the expected file to have the same name as the
   root file.  This is a trivial match, but still allows for capture of
   parameters and associated files.  Major version bump because this may result
   in additional, unexpected matches; to get the original behavior use the (new)
   `distinctResults` modifier on the `findSugar` results.
 * Support for GHC 9.6

## 2.0.1.0 -- 2023-01-09

 * Support for GHC 9.4.

## 2.0.0.1 -- 2022-11-10

 * Fixed arithmetic underflow exception.  This only happened when an
   unconstrained parameter value was used and a subdirectory matched a
   constrained parameter value.

 * Fixed test group name for unconstrained parameters to use the actual parameter
   value (if available).

## 2.0 -- 2022-11-07

 * Performance improvements.  Now scales better when there are more parameters.

 * Improved expected and associated file matching and Expected results parameter
   identification.  This may result in different results than the 1.x version of
   tasty-sugar!

   * Parameter values explicit in the root match but not in the expected file are
     now reported as Explicit in the expParamsMatch because they explicitly match
     part of one of the filenames.  This also means that files which were
     previously provided as expectations are no longer matched because the better
     root parameter matches exclude them.

   * Expectations are selected by best ParamMatch values, in order of parameter
     name, with ties resolved in favor of the longest expected filename.

 * The --showsearch can now also report internal stats for tasty-sugar.

## 1.3.0.2 -- 2022-09-06

 * Add test files to cabal's `extra-sources` so they are included in the package
   (thanks to Felix Yan).

## 1.3.0.1 -- 2022-06-29

 * Bump optparse-applicative upper bound to allow 0.17 series.

## 1.3.0.0 -- 2022-06-28

 * Add `inputDirs` to `CUBE` to replace single `inputDir`.  Allows test action
   files to be spread across various directories.  The `inputDir` is still
   supported for backward-compatibility, but the `inputDirs` is preferred.

 * Any input directory can be specified with a trailing asterisk
   (e.g. "test/data/*") in which case all subdirectories of that root directory
   will be scanned.

   - Any file (root, expected, or associated) may come from any directory (the
     files in a single Sweet Expectation do not all need to occur in
     the same subdirectory).

   - The parameter matching will also consider subdirectory names (prioritized
     over filename parameter matches).

 * `SweetExplanation` `results` field is a single entry instead of an array.

 * Added `getParamVal` function to extract the value (if any) from any
   `ParamMatch`.

 * Fixed some bugs in filename analysis and parameter determination.

## 1.2.0.0 -- 2022-05-16

 * Update for logict 0.8.0.0 breaking change.
 * Fix small issues in example usage haddock.

## 1.1.1.0 -- 2021-04-19

 * Use 'kvitable' to render `--showsearch` output.
 * Update build warnings and refactor code to remove potential partial
   results.
 * Fix tasty ingredients option double-defaulting.
 * Add dependency constraint bounds.
 * Update haddock usage for usage changes.

## 1.1.0.0 -- 2021-01-31

 * Allow multiple tests to be generated for each `Expectation` via
   `withSugarGroups` function.  The `withSugarGroups` third argument
   function (the test generator) now returns a list instead of a
   single test.  While roughly the same effect could have been
   achieved by the test generator function using a `testGroup` to wrap
   multiple tests, this change allows both (a) multiple tests to be
   generated without requiring another level in the test heirarchy,
   and (b) the generator can return an empty list if there should not
   be any tests generated for the specified `Expectation`.

## 1.0.2.0 -- 2021-01-31

 * Export `paramMatchVal` utility function

 * Fix over-trimming of `Expectation` matches

 * Fix identification ranking of associated files


## 1.0.1.1 -- 2021-01-18

 * Fix error where an expected set of matches could match multiple
   root names where the root names contain separators.

## 1.0.1.0 -- 2021-01-18

 * Associated files are now ranked based on the number of parameter
   components in the name and only the highest number of matches are
   provided.  Previously all possible matches were supplied, which
   meant that "more generic" associations caused an Expectation in
   addition to the "more specific" associations.  Now only the "more
   specific" assocations cause an Expectation.

 * Better indentation on Expectation information for --showsearch.

## 1.0.0.0 -- 2021-01-14

 * Allow multiple CUBE inputs for a single test session; Sweets are a semigroup.

 * Run the withSugarGroups mkSweets in MonadIO instead of pure.

 * Remove blank lines in --showsearch output.

## 0.2.0.0 -- 2021-01-12

 * Renamed CUBE "source" to "rootName"

 * Updated Sweets structure to show root match base and match name

 * Rewritten implementation using Logic capabilities.  Clarified many
   corner cases and fully implemented all logic.

* Significantly enhanced testing.

 * Updated documentation.

## 0.1.0.0 -- 2019-12-24

* Initial version.
