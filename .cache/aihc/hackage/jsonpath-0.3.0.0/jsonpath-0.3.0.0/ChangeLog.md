# Changelog for jsonpath-hs

## v0.3.0.0 - Lots of breaking changes, they come with new features

This release aims to address many deviations from similar libraries in other
programming languages, this is thanks to Christoph Burgmer's
[json-path-comparison
project](https://cburgmer.github.io/json-path-comparison/).

There has also been significant work in codifying JSONPath by the IETF-WG for
JSONPath, the draft spec can be acccessed
[here](https://ietf-wg-jsonpath.github.io/draft-ietf-jsonpath-base/draft-ietf-jsonpath-base.html).
This release also aims to adapt some of the ideas from from spec.

As a result there have been significant breaking changes in the types and also
small changes in the way JSONPaths are executed.

List of changes:
* Fix compiler warnings and bugs with non-total pattern matches.
* Allow double quoted literals and field accessors.
* Ensure termination when start or end of slice are too big/small.
* Implement slice execution based on IETF draft spec .
* Ensure that a valid JSONPath never fails to execute.
* Drop support for GHC <= 8.2.
* Use megaparsec instead of attoparsec for better error messages.
* Allow escape sequences in key names.
* Allow parsing empty paths.
* Allow spaces arround index selectors.
* Allow selecting keys in unions and allow many union elements.
* Implement 'and', 'or' and 'not' operator support in filters.
* Allow comparison between two singular paths.
* Allow bools and nulls in filters.

## v0.2.1.0

* Support and require aeson >= 2

## v0.2.0.0

* BreakingChange: Fix typo in `BeginningPoint`.
* Fix typo in parser error.

## v0.1.0.2

* Remove upper bounds from dependencies, as most of them are quite stable packages.

## v0.1.0.1

* Import Data.Semigroup to support GHC 8.
* Add test json files to make sure test sdist compile and runs.

## v0.1.0.0

* Start the project.
