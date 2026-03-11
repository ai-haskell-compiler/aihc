## Cue sheet 2.0.2

* Builds with `mtl-2.3`.

## Cue sheet 2.0.1

* Got rid of `data-default-class` dependency, which was completely
  redundant.

## Cue sheet 2.0.0

* Uses Megaparsec 7. The `parseCueSheet` now returns `ParseErrorBundle`.

## Cue sheet 1.0.1

* Allow Megaparsec 6.4.0.

## Cue sheet 1.0.0

* Uses Megaparsec 6.

* Adjusted the `Eec` type to use it with Megaparsec 6.

* The `parseCueSheet` function now accepts strict `ByteString` instead of
  lazy one.

## CUE sheet 0.1.1

* Improved documentation.

* Improved the parser using the newest features of Megaparsec 5.3.0.

## CUE sheet 0.1.0

* Initial release.
