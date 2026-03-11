# Change Log

## 0.1.0.0 - 2023-05-12
* First version.

## 0.1.0.1 - 2023-05-15
* Fixed some typos, more detailed docs.

## 0.1.0.2 - 2023-05-17
* Updated resolver to lts-20.21
* Minor documentation changes.

## 0.1.0.3 - 2023-05-17
* Fixed typo that caused build failure.

## 0.1.0.4 - 2023-05-18
* Trying resolver ghc-9.4.5

## 0.2.0.0 - 2023-05-22
* Some of the imperative-style constructions using list comprehensions
  have been replaced with more traditional Haskell constructions using map.
* There is less reliance on the list indexing operator (!!), but the
  use of vector and (!) is avoided to keep this package focused on
  list math.
* Documentation and examples have been added for the convolution functions.
* Convolution type signatures have been changed to make them more generic,
  which is the reason for the major version bump.
