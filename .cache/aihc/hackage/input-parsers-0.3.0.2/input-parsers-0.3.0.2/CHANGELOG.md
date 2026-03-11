# Revision history for input-parsers

## 0.3.0.2 -- 2023-11-25

* Incremented the upper bound of the `bytestring` and `text` dependencies.

## 0.3.0.1 -- 2023-06-17

* Incremented the upper bound of the `transformers` dependency.

## 0.3 -- 2022-10-02

* Dropped support for GHC < 8.4
* Incremented the upper bound of the `monoid-subclasses` dependency.
* Added `-Wall` and fixed all warnings.

## 0.2.3.2 -- 2022-03-25

* Incremented the upper bound of the `text` dependency.

## 0.2.3.1 -- 2021-11-25

* Incremented the upper bound of the optional `attoparsec` dependency.

## 0.2.3 -- 2021-03-26

* Improved documentation
* Fixed compilation with GHC 8.2.2

## 0.2.2 -- 2021-03-26

* Move Data.ByteString and Data.ByteString.Lazy imports outside ifdef (by Gary Coady)
* Exported all Position methods

## 0.2.1 -- 2021-03-09

* Changed the default instance of `ParserPosition`, made `Position` a subclass of `Ord`.

## 0.2 -- 2021-03-07

* Added `ParserPosition` and made `Position` a class. Deprecated.

## 0.1.0.1 -- 2020-07-19

* Incremented the upper bound of `base` dependency.

## 0.1.0.0 -- 2020-07-18

* First version. Released on an unsuspecting world.
