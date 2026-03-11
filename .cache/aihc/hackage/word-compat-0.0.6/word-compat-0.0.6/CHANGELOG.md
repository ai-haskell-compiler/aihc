# Revision history for word-compat

## 0.0.6

* Added a pattern synonym for `Word64#`

## 0.0.5

* Supported `Word64#` on GHC 9.4 and older

## 0.0.4

* Added `GHC.Prim.Compat`, providing array operations compatible with `GHC.Word.Compat`
## 0.0.3

* Added `COMPLETE` pragmas to pattern synonyms

## 0.0.2

* Fixed a bug where bitReverse* were incorrectly exported on base >= 4.12
* On GHCs older than 9.2, it now reexports data constructors instead of pattern synonyms

## 0.0.1

* First version. Released on an unsuspecting world.
