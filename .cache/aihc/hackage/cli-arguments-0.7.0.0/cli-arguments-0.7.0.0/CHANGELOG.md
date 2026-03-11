# Revision history for cli-arguments

## 0.1.0.0 -- 2021-12-27

* First version. Released on an unsuspecting world.

## 0.1.0.1 -- 2021-12-27

* First version revised A. Improved the cabal file so that the description is correct.

## 0.2.0.0 -- 2021-12-28

* Second version. Fixed issues with the partially defined function. Added new
functionality related to sorting and arrays.

## 0.3.0.0 -- 2021-12-29

* Third version. Added new functions with the 1 in their names to deal with apropos (in some way) framed by two delimiters
variants of the groups. Some code improvements, reduced duplications, changed pragmas INLINE to INLINABLE.

## 0.3.1.0 -- 2021-12-30

* Third version revised A. Added some more general functions to process arrays,
type synonyms to improve code readability, reduced code duplication.

## 0.4.0.0 -- 2022-01-10

* Fourth version. Added new functions. Changed the dependency boundaries.

## 0.5.0.0 -- 2022-01-11

* Fifth version. Added 3 new modules CLI.Arguments.Parsing, CLI.Arguments.Arr, CLI.Arguments.Get and the new functionality to
the first one, splitted the previous one between the last two. This should improve importing opportunities.

## 0.6.0.0 -- 2022-01-12

* Sixth version. Removed the functionality from the CLI.Arguments module that 
has a bug in it and non-consistent logics. Splitted the functionality between 
modules further, added a new module CLI.Arguments.Sorted with the corresponding
functionality. Please, use the CLI.Arguments.Parsing module as logic and 
consistent one.

## 0.7.0.0 -- 2023-01-30

* Seventh version. Switched to NoImplicitPrelude extension. Some documentation improvements. Changed
the dependency boundaries.

