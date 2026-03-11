# list-predicate
_list-predicate_ is a library for predicates (True/False queries) on lists.

## Features
The following features have been implemented and extensively tested:
  * `allEqual`, `allEqualBy`: Whether all the elements of a list are equal
  * `sorted`, `sortedBy`: Whether the elements are in sorted order.
  * `allUnique`, `allUniqueBy`: Whether the elements are all unique.
  * `ascSequential`, `descSequential`: Whether a list of Enums is ascending or
         descending sequentially (one-by-one)
  * `palindrome`: Whether the list is a palindrome

Full documentation can be built with Haddock (see instructions below). Once this
package is added to Hackage, documentation will be available there as well.

## Installation
Build and install with [Stack](https://www.haskellstack.org):

```
git clone https://github.com/pgujjula/list-utilities/list-predicate
cd list-predicate
stack build list-predicate            # build the project
stack haddock list-predicate --open   # build and view documentation
stack test list-predicate             # run the test suite
```

## Contact
If you want to report a bug, request a feature, or suggest improvements, feel
free to email me at preetham (dot) gujjula (at) protonmail (dot) com!

-- Preetham
