# Changelog for collect-errors

## v0.1.6

* Add utilities unCNfn1, unCNfn2

## v0.1.5

* Make clearPotentialErrors generic, with instances for CN, pairs and lists

## v0.1.4

* Add clearPotentialErrors for removing harmless errors
* Improve documentation

## v0.1.3

* Add CanTakeCNErrors type shortcut

## v0.1.2

* Add CanTakeErrors type class and liftTakeErrors function
* Add instance of deepseq's NFData

## v0.1.1

* Add removeValue functions

## v0.1.0

* Initial port of CollectErrors and CN from mixed-types-num-0.4.1
* Simplify the code by abandoning EnsureCE and related type functions and utilities
* NumErrors within CN are now a set to prevent multiple occurrences of the same error
* Add CollectError or CN instances for various Prelude type classes, including Num and Floating, checking for domain violations
