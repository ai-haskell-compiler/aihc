# Revision history for data-findcycle

## 0.1.2.0 -- 2025-05-31

* Correct unfortunate misspelling of "Nivasch" as "Nivash".
  New correctly named `nivasch*` functions were added. The old `nivash*` functions
  are deprecated and will be removed in 0.2.
* Fix documentation rendering issue caused by the introduction of LANGUAGE CPP
  by removing CPP. Turns out it wasn't really necessary to start with.
* Minor refactoring to improve test coverage.

## 0.1.1.0 -- 2025-04-24

* Add `nivashPart` and `nivashPartWithinBounds` as variants of `nivash` using
  stack partitioning.
* Minor documentation fixes and improvements.

## 0.1.0.0 -- 2025-03-08

* First version. Released on an unsuspecting world.
