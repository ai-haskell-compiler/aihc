## 0.2 [2018.07.01]
* `thread-local-storage` now checks that threads calling
  `Data.TLS.PThread.getTLS` are bound threads, and throws a runtime error if
  this is not the case.

  For this reason, if you call `getTLS` in a thread spawned by `forkOn`, it
  will break with this release, so you are encouraged to switch uses of
  `forkOn` to `forkOS`.

## 0.1.2 [2017.06.23]
* Fix the benchmark suite with `criterion-1.2`

## 0.1.1 [2017.01.22]
* Expose `Data.TLS.PThread.Internal`. Note that there are no API guarantees
  whatsoever with this module, so use it with caution.
* Add `pthread` to `extra-libraries`. Without it, some systems suffer
  from linker errors when using this library.
* Fix build on GHC 7.6 and 7.8.
