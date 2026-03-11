0.3
---
* Fixed an outstanding bug in round.

0.2.1.1
-------
* Minor documentation improvements.
* Require `base >= 4.7` properly in the source repository as the implementation uses `coerce`, so it doesn't work on GHC < 7.8.
  This was fixed by maintenance releases to hackage previously.

0.2.1
-----
* Fixed bug in `signum`

0.2
---
* Convert to/from Double to avoid precision issues.

0.1.0.1
-------
* Change `tested-with` to admit the support window is only 7.8+ at this point due to the use of `Data.Coerce`

0.1
---
* Initial release

