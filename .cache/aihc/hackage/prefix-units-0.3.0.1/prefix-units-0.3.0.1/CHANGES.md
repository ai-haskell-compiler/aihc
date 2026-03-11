## Version 0.3.0.1

*released Thu, 27 Apr 2023*

It turns out that even if `cabal check` shows no warnings, Hackage
doesn't accept packages with very old cabal version constraints. So
this release bumps that, which in turns makes `cabal check` actually
start to show warnings, so modernise the cabal file as a result.

There are no code changes compared to 0.3.0, so this is a no-op
release from the point of view of functionality.

## Version 0.3.0

*released Wed, 26 Apr 2023*

* Add the new quecto/ronto/ronna/quetta units. I didn't think I'd ever
  have to extend the list of units! Not sure entirely if these should
  be part of the parseKMGT function, since they're high enough to be
  out of normal computer units, but they do have lower/upper versions,
  so adding them seems the right thing.

## Version 0.2.0

*released Sun, 22 Nov 2015*

* Incompatible API change to cleanup some initial design decisions:
  the two level `FormatOption`/`FormatMode` model is removed, the
  fixed unit of `FormatOption` is moved to a new constructor
  `FormatMode`, and `FormatOption` is removed entirely. This should be
  a simpler API, at the cost of breaking compatibility.
* Fixed issue #3 (No support for negative numbers).
* Worked around issue #1 (Add 'base' unit) by adding a mode that
  disables scaling; it should have the same effect without introducing
  an artificial unit.

## Version 0.1.0.2

*released Sun, 23 Nov 2014*

* Trivial release for compatibility with QuickCheck 2.7 and older
  HUnit packages as found in Wheezy.
* The release switches the test suite to use Cabal macros, which might
  create issues in some cases.
* Fixed issue #2 (Wrong formatting for small numbers in SI mode).

## Version 0.1.0.1

*released Mon, 19 May 2014*

* Trivial release updating upper package bounds (for testing),
  updating homepage/related settings as part of move to github, and
  fixing a few documentation issues.

## Version 0.1.0

*released Thu, 03 May 2012*

* Initial release.
