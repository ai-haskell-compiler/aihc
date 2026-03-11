Version 0.4.7.0:  (2014-11-19)

  * `foldt` tweaked to be made slightly lazier,  thus also affecting
    `mergeAll`,  `mergeAllBy`, `unionAll`, and `unionAllBy`.  Thanks
    goes to Will Ness for providing the following example that
    previously entered into an infinite non-productive loop:

~~~
    let ps=3:minus[5,7..] (unionAll [[p*p,p*(p+2)..]|p<-ps]) in take 100 (2:ps)
~~~

    Note that the _de facto_ semantics of `foldt` has been modified
    slightly:  instead of computing the sum of a list,  it computes
    the sum of a list plus zero.   Of course,  if zero is a true
    identity of plus,  then this change is of no semantic consequence.

Version 0.4.6.1:  (2014-04-19)

  * Added strictness annotations inside of sortOn and nubSortOn

  * Tweaked documentation of nubSort

  * Conditionally reexport sortOn from Data.List when available.
    (See [GHC commit 44512e3c](https://ghc.haskell.org/trac/ghc/changeset/44512e3c855d8fb36ab6580f4f97f842ebcf4c6c/ghc))

Version 0.4.6:  (2014-02-15)

  * Generalized type signature of `isectBy`, `minusBy`, and `minusBy'`,
    leaving the definition unchanged.

  * Made the documentation of `union` more precise.

Version 0.4.5:  (2012-03-12)

  * New function, `minus'`

  * Exported `foldt` and `foldt'`

  * Documentation improvements

  * Reverted the implementation of `mergeAll` and `unionAll` to version
    0.4.2 because the simplified implementation would force the head of
    the inner list appearing at the next highest power of 2 well before
    it was necessary to do so.

Version 0.4.4:  (2010-12-24)

  * Simplified the implementation of `mergeAll` and `unionAll` based on
    comments from Will Ness.

    <http://www.haskell.org/pipermail/haskell-cafe/2010-December/087587.html>

Version 0.4.3:  (2010-03-02)

  * Improved the implementation of `nubSort`,  mirroring the improvements made
    to `Data.List.sort` in GHC-6.13.20091224 and first released in GHC-7.0.1.
    Thanks to Gwern Branwen for calling the change to my attention.  Instead
    of initially breaking the input list into singletons before the merge
    process,  the improved version breaks the input list into monotonic runs.

    <http://www.haskell.org/pipermail/libraries/2010-March/013066.html>

  * Minor formatting improvements in the Haddock documentation.

Version 0.4.2:  (2010-02-18)

  * Fixed non-productive loop in `unionAll` when applied to an infinite list
    of lists.  Thanks to Omar Antolín Camarena for reporting the bug and
    Heinrich Apfelmus for some useful comments.

    <http://www.haskell.org/pipermail/haskell-cafe/2010-February/073403.html>
    <http://www.haskell.org/pipermail/haskell-cafe/2010-February/073437.html>

  * Added regression test to test suite.

Version 0.4.1:  (2010-02-17)

  * Simplified the implementation of `mergeAll` and `unionAll` thanks
    to some pointers by Heinrich Apfelmus.  This introduced an infinite
    non-productive loop into `unionAll`,  which was fixed in 0.4.2 without
    giving up the simplifications.

    <http://www.haskell.org/pipermail/haskell-cafe/2010-February/073356.html>

  * Minor documentation fixes

Version 0.4:    (2010-02-15)

  * The "CHANGES" file was added to document the changes between releases.

  * Documentation Improvements

  * A rough first pass at a test suite

  * The functions `mergeAll` and `unionAll` were added.  They operate
    on a possibly infinite list of possibly infinite ordered lists; assuming
    the heads of the lists are ordered.

    Thanks goes to Omar Antolín Camarena, Heinrich Apfelmus, and Dave Bayer.

    Omar Antolín Camarena suggested the addition,  located the article
    used as the basis for the implementation,  and was quite helpful with
    testing and debugging.

    Heinrich Apfelmus wrote his "Implicit Heaps" article, where he
    simplified an algorithm by Dave Bayer.  It is this article that forms
    the basis of our implementation.

    <http://apfelmus.nfshost.com/articles/implicit-heaps.html>

    Dave Bayer posted his 'venturi' implementation to the haskell-cafe
    mailing list on 2007 Jul 22.  It also appears as "BayerPrimes.hs"
    inside of Melissa O'Neill's "haskell-primes.zip":

    <http://www.haskell.org/pipermail/haskell-cafe/2007-July/029391.html>
    <http://www.cs.hmc.edu/~oneill/code/haskell-primes.zip>

Version 0.2:    (2010-02-07)

  * The module name was changed from `Data.OrdList` to `Data.List.Ordered`

  * Fixed bugs in `insertSetBy`,  `insertBagBy`,  and `nub`.  The insertion
    functions assumed reversed lists, while `nub` failed to remove duplicates.

    Thanks to Topi Karvonen for reporting the first issue!

  * Changed semantics of `insertSetBy` slightly:  the new version replaces an
    element if it is already there.  If the old semantics turns out to be
    important,  a new function can be added at a later date.

  * Changed semantics of `nubBy`: the new version negates the binary relation,
    so that `new_nubBy f == old_nubBy (not . f)`.  It is now in better keeping
    with the spirit of the rest of the library,  and mades the bug in `nub`
    more obvious.

  * Better documentation,  I hope.  At the very least, the process of
    documenting `nubBy` revealed the bug in `nub`.


Version 0.0.1:  (2009-07-10)

  * The initial release, sadly, did not contain the source file

Version 0.0:    (2009-07-10)

  * Initial Release
