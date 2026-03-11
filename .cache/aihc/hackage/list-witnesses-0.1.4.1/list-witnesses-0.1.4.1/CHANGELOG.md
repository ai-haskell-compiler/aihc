Changelog
=========

Version 0.1.4.1
---------------

*February 27, 2023*

<https://github.com/mstksg/list-witnesses/releases/tag/v0.1.4.1>

*   Remove upper bounds, fix deprecated pragmas

Version 0.1.4.0
---------------

*July 22, 2023*

<https://github.com/mstksg/list-witnesses/releases/tag/v0.1.4.0>

*   Now requires singletons 3.0 and above, and GHC 9.2 and above.

Version 0.1.3.2
---------------

*August 25, 2019*

<https://github.com/mstksg/list-witnesses/releases/tag/v0.1.3.2>

*   Fixed overlapping instance issues resulting from ambiguities in `Auto`
    instances for `Insert`, `Delete`, `Substitute`, `Suffix`, `Interleave`, and
    `Subset`.

Version 0.1.3.1
---------------

*August 23, 2019*

<https://github.com/mstksg/list-witnesses/releases/tag/v0.1.3.1>

*   Quick renaming of `subsetToInterlaveLeft` and `subsetToInterleaveRight` to
    match naming conventions.

Version 0.1.3.0
---------------

*August 23, 2019*

<https://github.com/mstksg/list-witnesses/releases/tag/v0.1.3.0>

*   Add `Subset` and associated methods
*   `interleaveShapes`, `swapInterleave`, `appendShape`, `prefixShape`

Version 0.1.2.0
---------------

*August 12, 2019*

<https://github.com/mstksg/list-witnesses/releases/tag/v0.1.2.0>

*   Add predicates (`IsInsert`, `IsPrefix`, etc.) and `Auto` and `Decidable`
    instances for most of the data types, for auto-generation and searches.
*   Add some functions for creating `Append`s and witnesses of concatenation
    type families from `Append`s.
*   `interleavedIxes`, for more manipulation of `Interleave`

Version 0.1.1.1
---------------

*August 3, 2019*

<https://github.com/mstksg/list-witnesses/releases/tag/v0.1.1.1>

*   Add *microlens* as a dependency, and use actual type synonyms for lenses.
    Also got rid of re-implementations of over and view.

Version 0.1.1.0
---------------

*March 7, 2019*

<https://github.com/mstksg/list-witnesses/releases/tag/v0.1.1.0>

*   Add `Interleave` to *Data.Type.List.Sublist*.

Version 0.1.0.0
---------------

*March 6, 2019*

<https://github.com/mstksg/list-witnesses/releases/tag/v0.1.0.0>

*   Initial release

