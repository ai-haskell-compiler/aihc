# Changelog

`pandoc-lua-marshal` uses [PVP Versioning][].

## 0.3.2.1

Released 2026-01-09.

  * Added missing entry `test/test-table.lua` to
    *extra-source-files* in cabal file.

## 0.3.2

Released 2026-01-08.

  * Added a docstring to the `Pandoc.normalize` method.

  * Added `TableBody` constructor. (Sean Soon)

  * Fixed comments in files `TableFoot.hs` and `TableHead.hs`
    (Sean Soon).

  * Allow hslua-2.5 and containers-0.8.\*.

## 0.3.1

Released 2025-06-23.

  * Fix docs of the `Cite` constructor function

  * Relax upper bounds for hslua and QuickCheck.

  * Dropped support for GHC 8.6.

## 0.3.0

Released 2024-12-07.

  * Add method `normalize` to Pandoc objects.
    This returns a normalized document by merging adjacent spaces in inlines
    and by modifying tables.

  * Push Captions as userdata, move code to separate module.

  * Add tests for RawInline and its properties

  * Allow treatment of custom elements as singleton lists.

  * Remove `pandoc` prefix on table components (jgm/pandoc#8574).

## 0.2.9

Released 2024-10-01.

-   Update list module, thereby introducing a new method `iter`;
    the function returns an iterator that steps through list
    values on each call.

-   Added support for `__toinline` and `__toblock` metamethods. If
    the metamethods are available on a Lua object and return an
    appropriate value, then that returned value will be used
    during unmarshalling.

## 0.2.8

Released 2024-09-21.

-   Update list module, thereby introducing a new method `at` and
    an extended constructor for List types.

## 0.2.7.1

Released 2024-07-02.

-   Relaxed the upper bound of tasty-quickcheck, used in tests.
    Now allows version 0.11.

-   Update documentation on constructors; the descriptions now
    match those in the pandoc docs.

## 0.2.7

Released 2024-05-06.

-   Let the behavior of `content` attributes on BulletList and
    OrderedList elements match that of the constructor by treating
    a list of Block elements as a list of single-block items. The
    following assertion now holds true:

    ``` lua
    local content = {pandoc.Plain "one", pandoc.Plain "two"}
    local bl = pandoc.BulletList{}
    bl.content = content
    assert(bl == pandoc.BulletList(content))
    ```

## 0.2.6

Released 2024-03-29.

-   Fixed a bug that caused problems with empty Block lists in
    the `content` attributes of *Div*, *Figure*, and *BlockQuote*
    elements.

## 0.2.5

Released 2024-03-04.

-   The `clone` method on *Blocks* and *Inlines* elements now
    creates deep copies of the lists.

## 0.2.4

Released 2024-01-19.

-   Relaxed upper bound for aeson, allowing aeson-2.2.\*.

## 0.2.3

Released 2024-01-19.

-   Relaxed upper bound for text, containers, and bytestring,
    allowing text-2.1, containers-0.7, and bytestring-0.12.

## 0.2.2

Released 2023-03-15.

-   Add `__tostring` metamethods to *Blocks* and *Inlines*.

## 0.2.1.1

Released 2023-03-13.

-   The version constraints for hslua packages have been relaxed;
    other changes in hslua 2.3.\* do not affect this package.

## 0.2.1

Released 2023-02-11.

-   All userdata types have been given a `__tojson` metamethod.
    The methods return the default JSON representations of AST
    objects.

## 0.2.0

Released 2023-01-18.

-   Depend on pandoc-types 1.23: the `Null` Block constructor has
    been removed, and a `Figure` constructor has been added.

-   Support for Lua 5.3 has been dropped; the package now requires
    hslua 2.2 or later.

-   The implementation for `List` has been moved to the separate
    `hslua-list` module. This module no longer contains C code.

## 0.1.7

Released 2022-07-16.

-   Allow Blocks to be passed as Caption value. The resulting
    caption has the Blocks as its long version and no short
    version.

-   Add `clone` method to Pandoc elements.

## 0.1.6.1

Released 2022-06-10.

-   Provide better error messages when fuzzy retrieval of Inlines
    or Blocks fails.

-   Relax upper bound for text, allow text-2.0.

## 0.1.6

Released 2022-06-03.

-   Fix `applyFully`: the function always traversed the document
    type-wise, never topdown.

-   Avoid shadowing of a function name that was added in
    hslua-2.2.

-   Support concatenating of Pandoc values with the `..` operator.

## 0.1.5.1

Released 2022-02-19.

-   Relax upper bound for lua and hslua.

## 0.1.5

Released 2022-02-17.

-   Allow any type of callable object as argument to List
    functions `filter`, `map`, and `find_if`. These previously
    required the argument to be of type `function`, which was too
    restrictive.

-   Inline: the type of Image captions is now `Inlines` instead
    of `List`.

## 0.1.4

Released 2022-01-29.

-   Export AttributeList type and marshaling functions from
    `Text.Pandoc.Marshal.Attr`, namely `typeAttributeList`,
    `peekAttributeList`, and `pushAttributeList`.

-   Update to hslua 2.1, making use of the new utility functions.

## 0.1.3.1

Released 2022-01-14.

-   Fixed a bug in `List.include` that was causing the Lua stack
    to overflow when the function was applied to long lists.

## 0.1.3

Released 2021-12-23.

### Lua changes

-   The traversal order of filters can now be selected by setting
    the key `traverse` to either `'topdown'` or `'typewise'`; the
    default remains `'typewise'`.

    Topdown traversals can be cut short by returning `false` as a
    second value from the filter function. No child-element of
    the returned element is processed in that case.

-   All types can be compared. Previously, comparing values of
    different types would lead to errors in a number of cases.

-   Lists now have an `__eq` metamethod. List equality is checked
    by comparing both lists element-wise. Two lists are equal if
    they have the same type and have equal elements.

-   If start indices in `List:find` and `List:find_if` are
    negative the start index is relative to the list length.

-   TableFoot, TableHead, and Row values are marshaled as
    userdata objects.

### Haskell code

-   Text.Pandoc.Lua.Marshal.Filter exports the new type
    `WalkingOrder`. The type `Filter` now contains the the
    traversal specifier as a field.

-   New modules for TableFoot, TableHead, and Row, defining the
    usual marshaling methods and constructor functions for these
    types.

## 0.1.2

Released 2021-12-10.

-   Restored backward compatible retrieval of Rows. Cells can be
    either a userdata value or a table.

## 0.1.1

Released 2021-12-10.

### Behavior of Lua objects

-   Lists of Inline values and lists of Block values are now
    pushed with their own metatables (named "Inlines" and
    "Blocks").

-   The types `Block`, `Blocks`, `Inline`, `Inlines`, and
    `Pandoc` now all have a method `walk` that applies a filter
    to the document subtree.

-   Changed behavior for *Cell* values: these are now pushed as
    userdata; the old table-based structure is still accepted
    when retrieving a Cell from the stack.

### Haskell code

-   Module Text.Pandoc.Lua.Marshal.Cell exports the constructor
    function `mkCell`, the type definition `typeCell` and the
    fuzzy peeker `peekCellFuzzy`.

-   Added a new module `Text.Pandoc.Lua.Marshal.Filter` that
    handles Lua filters.

-   Added functions for filtering:

    -   Module Text.Pandoc.Lua.Marshal.Block:
        -   `walkBlockSplicing`: walk an AST element, applying a
            filter on each Block and splicing the result back
            into the list.
        -   `walkBlocks`: walk an AST element, modifying lists of
            Block elements by applying the `Blocks` filter
            function.
    -   Module Text.Pandoc.Lua.Marshal.Inline:
        -   `walkInlineSplicing`: walk an AST element, applying a
            filter on each Inline and splicing the result back
            into the list.
        -   `walkInlines`: walk an AST element, modifying lists
            of Inline elements by applying the `Inlines` filter
            function.

    -   Module Text.Pandoc.Lua.Marshal.Pandoc:
        -   `applyFully`: fully apply a filter on a Pandoc
            document.

-   New internal modules:

    -   Text.Pandoc.Lua.SpliceList: defines a helper type used to
        walk a list of elements in a way that replaces the
        element by splicing the function result back into the
        list.

        The module is a slight rewrite of pandoc’s
        `SingletonsList`.

    -   Text.Pandoc.Lua.Walk: handles walking of the document
        tree while modifying elements via filter functions. This
        is a re-implementation of large parts of pandoc’s
        T.P.Lua.Filter module.

    -   Text.Pandoc.Lua.Marshal.Shared: provides helper functions
        used in multiple Lua type definitions.

## 0.1.0.1

Released 2021-11-28.

-   Added test-simpletable.lua to the list of extra-source-files.

## 0.1.0

Released 2021-11-28.

-   Released into the wild. May it live long and prosper.

  [PVP Versioning]: https://pvp.haskell.org
