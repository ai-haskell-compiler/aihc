# hslua-module-doclayout

[![GitHub CI][CI badge]](https://github.com/hslua/hslua-module-doclayout/actions)
[![Hackage][Hackage badge]](https://hackage.haskell.org/package/hslua-module-doclayout)
[![Stackage Lts][Stackage Lts badge]](http://stackage.org/lts/package/hslua-module-doclayout)
[![Stackage Nightly][Stackage Nightly badge]](http://stackage.org/nightly/package/hslua-module-doclayout)
[![MIT license][License badge]](LICENSE)

Lua module wrapping the [doclayout] Haskell package.

[doclayout]: https://hackage.haskell.org/package/doclayout
[CI badge]: https://github.com/hslua/hslua-module-doclayout/workflows/CI/badge.svg
[Hackage badge]: https://img.shields.io/hackage/v/hslua-module-doclayout.svg?logo=haskell
[Stackage Lts badge]: http://stackage.org/package/hslua-module-doclayout/badge/lts
[Stackage Nightly badge]: http://stackage.org/package/hslua-module-doclayout/badge/nightly
[License badge]: https://img.shields.io/badge/license-MIT-blue.svg


Example
-------

``` haskell
loadProg :: Lua Status
loadProg = do
  openlibs
  preloadModule "doclayout"
  dostring $ unlines
    [ "doc = require 'doclayout'"
    , "local example_doc = (doc.literal 'Line' + 'One')"
    , "                 // (doc.literal 'Line' + 'Two')"
    , "-- prints:"
    , "-- Line One"
    , "--"
    , "-- Line Two"
    , "print(doc.render(example_doc))"
    , ""
    , "-- prints:"
    , "-- Line"
    , "-- One"
    , "--"
    , "-- Line"
    , "-- Two"
    , "local columns = 5"
    , "print(doc.render(example_doc, columns))"
    ]
```


Documentation
-------------

### Functions

#### render

`render (doc[, colwidth[, style]])`

Render the `Doc` using the given column width.

Parameters:

`doc`
:   Doc to render

`colwidth`
:   Maximum number of characters per line

`style`
:   Whether to generate 'plain' or 'ANSI' terminal output. Must be
    either `'plain'` or `'ansi'`. Defaults to `'plain'`. (string)

### Doc construction

All functions return a fresh `Doc` element.

#### after_break

`after_break`

Creates a `Doc` which is conditionally included only if it
comes at the beginning of a line.

#### before_non_blank

`before_non_blank (doc)`

Conditionally includes the given `Doc` unless it is followed by
a blank space.

#### blankline

`blankline`

Inserts a blank line unless one exists already.

#### blanklines

`blanklines (n)`

Insert blank lines unless they exist already.

Parameters:

`n`
:   Number of blank lines to insert.

#### braces

`braces (doc)`

Puts a `Doc` in curly braces.

#### brackets

`brackets (doc)`

Puts a `Doc` in square brackets.

#### cblock

`cblock (width, doc)`

Like `lblock` but aligned centered.

Parameters:

`width`
:   Width of the created block, in characters

`doc`
:   Contents of the block ([Doc])

#### chomp

`chomp (doc)`

Chomps trailing blank space off of a `Doc`.

#### concat

`concat (docs[, sep])`

Concatenate the given `Doc`s, interspersing `sep` if specified.

Parameters:

`docs`
:   List of `Doc`s

`sep`
:   Separator `Doc`

#### cr

A carriage return. Does nothing if we're at the beginning of a
line; otherwise inserts a newline.

#### double_quotes

`double_quotes (doc)`

Wraps a `Doc` in double quotes

#### empty

The empty document.

#### flush

`flush (doc)`

Makes a `Doc` flush against the left margin.

#### hang

`hang (indent, start, doc)`

Creates a hanging indent.

Parameters:

`indent`
:   Indentation width in characters

`start`
:   Start, printed unindented

`doc`
:   Doc which is indented by `indent` spaces on every line.

#### inside

`inside (start, end, contents)`

Encloses a `Doc` inside a start and end `Doc`.

Parameters:

`start`
:   Doc before contents

`end`
:   Doc after contents

`contents`
:   Contents Doc

#### lblock

`lblock (width, doc)`

Creates a block with the given width and content, aligned to the left.

Parameters:

`width`
:   Width of the created block, in characters

`doc`
:   Contents of the block ([Doc])


#### literal

`literal (string)`

Creates a `Doc` from a string.

#### nest

`nest (indent)`

Indents a `Doc` by the specified number of spaces.

Parameters:

`indent`
:   Indentation width.

#### nestle

`nestle (doc)`

Removes leading blank lines from a `Doc`.

#### nowrap

`nowrap (doc)`

Makes a `Doc` non-reflowable.

#### parens

`parens (doc)`

Puts a `Doc` in parentheses.

#### prefixed

`prefixed (prefix, doc)`

Uses the specified string as a prefix for every line of the
inside document (except the first, if not at the beginning of the
line).

Parameters:

`prefix`
:   Prefix to prepend to each line

`doc`
:   Inside Doc.

#### quotes

`quotes (doc)`

Wraps a `Doc` in single quotes.

#### rblock

`rblock (indent, doc)`

Like `lblock` but aligned to the right.

Parameters:

`width`
:   Width of the created block, in characters

`doc`
:   Contents of the block ([Doc])

#### space

A breaking (reflowable) space.

#### vfill

`vfill`

Creates an expandable border that, when placed next to a box,
expands to the height of the box.

Parameters:

`text`
:   Border text

### Operators

#### `..`

Concatenate two `Doc` elements.

#### `+`

Concatenate two `Doc`s, inserting a reflowable space between them.

#### `/`

If `a` and `b` are `Doc` elements, then `a / b` puts `a` above `b`.

#### `//`

If `a` and `b` are `Doc` elements, then `a // b` puts `a` above
`b`, inserting a blank line between them.

### Document Querying

#### is_empty

`is_empty (doc)`

Returns `true` iff `doc` is the empty document, `false` otherwise.

#### min_offset

`min_offset (doc)`

Returns the minimal width of a @'Doc'@ when reflowed at breakable
spaces.

#### update_column

`update_column (doc, i)`

Returns the column that would be occupied by the last laid out character.

#### height

`height (doc)`

Returns the height of a block or other Doc.

#### real_length

`real_length (str)`

Returns the real length of a string in a monospace font: 0 for a
combining character, 1, for a regular character, 2 for an East
Asian wide character.

#### offset

`offset (doc)`

Returns the width of a `Doc` (as number of characters).


License
-------

This package is made available under the MIT license. See [`LICENSE`](LICENSE)
for details.
