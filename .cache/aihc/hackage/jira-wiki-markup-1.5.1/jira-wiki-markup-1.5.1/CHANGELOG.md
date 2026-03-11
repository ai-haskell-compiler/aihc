Changelog
=========

`jira-wiki-markup` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

1.5.1
-----

Released 2023-03-13.

* Parser: Disallow image paths that start with a space.

1.5.0
-----

Released 2023-01-11.

* Relaxed upper bounds for mtl, allow mtl 2.3.

* Fixed handling of character sequences that would be interpreted
  as icons if they weren't followed by an alphanumeric character.
  See also [pandoc issue #8511][].

[pandoc issue #8511]: https://github.com/jgm/pandoc/issues/8511

1.4.0
-----

Released 2021-05-25.

* Allow quoted image parameters.

* Added support for "smart links".

* **API Change**: Add new constructors `SmartCard` and `SmartLink` to
  Text.Jira.Markup.LinkType.

1.3.5
-----

Released 2021-05-24.


* Allow spaces and most unicode characters in attachment links.

* No longer require a newline character after `{noformat}`.

* Only allow URI path segment characters in bare links.

* The `file:` schema is no longer allowed in bare links; these
  rarely make sense.

1.3.4
-----

Released 2021-03-13.

* Fixed parsing of autolinks (i.e., of bare URLs in the text).
  Previously an autolink would take up the rest of a line, as
  spaces were allowed characters in these items.
  
* Emoji character sequences no longer cause parsing failures. This
  was due to missing backtracking when emoji parsing fails.
  
* Block quotes are only rendered as `bq.` if they do not contain a
  linebreak.

1.3.3
-----

Released 2021-02-12.

* Modified the Doc parser to skip leading blank lines. This fixes
  parsing of documents which start with multiple blank lines.

* Prevent URLs within link aliases to be treated as autolinks.

1.3.2
-----

Released 2020-06-22.

* Braces are now always escaped when printing; Jira treats braces
  specially, regardless of context.

1.3.1
-----

Released 2020-06-14

* Added support for links to anchors.

* Styled text may not wrap across multiple lines; linebreaks in
  marked-up text are now forbidden.

* Module Text.Jira.Parser.Core: new function `many1Till` which
  behaves like `manyTill`, but requires at least on element to be
  parsed.

* Ensured the package works with GHC 8.10.

1.3.0
-----

Released 2020-04-04

* Support was added for additional syntax constructs:

    - citation markup (`??citation??`),
    - links to attachments (`[title^attachment.ext]`), and
    - user links (`[~username]`).

* Changes to module `Text.Jira.Markup`:

    * A new data type `LinkType` is exported from the module.

    * Changes to type `Inline`:

        - a new constructor `Citation` has been added;
        - the `Link` constructor now takes an additional
          parameter of type `LinkType`.

1.2.1
-----

Released 2020-04-02

* Fixed rendering of image attributes: image attributes are
  separated by commas instead of pipes; the latter are used in
  block parameters.

* Fixed parsing of blockquotes which are not preceeded by blank
  lines.

* Ensure parsing of single-line blockquotes is possible even if
  there is no space between `bq.` marker and contents.

* Fixed parsing of colors: parsing no longer fails for hexcolors
  which contain non-decimal digits.

* Changes to module `Text.Jira.Parser.Shared`:

    - New parsing function `colorName` which parses a color
      descriptor, i.e. either a name or a hexcolor.

1.2.0
-----

Released 2020-03-28

* Added check that a closing markup char is not preceeded by a
  whitespace character. Previously, plain text was still
  incorrectly treated as markup. E.g., the dashes in `-> step ->`
  used to be interpreted as delimiters marking deleted text.

* Allows empty table cells; table parsing failed if one of the
  cells did not contain any content.

* Changes to module `Text.Jira.Parser.Core`:

    - A field `stateLastSpcPos` was added to data type
      `ParserState` to keep track of spaces.
    - Function `updateLastSpcPos` was added to update the
      aforementioned field.
    - Function `afterSpace` was added to test the field.

1.1.4
-----

Released 2020-03-27

* Fixed parsing of image parameters. Thumbnails and images with
  parameters were previously not recognized as images.

1.1.3
-----

Released 2020-03-19

* Fixed table detection in endOfParagraph parser: Tables were
  expected to have a space between the leading pipe(s) and the
  cell content. Lines like `||Name|` were erroneously not
  recognized as the beginning of a new block.

1.1.2
-----

Released 2020-03-18

* Don't escape colon/semicolon unless necessary: it is necessary
  to escape colons or semicolons only if they could otherwise
  become part of a smiley.

1.1.1
-----

Released 2020-03-18

* Colon `:` and semicolon `;` are now parsed as special
  characters, since they can be the first characters of an emoji.
* Fixed parsing of words which contain non-special symbol
  characters: word boundaries were not set correctly if a word
  contained a dot `.` or similar chars.
* Fixed incorrect emphasis parsing: digits were erroneously
  allows as the first characters after closing emphasis
  characters.

1.1.0
-----

Released 2020-03-13.

* Lists are now allowed to be indented; i.e., lists are still recognized
  if list markers are preceded by spaces.
* Support for colored inlines has been added.
* New constructor `ColorInline` for type `Inline` (API change).

1.0.0
-----

Released 2019-12-17.

* Add `Doc` datatype representing a full document; `parse` now returns
  this type.
* Improve parsing:
  - double-backslash is recognized as linebreak;
  - emoticons are parsed as `Emoji`;
  - special sequences of dashes are translated into their unicode
    representation;
  - naked URLs are parsed as `AutoLink`;
  - blocks of colored text are parsed as `Color`;
  - interpretation of special characters as markup can be forced by
    surrounding them with curly braces.
* A parser `plainText` was made available to read markup-less text.
* *Inline*-parser `symbol` was renamed to `specialChar`.
* Add printer module to render the document AST as Jira markup.
* Markup datatype changes:
  - new *Block* elements `Color` and `HorizontalRule`.
  - new *Inline* elements `Emoji`, and `Styled`.
  - *Inline* constructors `Subscript`, `Superscript`, `Emph`, `Strong`,
    `Inserted`, and `Deleted` have been remove. Use `Styled` instead.
  - Constructor `Image` now takes a list of parameters as an additional
    argument.
* CI runs also test GHC 8.8.

0.1.1
-----

* Ensure proper parsing of backslash-escaped characters.

0.1.0
-----

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/tarleb/jira-wiki-markup/releases
