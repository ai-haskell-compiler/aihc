# Formatter Comment Attachment

This document records the comment-placement strategies used by current Haskell
formatters and the tradeoffs for `aihc-fmt`.

## Ormolu

Ormolu extracts comments from GHC parser annotations into a `CommentStream`,
then consumes that stream while rendering located AST nodes. The package
description explicitly lists `Ormolu.Parser.CommentStream` and
`Ormolu.Printer.Comments` as public implementation modules, and states core
goals including GHC-parser-based parsing, idempotence, robustness, and comment
preservation.

The comment stream normalizes line and block comments, removes valid Haddock
comments from the ordinary stream, handles stack script headers separately, and
extracts pragmas with comments associated to the same pragma line. During
printing, the `located` combinator is the main attachment point: it emits
preceding comments, renders the AST node with an enclosing span, and then emits
following comments. Following-comment attachment checks whether the comment is
on the same line, continues a previous comment block, is the last element in an
enclosing construct, and whether attaching it to the child would incorrectly
steal it from the parent based on column distance.

This design is high quality but depends heavily on a complete, precise span
stream and on every printer path consistently entering located nodes.

Sources:

- [Ormolu package/docs](https://hackage.haskell.org/package/ormolu)
- [Ormolu.Parser.CommentStream](https://raw.githubusercontent.com/tweag/ormolu/master/src/Ormolu/Parser/CommentStream.hs)
- [Ormolu.Printer.Comments](https://raw.githubusercontent.com/tweag/ormolu/master/src/Ormolu/Printer/Comments.hs)
- [Ormolu.Printer.Combinators](https://raw.githubusercontent.com/tweag/ormolu/master/src/Ormolu/Printer/Combinators.hs)

## Hindent

Hindent uses a relocation pass over GHC parser annotations. The pass starts from
the parser's comment collection and rewrites AST annotations so comments are
stored as prior or following comments on the nodes Hindent's printer expects.

The relocation pass contains specialized rules for pragmas, comments before
pragmas, export-list elements, class elements, case branches, do statements,
top-level declarations, same-line comments, top-level `where` clauses, and
remaining comments after nodes. It also scans annotations backwards for
following comments so inner nodes do not necessarily consume comments that
belong to enclosing syntax. This makes comment placement explicit and testable,
but it is coupled to GHC annotation shapes and requires special cases for
language constructs.

Sources:

- [Hindent package/docs](https://hackage.haskell.org/package/hindent)
- [HIndent.ModulePreprocessing.CommentRelocation](https://raw.githubusercontent.com/mihaimaruseac/hindent/master/src/HIndent/ModulePreprocessing/CommentRelocation.hs)

## Stylish Haskell

Stylish Haskell is a line-editing pipeline over a parsed GHC AST. Each step
receives the original lines and parsed module, then applies targeted textual
edits. This means comments are often preserved naturally because untouched lines
stay untouched.

The tradeoff is that steps that replace whole blocks can lose comments inside
those blocks. The import step explicitly notes that regrouping imports discards
blank lines and comments inside the import section. The module helper also notes
that merging imports loses the mapping between parser comments and import
declarations, because GHC annotation comments are not already attached to the
specific import nodes Stylish wants to manipulate.

Stylish Haskell therefore avoids many whole-program comment-placement problems
by editing only selected ranges, but that approach is a weaker fit for
`aihc-fmt`, whose MVP is a whole-module pretty-printer with semantic
round-trip checks.

Sources:

- [Stylish Haskell package/docs](https://hackage.haskell.org/package/stylish-haskell)
- [Stylish Haskell run pipeline](https://raw.githubusercontent.com/haskell/stylish-haskell/master/lib/Language/Haskell/Stylish.hs)
- [Stylish Haskell imports step](https://raw.githubusercontent.com/haskell/stylish-haskell/master/lib/Language/Haskell/Stylish/Step/Imports.hs)
- [Stylish Haskell module helpers](https://raw.githubusercontent.com/haskell/stylish-haskell/master/lib/Language/Haskell/Stylish/Module.hs)

## `aihc-fmt` MVP Direction

The MVP follows the Ormolu/Hindent principle that formatted output must be
reparsed and compared against the original AST before any output is printed or
written. The initial comment model is intentionally narrower:

- scan ordinary comments separately from the parser token stream;
- preserve language pragmas, module headers, imports, and declarations as
top-level render sections;
- attach comments before the next top-level section when they precede it;
- attach same-line comments to the section that ended on that line;
- preserve unsupported nested comments in source order, with xfail fixtures
documenting known placement gaps.

The likely next step is to move from section-level placement to AST-node
placement by extending parser annotations or adding a Hindent-style relocation
pass over `aihc-parser` spans.
