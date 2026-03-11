0.2.2
===

- GHC 9.14.1 support
- removed string-interpolate dependency
- use ByteString concatenation instead

0.2.1
===

- added Data instances


0.2.0
===

- switched to Diff for testing round-trips
- removed MarkupParse.Patch
- renamed MarkupParse.FlatParse to MarkupParse.Internal.FlatParse and removed unused exports. Added non-stable note.
- moved ParserWarn, runParser_ & runParserWarn to MarkupParse.
- changed diff executable to markupparse-diff.

0.1.1
===

- fixed escape bug
- bumped tasty & text upper bounds

0.1.0
===

- Added RenderStyle
- rename: Warn <== Result
- refactored Markup structure
- created OpenTagType
- rename: NameTag <== TagName
- Element type synonym for Tree Token
- suffixed parsers with a 'P'

0.0.0
===

Initial split away from chart-svg
