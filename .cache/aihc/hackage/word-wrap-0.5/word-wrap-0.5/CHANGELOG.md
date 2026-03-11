
0.5
===

API changes:
 * Line-wrapping now supports optional "filling", i.e., placing prefix
   strings at the start of wrapped lines. The behavior of filling is
   configured by new `WrapSettings` fields: `fillStrategy` (what to
   add to filled lines) and `fillScope` (which lines to affect with
   filling). Thanks to Brent Yorgey for this work!

0.4.1
=====

Bug fixes:
 * Fixed a bug that caused breakTokens to diverge for lines with
   indentation longer than the indentation width when
   preserveIndentation was enabled. (Thanks Callum Oakley.) The
   resulting fix does the following:
   * When breakLongWords is enabled, this change reduces the indentation
     of the indented lines to result in lines that are no longer than
     the wrap limit, so they will have reduced indentation and word
     fragments. This is a trade-off with other options that are open to
     evaluation.
   * When breakLongWords is disabled, this change reduces the
     indentation of the indented lines and leaves whole words, unbroken,
     on them, resulting in lines that are longer than the indentation
     limit. This behavior is similar to non-indented lines with
     over-long tokens. This is also a trade-off with other options that
     are open to evaluation.

0.4
===

Bug fixes:
 * Fixed a bug where each line was being wrapped after every word
   because only one case in breakTokens was reached (thanks Callum
   Oakley)

Package changes:
 * Added a simple benchmark suite

0.3.3
=====

Bug fixes:
 * Fixed accidental breaking of long tokens when they could be wrapped
   instead.

0.3.2
=====

Bug fixes:
 * Fixed a bug that prevented wrapping sometimes.

0.3.1
=====

Bug fixes:
 * Fix inconsistent long token breaking (long tokens anywhere but the
   beginning of a line)

0.3
===

API changes:
 * Added the breakLongWords setting to WrapSettings. This setting makes
   it possible to cause words to get broken up over multiple lines if
   their lengths exceed the wrapping width.

0.2
===

API changes:
 * Added a WrapSettings data type for controlling wrapping behavior.
 * All functions now require a WrapSettings.
 * Added defaultWrapSettings for prior behavior.
 * Wrap settings now include a setting to control how indentation is
   preserved in broken lines.

Bug fixes:
 * Lines with only whitespace are preserved as empty lines.

0.1.2
=====

Bug fixes:
 * Fixed a bug where multiple consecutive newlines were not properly preserved
   as advertised (#2)

0.1.1
=====

Package changes:
 * Removed a duplicate mention of the changelog file in the cabal
   package description that used the wrong filename case (#1)

0.1
===

* First version.
