5.4 (2024-01-26)

  * Fix for build failure with GHC 9.8.1


5.3 (2024-01-22)

  * Fixed a non-exhaustive pattern match
  * Removed unused imports and made most imports explicit
  * Alphabetized exports for this module
  * Used a better name than 'meta' in some code
  * Metadata formatting indent changed from 3 spaces to 2
  * Fixed parse failure for >1 source element
  * Made most imports explicit


5.2 (2023-08-09)

  * Fixed import issues with GHC 9.6.2 related to monad transformer functions


5.1 (2023-07-09)

  * Fixed an sdist problem with missing autogen-modules


5.0 (2023-07-09)

  * Fixed some problems with extra-source-files
  * Moved Stackage snapshot up to lts-20.12
  * Switched from hpack to cabal v2.2
  * Fixed all copyright date issues
  * Made Data.List imports explicit
  * Completely overhauled how dates are handled
  * Added another EPUB3 dcterms date type


4.5 (2016-10-19)

   * Fix merged for UTF8 issue


4.4 (2016-10-11)

   * Removed defunct cabal stability field
   * Moved copyright dates up to 2016
   * Moved tested-with up to GHC 8.0.1
   * Removed GHC switch simpl-tick-factor
   * Moved cabal-version up to >= 1.10
   * Project now builds with stack
   * Fixed failing damaged zip file unit test


4.3 (2015-05-25)

   * Replaced deprecated Control.Monad.Error with
     Control.Monad.Except
   * Updated cabal homepage, tested-with and source-repository
   * Cleaned out ui3.info stuff from the README
   * Updated boringfile with cabal sandbox filespecs
   * TODO is now a Markdown document


4.2 (2014-05-09)

   * Added many files missing from extra-source-files


4.1 (2014-05-04)

   * Fix for Simplifier ticks exhausted problem that was exposed
     after upgrade to GHC 7.8.2
   * Added README.md and changelog.md files, better docs!
   * Some documentation fixes
   * Fixed a bug reading books where the container XML document is malformed


4.0 (2013-09-20)

   * Added support for epub3 documents. This was done using a single
     set of datatypes, not specific to either epub2 or epub3.
   * Redesigned the book examining API to be an edsl. Actions are to be
     combined together based on what the developer needs from the document.
   * The data structures to contain epub metadata "sections" were
     redesigned to no longer be nested. Part of this change includes a
     typeclass-based pretty-print API for displaying this data.
   * Documentation rewrites and additions, including a working code
     example in the API docs.


3.0 (2013-04-13)

   * Repackaged this code to show that it's for epub v2.x only at
     this time
   * Fixed a bug where mimetype file not occurring first in the archive
     on Windows
   * Fixed support for all 0-or-more epub2 metadata fields
   * Redesigned the Format (metadata display) code
   * Modularized the unit tests so that more tests could be added that
     aren't specifically about parsing metadata.
   * Cosmetic updates to the epub metadata API including some
     documentation clean-up


2.3.2 (2013-02-01)

   * Added code to strip illegal characters from the beginning of the
     OPF metadata XML file
   * Switched the encoding hack to be case-insensitive


2.3.1 (2012-10-31)

   * Added a fix for epub zip files with a damaged central directory
     signature


2.3.0 (2012-01-29)

   * Encountered a problem in Windows where using a lazy ByteString
     to read the entire epub zip file was never closing the file while
     consumer applications were still running. Changed this library to
     use a strict ByteString instead for reading the zip archive.


2.2.0.1 (2011-10-27)

   * Changed display output of OPF data to label the textual content of
     a tag as "text"
   * Extensive changes to the cabal build of this project to bring it
     up to Cabal 1.10
   * Unit tests now use the test-suite cabal stanza


2.2.0.0 (2011-04-20)

   * Now includes some API functions for working directly with zip
     archives and directories full of the loose files which will be made
     into epubs
   * Moved the epubmeta utility from this project. It's now in epub-tools


2.1.0 (2011-02-13)

   * Project now uses zip-archive instead of the libzip
     library. zip-archive is a pure Haskell solution that's easier to
     build on non-UNIX-like platforms
   * Other changes to the build and project to make it buildable under
     Windows


2.0.2 (2011-01-10)

   * This library now uses the haskell LibZip library instead of relying
     on unzip shell command invocation
   * Changed command-line utility name from epub-meta to epubmeta


2.0.1 (2011-01-08)

   * Now always using namespace qualification for all OPF Package
     tags. Library would incorrectly fail if the OPF document was created
     using explicit namespacing for the OPF package elements.
   * Hack to avoid problems with UTF-8 encoding in OPF Package XML
     documents
   * Hack to avoid problems with !DOCTYPE declarations. Found some
     books where a DTD was specified for a completely different type of
     document. Discarding these declarations for now.
   * Changed this code to use System.Process instead of HSH for unzip
     invocation in order to have more control over interpreting exit
     codes.


2.0.0 (2011-01-02)

   * Added parsing and output of the manifest, spine and guide portions
     of the spec. Full read capability of OPF Package data is now
     complete.
   * Major redesign of the API. It's been split and organized into
     smaller, more focused modules.
   * Updated to work with the 9.x version of hxt
   * Redesigned epub metadata formatting code to be much more efficient
     using Writer monad and Data.Sequence
   * Added support for new capabilities to command-line utility
   * Clarified the Haddock documentation to describe how these data
     types map to the source XML data
   * Updated unit tests to reflect the large API changes


1.0.2 (2010-05-11)

   * Initial release
