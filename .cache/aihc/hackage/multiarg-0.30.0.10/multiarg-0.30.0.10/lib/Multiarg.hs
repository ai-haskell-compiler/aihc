-- | Parse command lines with /options/ that might take multiple
-- /option arguments/.
--
-- I built this library could not find anything that would readily
-- parse command lines where the /options/ took more than one
-- /option argument/. For example, for the @tail@ command on GNU systems, the
-- @--lines@ /option/ takes one /option argument/ to specify how many
-- lines you want to see. Well, what if you want to build a program
-- with an option that takes two /option arguments/, like @--foo bar
-- baz@? I found no such library so I built this one.
--
-- Please consult the "Multiarg.Vocabulary" module to learn common
-- vocabulary used throughout Multiarg and its documentation.  Words
-- that appear in /italics/ are defined in "Multiarg.Vocabulary".
--
-- Use this module to build parsers for simple commands.  The
-- 'parseCommandLine' function runs in the IO monad and will cause
-- your program to exit unsuccessfully if there are any errors in the
-- command line, printing an error message in the process.  If you
-- want more control over error handling, use the "Multiarg.Internal"
-- module.
--
-- To write parsers for commands with multiple modes (for instance,
-- @ghc-pkg@ has multiple modes, such as @ghc-pkg list@ and @ghc-pkg
-- check@) use the "Multiarg.Mode" module.
--
-- You will find examples in "Multiarg.Examples.Telly" for non-mode
-- parsers, and in "Multiarg.Examples.Grover" for mode parsers.
module Multiarg
  ( ArgSpec(..)
  , OptSpec
  , optSpec
  , parseCommandLine
  ) where

import Multiarg.Internal
import Multiarg.Types
