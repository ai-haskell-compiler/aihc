{-# LANGUAGE NoImplicitPrelude #-}
{-|
For the most part, the data constructors of 'Style' do not clash with other
names. When they do, import the module qualified. For example:

> import qualified RIO.PrettyPrint.Types.PrettyPrint as PP
-}
module RIO.PrettyPrint.Types
  ( Style (..)
  , Styles
  , StyleSpec
  ) where

import           Data.Array.IArray ( Array )
import           Data.Ix ( Ix )
import           Data.Text ( Text )
import           RIO
import           System.Console.ANSI.Types ( SGR )

-- | Type representing styles of output.
data Style
  = Error
    -- ^ Intended to be used sparingly, not to style entire long messages. For
    -- example, to style the @Error:@ or @[error]@ label for an error message,
    -- not the entire message.
  | Warning
    -- ^ Intended to be used sparingly, not to style entire long messages. For
    -- example, to style the @Warning:@ or @[warn]@ label for a warning message,
    -- not the entire message.
  | Info
    -- ^ Intended to be used sparingly, not to style entire long messages. For
    -- example, to style the @[info]@ label for an info message, not the entire
    -- message.
  | Debug
    -- ^ Intended to be used sparingly, not to style entire long messages. For
    -- example, to style the @[debug]@ label for a debug message, not the entire
    -- message.
  | OtherLevel
    -- ^ Intended to be used sparingly, not to style entire long messages. For
    -- example, to style the @[...]@ label for an other log level message, not
    -- the entire message.
  | Good
    -- ^ Style in a way to emphasize that it is a particularly good thing.
  | Shell
    -- ^ Style as a shell command, i.e. when suggesting something to the user
    -- that should be typed in directly as written.
  | File
    -- ^ Style as a filename. See 'Dir' for directories.
  | Url
    -- ^ Style as a URL.
  | Dir
    -- ^ Style as a directory name. See 'File' for files.
  | Recommendation
    -- ^ Style used to highlight part of a recommended course of action.
  | Current
    -- ^ Style in a way that emphasizes that it is related to a current thing.
    -- For example, to report the current package that is being processed when
    -- outputting the name of it.
  | Target
    -- ^ Style used the highlight the target of a course of action.
  | Module
    -- ^ Style as a module name.
  | PkgComponent
    -- ^ Style used to highlight the named component of a package.
  | Secondary
    -- ^ Style for secondary content. For example, to style timestamps.
  | Highlight
    -- ^ Intended to be used sparingly, not to style entire long messages. For
    -- example, to style the duration in a @Finished process in ... ms@ message.
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)

-- | The first style overrides the second.
instance Semigroup Style where
  s <> _ = s

-- | A style specification, pairing its \'key\' with the corresponding list of
-- 'SGR' codes.
type StyleSpec = (Text, [SGR])

-- | Style specifications indexed by the style.
type Styles = Array Style StyleSpec
