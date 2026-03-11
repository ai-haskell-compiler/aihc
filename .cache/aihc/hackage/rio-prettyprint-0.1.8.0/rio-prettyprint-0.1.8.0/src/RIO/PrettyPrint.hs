{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module RIO.PrettyPrint
  (
    -- * Type classes for optionally colored terminal output
    HasTerm (..)
  , HasStylesUpdate (..)
    -- * Pretty printing functions
  , displayPlain
  , displayWithColor
    -- * Logging based on pretty-print typeclass
    -- | The @pretty...@ functions come in three varieties:
    --
    -- * The normal variety, with a single styled document;
    -- * The @L@ variety. The listed styled documents are concatenated with
    --   'fillSep'; and
    -- * The @S@ variety. 'flow' is applied to the 'String'.
    --
    -- Pretty message at log level 'LevelDebug'.
  , prettyDebug
  , prettyDebugL
  , prettyDebugS
    -- | Pretty message at log level 'LevelInfo'.
  , prettyInfo
  , prettyInfoL
  , prettyInfoS
    -- | Pretty messages at log level 'LevelInfo', starting on a new line with
    -- label @Note:@, with the message indented after the label.
  , prettyNote
  , prettyNoteL
  , prettyNoteS
    -- | Pretty messages at log level 'LevelWarn', starting on a new line with
    -- label @Warning:@, with or without the message indented after the label.
  , prettyWarn
  , prettyWarnL
  , prettyWarnS
  , prettyWarnNoIndent
  , prettyWarnNoIndentL
  , prettyWarnNoIndentS
    -- | Pretty messages at log level 'LevelError', starting on a new line with
    -- label @Error:@, with or without the message indented after the label.
  , prettyError
  , prettyErrorL
  , prettyErrorS
  , prettyErrorNoIndent
  , prettyErrorNoIndentL
  , prettyErrorNoIndentS
    -- | Pretty messages at the specified log level.
  , prettyGeneric
  , prettyWith

    -- * Semantic styling functions
    -- | These are used rather than applying colors or other styling directly,
    -- to provide consistency.
  , style
  , displayMilliseconds
  , logLevelToStyle
    -- * Formatting utils
  , blankLine
  , bulletedList
  , spacedBulletedList
  , mkBulletedList
  , mkNarrativeList
  , debugBracket
    -- * Re-exports from "Text.PrettyPrint.Leijen.Extended"
  , Pretty (..)
  , StyleDoc (..)
  , StyleAnn (..)
  , nest
  , line
  , linebreak
  , group
  , softline
  , softbreak
  , align
  , hang
  , indent
  , encloseSep
  , (<+>)
  , hsep
  , vsep
  , fillSep
  , sep
  , hcat
  , vcat
  , fillCat
  , cat
  , punctuate
  , fill
  , fillBreak
  , enclose
  , squotes
  , dquotes
  , parens
  , angles
  , braces
  , brackets
  , string
  , indentAfterLabel
  , wordDocs
  , flow
    -- * Re-exports from "RIO.PrettyPrint.Types.PrettyPrint"
  , Style (..)
  ) where

import           Data.List ( intersperse )
import           RIO
import           RIO.PrettyPrint.StylesUpdate ( HasStylesUpdate (..) )
import           RIO.PrettyPrint.Types ( Style (..) )
import           Text.PrettyPrint.Leijen.Extended
                   ( Pretty (pretty), StyleAnn (..), StyleDoc, (<+>), align
                   , angles, braces, brackets, cat, displayAnsi, displayPlain
                   , dquotes, enclose, encloseSep, fill, fillBreak, fillCat
                   , fillSep, group, hang, hcat, hsep, indent, line, linebreak
                   , nest, parens, punctuate, sep, softbreak, softline, squotes
                   , string, styleAnn, vcat, vsep
                   )

class (HasLogFunc env, HasStylesUpdate env) => HasTerm env where
  useColorL :: Lens' env Bool
  termWidthL :: Lens' env Int

displayWithColor ::
     (HasTerm env, Pretty a, MonadReader env m, HasCallStack)
  => a
  -> m Utf8Builder
displayWithColor x = do
  useAnsi <- view useColorL
  termWidth <- view termWidthL
  (if useAnsi then displayAnsi else displayPlain) termWidth x

-- TODO: switch to using implicit callstacks once 7.8 support is dropped

prettyGeneric ::
     (HasTerm env, HasCallStack, Pretty b, MonadReader env m, MonadIO m)
  => LogLevel
  -> b
  -> m ()
prettyGeneric level = prettyWith level id

prettyWith ::
     (HasTerm env, HasCallStack, Pretty b, MonadReader env m, MonadIO m)
  => LogLevel
  -> (a -> b)
  -> a
  -> m ()
prettyWith level f = logGeneric "" level . RIO.display <=< displayWithColor . f

-- Note: I think keeping this section aligned helps spot errors, might be
-- worth keeping the alignment in place.

prettyDebugWith, prettyInfoWith, prettyNoteWith, prettyWarnWith, prettyErrorWith, prettyWarnNoIndentWith, prettyErrorNoIndentWith
  :: (HasCallStack, HasTerm env, MonadReader env m, MonadIO m)
  => (a -> StyleDoc) -> a -> m ()
prettyDebugWith = prettyWith LevelDebug
prettyInfoWith  = prettyWith LevelInfo
prettyNoteWith f  = prettyWith LevelInfo
                          ((line <>) . (style Good "Note:" <+>) .
                           indentAfterLabel . f)
prettyWarnWith f  = prettyWith LevelWarn
                          ((line <>) . (style Warning "Warning:" <+>) .
                           indentAfterLabel . f)
prettyErrorWith f = prettyWith LevelError
                          ((line <>) . (style Error   "Error:" <+>) .
                           indentAfterLabel . f)
prettyWarnNoIndentWith f  = prettyWith LevelWarn
                                  ((line <>) . (style Warning "Warning:" <+>) . f)
prettyErrorNoIndentWith f = prettyWith LevelError
                                  ((line <>) . (style Error   "Error:" <+>) . f)

prettyDebug, prettyInfo, prettyNote, prettyWarn, prettyError, prettyWarnNoIndent, prettyErrorNoIndent
  :: (HasCallStack, HasTerm env, MonadReader env m, MonadIO m)
  => StyleDoc -> m ()
prettyDebug         = prettyDebugWith         id
prettyInfo          = prettyInfoWith          id
prettyNote          = prettyNoteWith          id
prettyWarn          = prettyWarnWith          id
prettyError         = prettyErrorWith         id
prettyWarnNoIndent  = prettyWarnNoIndentWith  id
prettyErrorNoIndent = prettyErrorNoIndentWith id

prettyDebugL, prettyInfoL, prettyNoteL, prettyWarnL, prettyErrorL, prettyWarnNoIndentL, prettyErrorNoIndentL
  :: (HasCallStack, HasTerm env, MonadReader env m, MonadIO m)
  => [StyleDoc] -> m ()
prettyDebugL         = prettyDebugWith         fillSep
prettyInfoL          = prettyInfoWith          fillSep
prettyNoteL          = prettyNoteWith          fillSep
prettyWarnL          = prettyWarnWith          fillSep
prettyErrorL         = prettyErrorWith         fillSep
prettyWarnNoIndentL  = prettyWarnNoIndentWith  fillSep
prettyErrorNoIndentL = prettyErrorNoIndentWith fillSep

prettyDebugS, prettyInfoS, prettyNoteS, prettyWarnS, prettyErrorS, prettyWarnNoIndentS, prettyErrorNoIndentS
  :: (HasCallStack, HasTerm env, MonadReader env m, MonadIO m)
  => String -> m ()
prettyDebugS         = prettyDebugWith         flow
prettyInfoS          = prettyInfoWith          flow
prettyNoteS          = prettyNoteWith          flow
prettyWarnS          = prettyWarnWith          flow
prettyErrorS         = prettyErrorWith         flow
prettyWarnNoIndentS  = prettyWarnNoIndentWith  flow
prettyErrorNoIndentS = prettyErrorNoIndentWith flow

-- End of aligned section

-- | Use after a label and before the rest of what's being labelled for
--   consistent spacing/indenting/etc.
--
--   For example this is used after "Warning:" in warning messages.
indentAfterLabel :: StyleDoc -> StyleDoc
indentAfterLabel = align

-- | Make a 'StyleDoc' from each word in a 'String'
wordDocs :: String -> [StyleDoc]
wordDocs = map fromString . words

-- | Wordwrap a 'String'
flow :: String -> StyleDoc
flow = fillSep . wordDocs

-- | A blank line.
blankLine :: StyleDoc
blankLine = line <> line

-- | @debug message action@ brackets any output of the specified @action@ with
-- an initial and final @message@ at log level 'LevelDebug'. The initial message
-- is prefixed with the label @Start:@. The final message is prefixed with
-- information about the duration of the action in milliseconds (ms) and, if
-- an exception is thrown by the action, the exception. For example:
--
-- > Start: <message>
-- > <output of action>
-- > Finished in ...ms: <message>
--
-- or:
--
-- > Start: <message>
-- > <output of action>
-- > Finished with exception in ...ms: <message>
-- > Exception thrown: <exception_message>
debugBracket :: (HasCallStack, HasTerm env, MonadReader env m,
                 MonadIO m, MonadUnliftIO m) => StyleDoc -> m a -> m a
debugBracket msg f = do
  let output = logDebug . RIO.display <=< displayWithColor
  output $ "Start: " <> msg
  start <- getMonotonicTime
  x <- f `catch` \ex -> do
    end <- getMonotonicTime
    let diff = end - start
    output $
         "Finished with exception in"
      <+> displayMilliseconds diff <> ":"
      <+> msg
      <> line
      <> "Exception thrown: "
      <> fromString (show ex)
    throwIO (ex :: SomeException)
  end <- getMonotonicTime
  let diff = end - start
  output $ "Finished in" <+> displayMilliseconds diff <> ":" <+> msg
  return x

-- | Annotate a 'StyleDoc' with a 'Style'.
style :: Style -> StyleDoc -> StyleDoc
style = styleAnn

-- | Display as milliseconds in style 'Good'.
displayMilliseconds ::
     Double
     -- ^ Amount of time in seconds.
  -> StyleDoc
displayMilliseconds t = style Good $
  fromString (show (round (t * 1000) :: Int)) <> "ms"

-- | Display a bulleted list of 'StyleDoc' with @*@ as the bullet point.
bulletedList :: [StyleDoc] -> StyleDoc
bulletedList = mkBulletedList False '*'

-- | Display a bulleted list of 'StyleDoc', spaced with blank lines or not,
-- given a character for the bullet point.
--
-- @since 0.1.6.0
mkBulletedList ::
     Bool
     -- ^ Spaced with a blank line between each item?
  -> Char
     -- ^ The character to act as the bullet point.
  -> [StyleDoc]
  -> StyleDoc
mkBulletedList isSpaced bullet =
  mconcat . intersperse spacer . map ((fromString [bullet] <+>) . align)
 where
  spacer = if isSpaced then line <> line else line

-- | A helper function to yield a narrative list from a list of items, with a
-- final fullstop. For example, helps produce the output
-- @\"apple, ball and cat.\"@ (no serial comma) or @\"apple, ball, and cat.\"@
-- (serial comma) from @[\"apple\", \"ball\", \"cat\"]@.
--
-- @since 0.1.4.0
mkNarrativeList ::
     Pretty a
  => Maybe Style
  -- ^ Style the items in the list?
  -> Bool
  -- ^ Use a serial comma?
  -> [a]
  -> [StyleDoc]
mkNarrativeList _ _ [] = []
mkNarrativeList mStyle _ [x] = [maybe id style mStyle (pretty x) <> "."]
mkNarrativeList mStyle useSerialComma [x1, x2] =
    mStyle' (pretty x1) <> (if useSerialComma then "," else mempty)
  : "and"
  : mkNarrativeList mStyle useSerialComma [x2]
 where
  mStyle' = maybe id style mStyle
mkNarrativeList mStyle useSerialComma (x:xs) =
    maybe id style mStyle (pretty x) <> ","
  : mkNarrativeList mStyle useSerialComma xs

-- | Display a bulleted list of 'StyleDoc' with a blank line between
-- each and @*@ as the bullet point.
spacedBulletedList :: [StyleDoc] -> StyleDoc
spacedBulletedList = mkBulletedList True '*'

-- | The 'Style' intended to be associated with a 'LogLevel'.
--
-- @since 0.1.1.0
logLevelToStyle :: LogLevel -> Style
logLevelToStyle level = case level of
  LevelDebug   -> Debug
  LevelInfo    -> Info
  LevelWarn    -> Warning
  LevelError   -> Error
  LevelOther _ -> OtherLevel
