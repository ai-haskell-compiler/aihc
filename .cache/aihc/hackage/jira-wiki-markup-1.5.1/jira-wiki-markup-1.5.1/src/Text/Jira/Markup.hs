{-# LANGUAGE LambdaCase #-}
{-|
Module      : Text.Jira.Markup
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Jira markup types.
-}
module Text.Jira.Markup
  ( Doc (..)
  , Block (..)
  , Inline (..)
  , InlineStyle (..)
  , LinkType (..)
  , ListStyle (..)
  , URL (..)
  , ColorName (..)
  , Icon (..)
  , Row (..)
  , Cell (..)
  , Language (..)
  , Parameter (..)
  , normalizeInlines
  , iconText
  ) where

import Data.Text (Text, append)

-- | Jira document
newtype Doc = Doc { fromDoc :: [Block] }
  deriving (Eq, Ord, Show)

-- | Inline Jira markup elements.
data Inline
  = Anchor Text                         -- ^ anchor for internal links
  | AutoLink URL                        -- ^ URL which is also a link
  | Citation [Inline]                   -- ^ source of a citation
  | ColorInline ColorName [Inline]      -- ^ colored inline text
  | Emoji Icon                          -- ^ emoticon
  | Entity Text                         -- ^ named or numeric HTML entity
  | Image [Parameter] URL               -- ^ an image
  | Linebreak                           -- ^ hard linebreak
  | Link LinkType [Inline] URL          -- ^ hyperlink with alias
  | Monospaced [Inline]                 -- ^ text rendered with monospaced font
  | Space                               -- ^ space between words
  | SpecialChar Char                    -- ^ single char with special meaning
  | Str Text                            -- ^ simple, markup-less string
  | Styled InlineStyle [Inline]         -- ^ styled text
  deriving (Eq, Ord, Show)

-- | Supported inline text effect styles.
data InlineStyle
  = Emphasis                            -- ^ emphasized text
  | Insert                              -- ^ text marked as having been inserted
  | Strikeout                           -- ^ deleted (struk-out) text
  | Strong                              -- ^ strongly emphasized text
  | Subscript                           -- ^ subscript text
  | Superscript                         -- ^ superscript text
  deriving (Eq, Ord, Show)

-- | Type of a link.
data LinkType
  = Attachment                          -- ^ link to an attachment
  | Email                               -- ^ link to an email address
  | External                            -- ^ external resource, like a website
  | SmartCard                           -- ^ smart-card link (external)
  | SmartLink                           -- ^ "smart" link with icon, short-name
  | User                                -- ^ link to a user
  deriving (Eq, Ord, Show)

-- | Blocks of text.
data Block
  = Code Language [Parameter] Text      -- ^ Code block with panel parameters
  | Color ColorName [Block]             -- ^ text displayed in a specific color
  | BlockQuote [Block]                  -- ^ Block of quoted content
  | Header Int [Inline]                 -- ^ Header with level and text
  | HorizontalRule                      -- ^ horizontal ruler
  | List ListStyle [[Block]]            -- ^ List
  | NoFormat [Parameter] Text           -- ^ Unformatted text
  | Panel [Parameter] [Block]           -- ^ Formatted panel
  | Para [Inline]                       -- ^ Paragraph of text
  | Table [Row]                         -- ^ Table
  deriving (Eq, Ord, Show)

-- | Style used for list items.
data ListStyle
  = CircleBullets                       -- ^ List with round bullets
  | SquareBullets                       -- ^ List with square bullets
  | Enumeration                         -- ^ Enumeration, i.e., numbered items
  deriving (Eq, Ord, Show)

-- | Text color
newtype ColorName = ColorName Text
  deriving (Eq, Ord, Show)

-- | Unified resource location
newtype URL = URL { fromURL :: Text }
  deriving (Eq, Ord, Show)

-- | Table row, containing an arbitrary number of cells.
newtype Row = Row { fromRow :: [Cell] }
  deriving (Eq, Ord, Show)

-- | Table cell with block content
data Cell
  = BodyCell [Block]
  | HeaderCell [Block]
  deriving (Eq, Ord, Show)

-- | Programming language used for syntax highlighting.
newtype Language = Language Text
  deriving (Eq, Ord, Show)

-- | Panel parameter
data Parameter = Parameter
  { parameterKey :: Text
  , parameterValue :: Text
  } deriving (Eq, Ord, Show)

-- | Graphical emoticons
data Icon
  = IconSlightlySmiling
  | IconFrowning
  | IconTongue
  | IconSmiling
  | IconWinking
  | IconThumbsUp
  | IconThumbsDown
  | IconInfo
  | IconCheckmark
  | IconX
  | IconAttention
  | IconPlus
  | IconMinus
  | IconQuestionmark
  | IconOn
  | IconOff
  | IconStar
  | IconStarRed
  | IconStarGreen
  | IconStarBlue
  | IconStarYellow
  | IconFlag
  | IconFlagOff
  deriving (Enum, Eq, Ord, Show)

-- | Normalize a list of inlines, merging elements where possible.
normalizeInlines :: [Inline] -> [Inline]
normalizeInlines = \case
  []                     -> []
  [Space]                -> []
  [Linebreak]            -> []
  Space : Space : xs     -> Space : normalizeInlines xs
  Space : Linebreak : xs -> Linebreak : normalizeInlines xs
  Linebreak : Space : xs -> Linebreak : normalizeInlines xs
  Str s1 : Str s2 : xs   -> Str (s1 `append` s2) : normalizeInlines xs
  x : xs                 -> x : normalizeInlines xs

-- | Gets the characters used to represent an emoji.
iconText :: Icon -> Text
iconText = \case
  IconSlightlySmiling -> ":)"
  IconFrowning        -> ":("
  IconTongue          -> ":P"
  IconSmiling         -> ":D"
  IconWinking         -> ";)"
  IconThumbsUp        -> "(y)"
  IconThumbsDown      -> "(n)"
  IconInfo            -> "(i)"
  IconCheckmark       -> "(/)"
  IconX               -> "(x)"
  IconAttention       -> "(!)"
  IconPlus            -> "(+)"
  IconMinus           -> "(-)"
  IconQuestionmark    -> "(?)"
  IconOn              -> "(on)"
  IconOff             -> "(off)"
  IconStar            -> "(*)"
  IconStarRed         -> "(*r)"
  IconStarGreen       -> "(*g)"
  IconStarBlue        -> "(*b)"
  IconStarYellow      -> "(*y)"
  IconFlag            -> "(flag)"
  IconFlagOff         -> "(flagoff)"
