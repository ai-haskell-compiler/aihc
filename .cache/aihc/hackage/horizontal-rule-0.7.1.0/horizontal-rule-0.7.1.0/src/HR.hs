------------------------------------------------------------------------------
-- |
-- Module      : HR
-- Description : horizontal rule for terminals
-- Copyright   : Copyright (c) 2019-2026 Travis Cardwell
-- License     : MIT
--
-- This library is meant to be imported qualified, as follows:
--
-- @
-- import qualified HR
-- @
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HR
  ( -- * Constants
    version
    -- * Parts
  , Parts(..)
  , asciiParts
  , unicodeParts
    -- * API
    -- ** Pure
  , render
  , renderAscii
  , renderUnicode
    -- ** IO
  , put
  , putAscii
  , putUnicode
  , putAuto
  , putAutoAscii
  , putAutoUnicode
  ) where

-- https://hackage.haskell.org/package/base
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- (horizontal-rule)
import qualified HR.Monad.Terminal as Terminal
import HR.Monad.Terminal (MonadTerminal)

-- (horizontal-rule:cabal)
import qualified Paths_horizontal_rule as Project

------------------------------------------------------------------------------
-- $Constants

-- | hr version string (\"@hr-haskell X.X.X.X@\")
--
-- @since 0.3.0.0
version :: String
version = "hr-haskell " ++ showVersion Project.version

------------------------------------------------------------------------------
-- $Parts

-- | Text parts of a horizontal rule
--
-- @since 0.3.0.0
data Parts
  = Parts
    { leftPart  :: !Text  -- ^ text before the first note
    , midPart   :: !Text  -- ^ text between notes
    , rightPart :: !Text  -- ^ text after the last note
    , fillPart  :: !Char  -- ^ fill character
    }

-- | ASCII text parts of a horizontal rule
--
-- @since 0.3.0.0
asciiParts :: Parts
asciiParts = Parts
    { leftPart  = "--|"
    , midPart   = "|-|"
    , rightPart = "|--"
    , fillPart  = '-'
    }

-- | Unicode text parts of a horizontal rule
--
-- @since 0.3.0.0
unicodeParts :: Parts
unicodeParts = Parts
    { leftPart  = "━━┫"
    , midPart   = "┣━┫"
    , rightPart = "┣━━"
    , fillPart  = '━'
    }

------------------------------------------------------------------------------
-- $API

-- | Render a horizontal rule
--
-- Note that the rendered horizontal rule maybe longer than the specified rule
-- width if the provided notes is too wide.
--
-- @since 0.3.0.0
render
  :: Parts
  -> Int     -- ^ rule width (characters)
  -> [Text]  -- ^ notes
  -> Text
render Parts{..} width = \case
    [] -> T.pack $ replicate width fillPart
    notes ->
      let rule = T.concat
            [ leftPart
            , T.intercalate midPart notes
            , rightPart
            ]
      in  case max 0 (width - T.length rule) of
            0         -> rule
            fillWidth -> rule `T.append` T.pack (replicate fillWidth fillPart)

-- | Render an ASCII horizontal rule
--
-- Note that the rendered horizontal rule maybe longer than the specified rule
-- width if the provided notes is too wide.
--
-- @since 0.3.0.0
renderAscii
  :: Int     -- ^ rule width (characters)
  -> [Text]  -- ^ notes
  -> Text
renderAscii = render asciiParts

-- | Render a Unicode horizontal rule
--
-- Note that the rendered horizontal rule maybe longer than the specified rule
-- width if the provided notes is too wide.
--
-- @since 0.3.0.0
renderUnicode
  :: Int     -- ^ rule width (characters)
  -> [Text]  -- ^ notes
  -> Text
renderUnicode = render unicodeParts

------------------------------------------------------------------------------

-- | Write a horizontal rule to the standard output device
--
-- @since 0.5.0.0
put
  :: MonadTerminal m
  => Parts
  -> Int     -- ^ rule width (characters)
  -> [Text]  -- ^ notes
  -> m ()
put parts width = Terminal.putStrLn . render parts width

-- | Write an ASCII horizontal rule to the standard output device
--
-- @since 0.5.0.0
putAscii
  :: MonadTerminal m
  => Int     -- ^ rule width (characters)
  -> [Text]  -- ^ notes
  -> m ()
putAscii = put asciiParts

-- | Write a Unicode horizontal rule to the standard output device
--
-- @since 0.5.0.0
putUnicode
  :: MonadTerminal m
  => Int     -- ^ rule width (characters)
  -> [Text]  -- ^ notes
  -> m ()
putUnicode = put unicodeParts

------------------------------------------------------------------------------

-- | Write a full-width horizontal rule to the standard output device
--
-- The default rule width is used if the terminal width cannot be determined.
--
-- @since 0.5.0.0
putAuto
  :: MonadTerminal m
  => Parts
  -> Int     -- ^ default rule width (characters)
  -> [Text]  -- ^ notes
  -> m ()
putAuto parts defaultWidth notes = do
    width <- fromMaybe defaultWidth <$> Terminal.getWidth
    Terminal.putStrLn $ render parts width notes

-- | Write a full-width ASCII horizontal rule to the standard output device
--
-- The default rule width is used if the terminal width cannot be determined.
--
-- @since 0.5.0.0
putAutoAscii
  :: MonadTerminal m
  => Int     -- ^ default rule width (characters)
  -> [Text]  -- ^ notes
  -> m ()
putAutoAscii = putAuto asciiParts

-- | Write a full-width Unicode horizontal rule to the standard output device
--
-- The default rule width is used if the terminal width cannot be determined.
--
-- @since 0.5.0.0
putAutoUnicode
  :: MonadTerminal m
  => Int     -- ^ default rule width (characters)
  -> [Text]  -- ^ notes
  -> m ()
putAutoUnicode = putAuto unicodeParts
