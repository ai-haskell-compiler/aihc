------------------------------------------------------------------------------
-- |
-- Module      : Redact.Markdown
-- Description : Markdown redaction
-- Copyright   : Copyright (c) 2020-2024 Travis Cardwell
-- License     : MIT
--
-- Markdown inline code (text enclosed in backticks) is redacted.  This
-- redacted text may not span multiple lines.  Example:
--
-- @The word `hidden` is redacted.@
--
-- Markdown fenced code (text in between lines of three or more backticks is
-- redacted.  The number of backticks is not matched.  The backticks in the
-- beginning line may optionally be followed by addition text, which is
-- ignored.  Example:
--
-- @
-- ```
-- All lines of text between the backticks
-- are hidden.
-- ```
-- @
------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Redact.Markdown
  ( -- * Types
    Line(..)
  , Part(..)
  , Error(..)
    -- * Pure API
  , redact
  , redact'
    -- * Terminal API
  , textToTerminal
  , textToTerminal'
  , handleToTerminal
  , handleToTerminal'
  , fileToTerminal
  , fileToTerminal'
    -- * Internal
  , redactLine
  , redactLine'
  ) where

-- https://hackage.haskell.org/package/ansi-terminal
import qualified System.Console.ANSI as Term

-- https://hackage.haskell.org/package/base
import Control.Monad (guard)
import Prelude hiding (lines)
import System.IO (Handle)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- (redact)
import qualified Redact.Internal as Internal
import qualified Redact.Monad.Handle as RMH
import Redact.Monad.Handle (MonadHandle)
import qualified Redact.Monad.Terminal as RMT
import Redact.Monad.Terminal (MonadTerminal)
import Redact.Types
  ( Error(IOError, RedactError), Line(NormalLine, RedactLine)
  , Part(Redact, Stet)
  )

------------------------------------------------------------------------------
-- $PureAPI

-- | Redact text strictly
--
-- This function fails if inline or fenced code is not closed.
--
-- @since 0.4.0.0
redact :: Text -> Either String [Line]
redact = Internal.redact strictStep strictEnd initialStrictState

------------------------------------------------------------------------------

-- | Redact text leniently
--
-- This function does not fail if inline or fenced code is not closed.
--
-- @since 0.4.0.0
redact' :: Text -> [Line]
redact' = Internal.redact' lenientStep initialLenientState

------------------------------------------------------------------------------
-- $TerminalAPI

-- | Redact text strictly, putting it to the terminal
--
-- @since 0.4.0.0
textToTerminal
  :: MonadTerminal m
  => [Term.SGR]  -- ^ 'Term.SGR's for redacted text
  -> Text
  -> m (Either Error ())
textToTerminal sgrs
    = either (pure . Left . RedactError) (fmap Right . RMT.putLines sgrs)
    . redact

------------------------------------------------------------------------------

-- | Redact text leniently, putting it to the terminal
--
-- @since 0.4.0.0
textToTerminal'
  :: MonadTerminal m
  => [Term.SGR]  -- ^ 'Term.SGR's for redacted text
  -> Text
  -> m ()
textToTerminal' sgrs = RMT.putLines sgrs . redact'

------------------------------------------------------------------------------

-- | Redact text from a 'Handle' strictly, putting it to the terminal
--
-- @since 0.4.0.0
handleToTerminal
  :: (MonadHandle m, MonadTerminal m)
  => [Term.SGR]  -- ^ 'Term.SGR's for redacted text
  -> Handle
  -> m (Either Error ())
handleToTerminal =
    RMH.handleToTerminal strictStep strictEnd initialStrictState

------------------------------------------------------------------------------

-- | Redact text from a 'Handle' leniently, putting it to the terminal
--
-- @since 0.4.0.0
handleToTerminal'
  :: (MonadHandle m, MonadTerminal m)
  => [Term.SGR]  -- ^ 'Term.SGR's for redacted text
  -> Handle
  -> m ()
handleToTerminal' = RMH.handleToTerminal' lenientStep initialLenientState

------------------------------------------------------------------------------

-- | Redact text from a file strictly, putting it to the terminal
--
-- @since 0.4.0.0
fileToTerminal
  :: (MonadHandle m, MonadTerminal m)
  => [Term.SGR]  -- ^ 'Term.SGR's for redacted text
  -> FilePath
  -> m (Either Error ())
fileToTerminal = RMH.fileToTerminal strictStep strictEnd initialStrictState

------------------------------------------------------------------------------

-- | Redact text from a file leniently, putting it to the terminal
--
-- @since 0.4.0.0
fileToTerminal'
  :: (MonadHandle m, MonadTerminal m)
  => [Term.SGR]  -- ^ 'Term.SGR's for redacted text
  -> FilePath
  -> m (Either Error ())
fileToTerminal' = RMH.fileToTerminal' lenientStep initialLenientState

------------------------------------------------------------------------------
-- $Internal

-- | Redact a 'NormalLine' strictly
redactLine :: Text -> Maybe Line
redactLine line = do
    let ts = T.splitOn "`" line
    guard $ length ts `mod` 2 == 1
    pure $ redactLineParts ts

------------------------------------------------------------------------------

-- | Redact a 'NormalLine' leniently
redactLine' :: Text -> Line
redactLine' = redactLineParts . T.splitOn "`"

------------------------------------------------------------------------------

-- | Construct a 'NormalLine' from parts of a line split on @`@
redactLineParts :: [Text] -> Line
redactLineParts = NormalLine . go
  where
    go :: [Text] -> [Part]
    go (tStet:ts)
      | T.null tStet = goR [] ts
      | otherwise = goR [tStet] ts
    go [] = []

    goR :: [Text] -> [Text] -> [Part]
    goR acc (tRedact:ts)
      | T.null tRedact = goS ("`" : acc) ts
      | otherwise
          = Stet (T.concat . reverse $ "`" : acc)
          : Redact tRedact
          : goS [] ts
    goR acc []
      | null acc = []
      | otherwise = [Stet . T.concat $ reverse acc]

    goS :: [Text] -> [Text] -> [Part]
    goS acc (tStet:ts)
      | T.null tStet = goR ("`" : acc) ts
      | otherwise = goR (tStet : "`" : acc) ts
    goS acc []
      | null acc = []
      | otherwise = [Stet . T.concat $ reverse acc]

------------------------------------------------------------------------------

-- | Does a line begin a fenced code block?
--
-- A line that begins a fenced code block beings with three or more backticks.
isFenceBegin :: Text -> Bool
isFenceBegin = (>= 3) . T.length . T.takeWhile (== '`')

------------------------------------------------------------------------------

-- | Does a line end a fence code block?
--
-- A line that ends a fenced code block consists of three or more backticks.
isFenceEnd :: Text -> Bool
isFenceEnd = (&&) <$> (T.null . T.dropWhile (== '`')) <*> ((>= 3) . T.length)

------------------------------------------------------------------------------

-- | State for strict parsing
type StrictState = (Int, Bool)  -- (lineNum, isFenced)

------------------------------------------------------------------------------

-- | Initial 'StrictState'
initialStrictState :: StrictState
initialStrictState = (1, False)

------------------------------------------------------------------------------

-- | Parse the next line strictly
strictStep
  :: StrictState
  -> Text
  -> Either String (Line, StrictState)
strictStep (!lineNum, isFenced) t
    | isFenced = if isFenceEnd t
        then Right (NormalLine [Stet t], (lineNum + 1, False))
        else Right (RedactLine t, (lineNum + 1, True))
    | otherwise = if isFenceBegin t
        then Right (NormalLine [Stet t], (lineNum + 1, True))
        else case redactLine t of
          Just line -> Right (line, (lineNum + 1, False))
          Nothing -> Left inlineError
  where
    inlineError :: String
    inlineError = "inline code not terminated (line " ++ show lineNum ++ ")"

------------------------------------------------------------------------------

-- | Process the end of the input strictly
strictEnd :: StrictState -> Maybe String
strictEnd (!lineNum, isFenced)
    | isFenced = Just fencedError
    | otherwise = Nothing
  where
    fencedError :: String
    fencedError = "fenced code not terminated (line " ++ show lineNum ++ ")"

------------------------------------------------------------------------------

-- | State for lenient parsing
type LenientState = Bool  -- isFenced

------------------------------------------------------------------------------

-- Initial 'LenientState'
initialLenientState :: LenientState
initialLenientState = False

------------------------------------------------------------------------------

-- | Parse the next line leniently
lenientStep
  :: LenientState
  -> Text
  -> (Line, LenientState)
lenientStep isFenced t
    | isFenced = if isFenceEnd t
        then (NormalLine [Stet t], False)
        else (RedactLine t, True)
    | otherwise = if isFenceBegin t
        then (NormalLine [Stet t], True)
        else (redactLine' t, False)
