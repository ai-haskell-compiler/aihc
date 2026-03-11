------------------------------------------------------------------------------
-- |
-- Module      : Redact.Monad.Terminal
-- Description : terminal output
-- Copyright   : Copyright (c) 2020-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Redact.Monad.Terminal
  ( -- * MonadTerminal
    MonadTerminal(..)
    -- * API
  , redactSGRs
  , resetSGRs
  , reset
  , putLines
    -- * Internal
  , initialize
  , putLine
  ) where

-- https://hackage.haskell.org/package/ansi-terminal
import qualified System.Console.ANSI as Term

-- https://hackage.haskell.org/package/base
import Control.Monad (unless, when)
import Data.Maybe (listToMaybe)
import Prelude hiding (lines, putStr, putStrLn)

-- https://hackage.haskell.org/package/text
import Data.Text (Text)
import qualified Data.Text.IO as TIO

-- (redact)
import Redact.Types (Line(NormalLine, RedactLine), Part(Redact, Stet))

------------------------------------------------------------------------------
-- $MonadTerminal

-- | Terminal output
--
-- @since 0.4.0.0
class Monad m => MonadTerminal m where
  -- | Write a string to the terminal
  putStr :: Text -> m ()

  -- | Write a string to the terminal, appending a newline
  putStrLn :: Text -> m ()

  -- | Set Select Graphic Rendition mode
  setSGR :: [Term.SGR] -> m ()

instance MonadTerminal IO where
  putStr = TIO.putStr
  {-# INLINE putStr #-}

  putStrLn =  TIO.putStrLn
  {-# INLINE putStrLn #-}

  setSGR = Term.setSGR
  {-# INLINE setSGR #-}

------------------------------------------------------------------------------
-- $API

-- | Construct 'Term.SGR's for redacted text
--
-- @since 0.4.0.0
redactSGRs :: Term.Color -> Term.ColorIntensity -> [Term.SGR]
redactSGRs color intensity =
    [ Term.SetColor Term.Foreground intensity color
    , Term.SetColor Term.Background intensity color
    ]

------------------------------------------------------------------------------

-- | 'Term.SGR's for resetting to normal mode
--
-- @since 0.4.0.0
resetSGRs :: [Term.SGR]
resetSGRs = [Term.Reset]

------------------------------------------------------------------------------

-- | Reset the terminal color mode and go to the next line
--
-- @since 0.4.0.0
reset :: MonadTerminal m => m ()
reset = setSGR resetSGRs >> putStrLn ""

------------------------------------------------------------------------------

-- | Put redacted text to the terminal
--
-- It is assumed that the terminal is set to display normal colors when this
-- function is called.  If the first 'Line' is a 'RedactLine', then an extra
-- blank line is first output in order to set the colors.  The terminal is set
-- to display normal colors when this function exits.
--
-- @since 0.4.0.0
putLines
  :: forall m. MonadTerminal m
  => [Term.SGR]
  -> [Line]
  -> m ()
putLines sgrs lines = do
    maybe (pure ()) (initialize sgrs) . listToMaybe $ take 1 lines
    mapM_ (uncurry $ putLine sgrs) $
      zip lines ((Just <$> drop 1 lines) ++ [Nothing])

------------------------------------------------------------------------------
-- $Internal

-- | Initialize the terminal colors
--
-- When the first line is a 'RedactLine', an extra blank line is output in
-- order to set the terminal colors.
initialize
  :: MonadTerminal m
  => [Term.SGR]  -- ^ 'Term.SGR's for redacted text
  -> Line        -- ^ first line
  -> m ()
initialize sgrs = \case
    NormalLine{} -> pure ()
    RedactLine{} -> setSGR sgrs >> putStrLn ""

------------------------------------------------------------------------------

-- | Put a line of redacted text to the terminal
--
-- The colors for the line is assumed to be already set.  Be sure to call
-- 'initialize' before putting the first line.
putLine
  :: forall m. MonadTerminal m
  => [Term.SGR]  -- ^ 'Term.SGR's for redacted text
  -> Line        -- ^ line to put
  -> Maybe Line  -- ^ 'Just' next line or 'Nothing' if end
  -> m ()
putLine sgrs line mNextLine = case line of
    NormalLine parts -> go False parts
    RedactLine t
      | isNextRedact -> putStrLn t
      | otherwise    -> putStr t >> setSGR resetSGRs >> putStrLn ""
  where
    isNextRedact :: Bool
    isNextRedact = case mNextLine of
      Just RedactLine{} -> True
      _normalLineOrEnd  -> False

    go :: Bool -> [Part] -> m ()
    go isRedact [part] = case part of
      Stet t -> do
        when isRedact $ setSGR resetSGRs
        if isNextRedact
          then putStr t >> setSGR sgrs >> putStrLn ""
          else putStrLn t
      Redact t -> do
        unless isRedact $ setSGR sgrs
        if isNextRedact
          then putStrLn t
          else putStr t >> setSGR resetSGRs >> putStrLn ""
    go isRedact (part:parts) = case (isRedact, part) of
      (False, Stet t)   ->                     putStr t >> go False parts
      (True,  Stet t)   -> setSGR resetSGRs >> putStr t >> go False parts
      (False, Redact t) -> setSGR sgrs      >> putStr t >> go True  parts
      (True,  Redact t) ->                     putStr t >> go True  parts
    go _isRedact [] = putStrLn ""
