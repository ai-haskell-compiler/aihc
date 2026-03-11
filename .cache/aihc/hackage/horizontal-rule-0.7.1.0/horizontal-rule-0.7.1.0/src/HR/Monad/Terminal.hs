------------------------------------------------------------------------------
-- |
-- Module      : HR.Monad.Terminal
-- Description : terminal output
-- Copyright   : Copyright (c) 2019-2026 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module HR.Monad.Terminal
  ( -- * MonadTerminal
    MonadTerminal(..)
  ) where

-- https://hackage.haskell.org/package/terminal-size
import qualified System.Console.Terminal.Size as TS

-- https://hackage.haskell.org/package/text
import Data.Text (Text)
import qualified Data.Text.IO as TIO

------------------------------------------------------------------------------
-- $MonadTerminal

-- | Terminal output
--
-- @since 0.5.0.0
class Monad m => MonadTerminal m where
  -- | Get the width of the terminal, if possible
  getWidth :: m (Maybe Int)

  -- | Write a string to @STDOUT@, appending a newline
  putStrLn :: Text -> m ()

instance MonadTerminal IO where
  getWidth = fmap TS.width <$> TS.size
  {-# INLINE getWidth #-}

  putStrLn = TIO.putStrLn
  {-# INLINE putStrLn #-}
