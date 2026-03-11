------------------------------------------------------------------------------
-- |
-- Module      : Redact.Monad.Handle
-- Description : handle I/O
-- Copyright   : Copyright (c) 2020-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Redact.Monad.Handle
  ( -- * MonadHandle
    MonadHandle(..)
    -- * Internal
  , handleToTerminal
  , handleToTerminal'
  , fileToTerminal
  , fileToTerminal'
  ) where

-- https://hackage.haskell.org/package/ansi-terminal
import qualified System.Console.ANSI as Term

-- https://hackage.haskell.org/package/base
import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Typeable (Typeable)
import qualified System.IO as IO
import System.IO (Handle, IOMode(ReadMode))
import System.IO.Error (tryIOError)

-- https://hackage.haskell.org/package/text
import Data.Text (Text)
import qualified Data.Text.IO as TIO

-- (redact)
import Redact.Monad.Terminal (MonadTerminal, initialize, putLine)
import Redact.Types (Error(IOError, RedactError), Line)

------------------------------------------------------------------------------
-- $MonadHandle

-- | Handle I/O
--
-- @since 0.4.0.0
class Monad m => MonadHandle m where
  -- | Read a single line from a handle
  hGetLine :: Handle -> m Text

  -- | Check if a handle has more content
  hIsEOF :: Handle -> m Bool

  -- | Open a file and perform an action on its handle
  withFile
    :: Typeable r
    => FilePath
    -> IOMode
    -> (Handle -> m r)
    -> m (Either IOError r)

instance MonadHandle IO where
  hGetLine = TIO.hGetLine
  {-# INLINE hGetLine #-}

  hIsEOF = IO.hIsEOF
  {-# INLINE hIsEOF #-}

  withFile path mode = tryIOError . IO.withFile path mode
  {-# INLINE withFile #-}

------------------------------------------------------------------------------
-- $Internal

-- | Redact text from a 'Handle' strictly, putting it to the terminal
handleToTerminal
  :: forall m s. (MonadHandle m, MonadTerminal m)
  => (s -> Text -> Either String (Line, s))  -- ^ step function
  -> (s -> Maybe String)                     -- ^ end function
  -> s                                       -- ^ initial state
  -> [Term.SGR]  -- ^ 'Term.SGR's for redacted text
  -> Handle
  -> m (Either Error ())
handleToTerminal step end initialState sgrs handle =
    begin =<< maybeGetLine initialState
  where
    maybeGetLine :: s -> m (Either String (Maybe Line, s))
    maybeGetLine state
      = bool
          (fmap (first Just) . step state <$> hGetLine handle)
          (pure $ Right (Nothing, state))
      =<< hIsEOF handle

    begin :: Either String (Maybe Line, s) -> m (Either Error ())
    begin = \case
      Right (Just line, state) -> do
        initialize sgrs line
        loop line =<< maybeGetLine state
      Right (Nothing, _state) -> pure $ Right ()
      Left err -> pure . Left $ RedactError err

    loop :: Line -> Either String (Maybe Line, s) -> m (Either Error ())
    loop line = \case
      Right (Just nextLine, state) -> do
        putLine sgrs line $ Just nextLine
        loop nextLine =<< maybeGetLine state
      Right (Nothing, state) -> do
        putLine sgrs line Nothing
        case end state of
          Nothing -> pure $ Right ()
          Just err -> pure . Left $ RedactError err
      Left err -> do
        putLine sgrs line Nothing
        pure . Left $ RedactError err

------------------------------------------------------------------------------

-- | Redact text from a 'Handle' leniently, putting it to the terminal
handleToTerminal'
  :: forall m s. (MonadHandle m, MonadTerminal m)
  => (s -> Text -> (Line, s))  -- ^ step function
  -> s                         -- ^ initial state
  -> [Term.SGR]                -- ^ 'Term.SGR's for redacted text
  -> Handle
  -> m ()
handleToTerminal' step initialState sgrs handle =
    begin =<< maybeGetLine initialState
  where
    maybeGetLine :: s -> m (Maybe Line, s)
    maybeGetLine state
      = bool
          (first Just . step state <$> hGetLine handle)
          (pure (Nothing, state))
      =<< hIsEOF handle

    begin :: (Maybe Line, s) -> m ()
    begin = \case
      (Just line, state) -> do
        initialize sgrs line
        loop line =<< maybeGetLine state
      (Nothing, _state) -> pure ()

    loop :: Line -> (Maybe Line, s) -> m ()
    loop line (mNextLine, state) = do
      putLine sgrs line mNextLine
      case mNextLine of
        Just nextLine -> loop nextLine =<< maybeGetLine state
        Nothing -> pure ()

------------------------------------------------------------------------------

-- | Redact text from a file strictly, putting it to the terminal
fileToTerminal
  :: (MonadHandle m, MonadTerminal m)
  => (s -> Text -> Either String (Line, s))  -- ^ step function
  -> (s -> Maybe String)                     -- ^ end function
  -> s                                       -- ^ initial state
  -> [Term.SGR]  -- ^ 'Term.SGR's for redacted text
  -> FilePath
  -> m (Either Error ())
fileToTerminal step end initialState sgrs path
    = fmap (join . first IOError)
    . withFile path ReadMode
    $ handleToTerminal step end initialState sgrs

------------------------------------------------------------------------------

-- | Redact text from a file leniently, putting it to the terminal
fileToTerminal'
  :: (MonadHandle m, MonadTerminal m)
  => (s -> Text -> (Line, s))  -- ^ step function
  -> s                         -- ^ initial state
  -> [Term.SGR]                -- ^ 'Term.SGR's for redacted text
  -> FilePath
  -> m (Either Error ())
fileToTerminal' step initialState sgrs path
    = fmap (first IOError)
    . withFile path ReadMode
    $ handleToTerminal' step initialState sgrs
