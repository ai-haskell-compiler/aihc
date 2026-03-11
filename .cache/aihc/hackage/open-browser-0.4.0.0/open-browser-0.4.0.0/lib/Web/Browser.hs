{-# LANGUAGE LambdaCase #-}

{-|
Module      : Web.Browser
Description : Open a web browser from Haskell
Copyright   : (c) rightfold 2015
License     : BSD3
Maintainer  : public@pilgrem.com

Open a web browser from Haskell. Supported operating systems are Windows, macOS,
Linux and BSD.
-}

module Web.Browser
  ( openBrowser
    -- * Utilities
  , openBrowserWithExitCode
  ) where

import           Control.Exception ( Exception (..), SomeException, try)
import           System.Exit ( ExitCode (..) )
import qualified Web.Browser.OS as OS

-- | Seeks to open the given item, silently. If the item is a URL or another
-- item associated with a web browser (for example, it represents a local
-- @.html@ file), seeks to open it in the user's preferred web browser. Returns
-- whether or not the operation succeeded.
--
-- No checks are performed on the nature or validity of the given item.
--
-- Implemented using:
--
-- * on Windows, the \'open\' operation provided by the Win32 API. For an item
--   that represents a file, equivalent double-clicking on the file's icon;
--
-- * on macOS, the \'open\' application, if it is on the user's PATH. For an
--   item that represents a file, equivalent to double-clicking on the file's
--   icon; and
--
-- * on Linux, FreeBSD, OpenBSD or NetBSD, the \'xdg-open\' script, if it
--   is on the user's PATH.
--
-- On other operating systems, the operation always fails.
--
-- @since 0.1.0.0
openBrowser ::
     String
     -- ^ URL or other item to try to open.
  -> IO Bool
openBrowser url = tryOpenUrl >>= \case
  Left _ -> pure False
  Right (ec, _, _) -> pure $ ec == ExitSuccess
 where
  tryOpenUrl :: IO (Either SomeException (ExitCode, String, String))
  tryOpenUrl = openBrowserWithExitCode url

-- | Exported to help with debugging. As for 'openBrowser' but returns either an
-- exception or, as a triple, the 'ExitCode' of the opening mechanism and any
-- output to the standard output and standard error channels. On failure, the
-- meaning of the exit code will depend on the operating system; for unsupported
-- operating systems, it will be 'ExitFailure' @1@.
--
-- @since 0.4.0.0
openBrowserWithExitCode ::
     Exception e
  => String
     -- ^ URL or other item to try to open.
  -> IO (Either e (ExitCode, String, String))
openBrowserWithExitCode url = try $ OS.openBrowserWithExitCode url
