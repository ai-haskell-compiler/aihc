{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides functions to detect the most appropriate color mode for the 
-- current environment.
module Graphics.Vty.Platform.Windows.Output.Color
  ( detectColorMode
  , defaultColorMode
  )
where

import Control.Exception (Exception(..))
import Data.Typeable (Typeable)
import Graphics.Vty.Attributes.Color ( ColorMode(..) )

-- | Type of errors that can be thrown when configuring VTY
newtype VtyConfigurationError =
    VtyUnsupportedTermType String
    -- ^ Terminal type not supported by vty
    deriving (Show, Eq, Typeable)

instance Exception VtyConfigurationError where
    displayException (VtyUnsupportedTermType name) = "TERM type [" ++ name ++ "] is not supported at this time"

-- | Windows console supports full color.
detectColorMode :: String -> IO ColorMode
detectColorMode termType =
  case termType of
    "xterm-256color" -> return FullColor
    "xterm"          -> return FullColor
    _                -> return FullColor

-- | The default color mode for Windows
defaultColorMode :: IO ColorMode
defaultColorMode = return FullColor