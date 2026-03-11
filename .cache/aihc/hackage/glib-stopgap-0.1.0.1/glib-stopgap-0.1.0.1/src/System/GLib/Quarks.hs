{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.GLib.Quarks (
	GQuark,
	gQuarkFromString, gQuarkToString, gInternString, gUninternString ) where

import System.GLib.Quarks.Internal
