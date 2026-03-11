{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gdk.Window where

import Foreign.Ptr

data WTag

newtype W = W (Ptr WTag) deriving Show

destroy :: W -> IO ()
destroy = c_gdk_window_destroy

foreign import ccall "gdk_window_destroy" c_gdk_window_destroy :: W -> IO ()
