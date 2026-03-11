-- |
-- Module: WildBind.X11
-- Description: X11-specific implementation for WildBind
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module exports a 'FrontEnd' for X11 environments.
module WildBind.X11
    ( -- * X11 front-end
      withFrontEnd
    , XKeyInput (..)
      -- * Windows in X11
    , Window
    , ActiveWindow
      -- ** Getters
    , winInstance
    , winClass
    , winName
      -- * Keys in X11
    , XKeyEvent (..)
    , XMod (..)
    , KeyEventType (..)
    , ToXKeyEvent (..)
      -- ** Setters
    , press
    , release
    , shift
    , ctrl
    , alt
    , super
    , addXMod
      -- * X11Front
    , X11Front
    , withX11Front
    , makeFrontEnd
    , defaultRootWindow
    ) where

import           WildBind                       (FrontEnd)
import qualified WildBind.Description           as WBD

import           WildBind.X11.Internal.FrontEnd (X11Front, defaultRootWindow, makeFrontEnd,
                                                 withFrontEnd, withX11Front)
import           WildBind.X11.Internal.Key      (KeyEventType (..), ToXKeyEvent (..),
                                                 XKeyEvent (..), XKeyInput (..), XMod (..), addXMod,
                                                 alt, ctrl, press, release, shift, super)
import           WildBind.X11.Internal.Window   (ActiveWindow, Window, winClass, winInstance,
                                                 winName)

