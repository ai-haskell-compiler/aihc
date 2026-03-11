{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module: WildBind.X11.Emulate
-- Description: X11 event emulation functions
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module defines functions to emulate key inputs.
--
-- See "WildBind.X11.Emulate.Example" for an example.
--
-- @since 0.2.0.0
module WildBind.X11.Emulate
    ( -- * Create key inputs
      sendKeyTo
    , sendKey
    , pushTo
    , push
      -- * Key remap binding
    , remap
    , remapR
    ) where

import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Reader.Class     (MonadReader (ask))
import           WildBind.Binding               (Binding', bindsF, on, run)

import           WildBind.X11.Internal.FrontEnd (X11Front (..))
import           WildBind.X11.Internal.Key      (KeyEventType (..), ToXKeyEvent (..), XKeyEvent,
                                                 press, release, xSendKeyEvent)
import           WildBind.X11.Internal.Window   (ActiveWindow, Window, winID)

-- | Send a X11 key event to a 'Window'.
sendKeyTo :: (ToXKeyEvent k, MonadIO m)
          => X11Front i
          -> Window -- ^ target window
          -> k -- ^ Key event to send
          -> m ()
sendKeyTo front win key = liftIO $ xSendKeyEvent kmmap disp win_id key_event
  where
    kmmap = x11KeyMaskMap front
    disp = x11Display front
    win_id = winID win
    key_event = toXKeyEvent key

-- | Same as 'sendKeyTo', but the target window is obtained from
-- 'MonadReader'.
sendKey :: (ToXKeyEvent k, MonadIO m, MonadReader Window m) => X11Front i -> k -> m ()
sendKey front key = do
  win <- ask
  sendKeyTo front win key

-- | Send a \"key push\" event to a 'Window', that is, send 'KeyPress'
-- and 'KeyRelease' events.
pushTo :: (ToXKeyEvent k, MonadIO m) => X11Front i -> Window -> k -> m ()
pushTo front win key = do
  send $ press key
  send $ release key
  where
    send = sendKeyTo front win

-- | Same as 'pushTo', but the target window is obtained from
-- 'MonadReader'.
push :: (ToXKeyEvent k, MonadIO m, MonadReader Window m) => X11Front i -> k -> m ()
push front key = do
  win <- ask
  pushTo front win key

-- | Create a binding that remaps key event \"@from@\" to
-- \"@to@\".
--
-- This binding captures 'KeyPress' and 'KeyRelease' events of
-- \"@from@\", and sends respective events of \"@to@\" to the active
-- window.
--
-- Sometimes 'remap' doesn't work as you expect, because the
-- 'KeyPress' event is sent to the window while it doesn't have
-- keyboard focus. In that case, try using 'remapR'.
remap :: (ToXKeyEvent from, ToXKeyEvent to) => X11Front i -> from -> to -> Binding' bs ActiveWindow XKeyEvent
remap front from to = bindsF $ do
  on (press from) `run` sendKey' (press to)
  on (release from) `run` sendKey' (release to)
  where
    sendKey' = sendKey front

-- | remap on Release. Like 'remap', but this binding captures only
-- 'KeyRelease' event and sends a pair of 'KeyPress' and 'KeyRelease'
-- events.
--
-- Because the original 'KeyRelease' event occurs after the focus
-- returns to the window, the emulated events are sent to the window
-- with focus.
remapR :: (ToXKeyEvent from, ToXKeyEvent to) => X11Front i -> from -> to -> Binding' bs ActiveWindow XKeyEvent
remapR front from to = bindsF $ do
  on (release from) `run` push front to
