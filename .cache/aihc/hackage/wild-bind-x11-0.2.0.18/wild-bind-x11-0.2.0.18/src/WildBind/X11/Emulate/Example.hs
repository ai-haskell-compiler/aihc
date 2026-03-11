{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: WildBind.X11.Emulate.Example
-- Description: Example of WildBind.X11.Emulate
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an example of using "WildBind.X11.Emulate". See the source.
--
-- @since 0.2.0.0
module WildBind.X11.Emulate.Example where

import           WildBind             (Binding, as, bindsF, on, run, wildBind)
import           WildBind.X11         (ActiveWindow, X11Front, XKeyEvent, alt, ctrl, makeFrontEnd,
                                       press, release, withX11Front)
import           WildBind.X11.Emulate (sendKey)
import           WildBind.X11.KeySym  (xK_c, xK_w)

main :: IO ()
main = withX11Front $ \x11 -> wildBind (myBinding x11) (makeFrontEnd x11)

-- | To use emulation functions, you need to obtain an 'X11Front'
-- object by 'withX11Front', and call emulation functions on it.
--
-- 'bindsF' function is useful to send keyboard events to the current
-- 'ActiveWindow'.
myBinding :: X11Front XKeyEvent -> Binding ActiveWindow XKeyEvent
myBinding x11 = bindsF $ do
  on (alt xK_w) `as` "Copy" `run` sendKey x11 (ctrl xK_c)
