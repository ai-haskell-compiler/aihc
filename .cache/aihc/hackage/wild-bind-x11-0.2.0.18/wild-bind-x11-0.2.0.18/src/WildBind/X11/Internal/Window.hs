{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: WildBind.X11.Internal.Window
-- Description: types and functions related to X11 windows
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. Package users should not rely on this.__
module WildBind.X11.Internal.Window
    ( -- * The 'Window' data type
      Window
    , ActiveWindow
    , emptyWindow
    , fromWinID
      -- * Accessor functions for 'Window'
    , winInstance
    , winClass
    , winName
      -- ** project-internal accessor
    , winID
      -- * Functions
    , getActiveWindow
    , defaultRootWindowForDisplay
    ) where

import           Control.Applicative       (empty, (<$>), (<|>))
import           Control.Monad             (guard)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.Maybe                (listToMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Foreign
import qualified Graphics.X11.Xlib         as Xlib
import qualified Graphics.X11.Xlib.Extras  as XlibE

-- | Information about window. You can inspect properties 'winInstance'
-- and 'winClass' by @wmctrl@ command.
--
-- > $ wmctrl -lx
-- > 0x01400004 -1 xfce4-panel.Xfce4-panel  mydesktop xfce4-panel
-- > 0x01800003 -1 xfdesktop.Xfdesktop   mydesktop desktop
-- > 0x03800004  0 xfce4-terminal.Xfce4-terminal  mydesktop Terminal - toshio@mydesktop - byobu
-- > 0x03a000a7  0 emacs.Emacs23         mydesktop emacs@mydesktop
-- > 0x03e010fc  0 Navigator.Firefox     mydesktop debug-ito (Toshio Ito) - Mozilla Firefox
-- > 0x02600003  0 totem.Totem           mydesktop Movie Player
--
-- In the above example, the third column shows @winInstance.winClass@.
data Window
  = Window
      { winInstance :: Text
        -- ^ name of the application instance (part of @WM_CLASS@ property)
      , winClass    :: Text
        -- ^ name of the application class (part of @WM_CLASS@ property)
      , winName     :: Text
        -- ^ what's shown in the title bar
      , winID       :: Xlib.Window
        -- ^ X11 window ID.
        --
        -- @since 0.2.0.0
      }
  deriving (Eq, Ord, Show)

-- | Use this type especially when the 'Window' is active.
type ActiveWindow = Window

-- | An empty Window instance used for fallback and/or default value.
emptyWindow :: Window
emptyWindow = Window "" "" "" 0

-- | Create 'Window' from X11's 'Xlib.Window'. Only for testing.
--
-- @since 0.2.0.0
fromWinID :: Xlib.Window -> Window
fromWinID wid = emptyWindow { winID = wid }

-- | Get currently active 'Window'.
getActiveWindow :: Xlib.Display -> IO ActiveWindow
getActiveWindow disp = maybe emptyWindow id <$> runMaybeT getActiveWindowM where
  getActiveWindowM = do
    awin <- xGetActiveWindow disp
    guard (awin /= 0) -- sometimes X11 returns 0 (NULL) as a window ID, which I think is always invalid
    name <- xGetWindowName disp awin
    class_hint <- liftIO $ xGetClassHint disp awin
    return $ (uncurry Window) class_hint name awin

-- | Get the default root window of the display.
--
-- @since 0.2.0.0
defaultRootWindowForDisplay :: Xlib.Display -> Window
defaultRootWindowForDisplay disp = Window "" "" "" $ Xlib.defaultRootWindow disp

-- | Check whether specified feature is supported by the window
-- manager(?) Port of libxdo's @_xdo_ewmh_is_supported()@ function.
ewmhIsSupported :: Xlib.Display -> String -> IO Bool
ewmhIsSupported disp feature_str = do
  req <- Xlib.internAtom disp "_NET_SUPPORTED" False
  feature <- Xlib.internAtom disp feature_str False
  result <- XlibE.getWindowProperty32 disp req (Xlib.defaultRootWindow disp)
  case result of
    Nothing    -> return False
    Just atoms -> return $ any ((feature ==) . fromIntegral) atoms

-- | Get X11 Window handle for the active window. Port of libxdo's
-- @xdo_window_get_active()@ function.
xGetActiveWindow :: Xlib.Display -> MaybeT IO Xlib.Window
xGetActiveWindow disp = do
  let req_str = "_NET_ACTIVE_WINDOW"
  supported <- liftIO $ ewmhIsSupported disp req_str
  if not supported
    then empty
    else do
    req <- liftIO $ Xlib.internAtom disp req_str False
    result <- MaybeT $ XlibE.getWindowProperty32 disp req (Xlib.defaultRootWindow disp)
    case result of
      []      -> empty
      (val:_) -> return $ fromIntegral val

xGetClassHint :: Xlib.Display -> Xlib.Window -> IO (Text, Text)
xGetClassHint disp win = do
  hint <- XlibE.getClassHint disp win
  return (Text.pack $ XlibE.resName hint, Text.pack $ XlibE.resClass hint)

xGetTextProperty :: Xlib.Display -> Xlib.Window -> String -> MaybeT IO Text
xGetTextProperty disp win prop_name = do
  req <- liftIO $ Xlib.internAtom disp prop_name False
  text_prop <- MaybeT $ Foreign.alloca $ \ptr_prop -> do
    status <- XlibE.xGetTextProperty disp win ptr_prop req
    if status == 0
      then return Nothing
      else fmap Just $ Foreign.peek ptr_prop
  Text.pack <$> MaybeT (listToMaybe <$> (XlibE.wcTextPropertyToTextList disp text_prop))

-- | Get the window name for the X11 window. The window name refers to
-- @_NET_WM_NAME@ or @WM_NAME@.
xGetWindowName :: Xlib.Display -> Xlib.Window -> MaybeT IO Text
xGetWindowName disp win = xGetTextProperty disp win "_NET_WM_NAME" <|> xGetTextProperty disp win "WM_NAME"
