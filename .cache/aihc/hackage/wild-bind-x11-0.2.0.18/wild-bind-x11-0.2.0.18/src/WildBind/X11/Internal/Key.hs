{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
-- |
-- Module: WildBind.X11.Internal.Key
-- Description: types and functions related to key symbols and their conversion
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. Package users should not rely on this.__
module WildBind.X11.Internal.Key
    ( -- * Key
      XKeyInput (..)
    , xKeyEventToXKeyInput
    , KeyEventType (..)
      -- * Modifiers
    , KeyMaskMap (..)
    , getKeyMaskMap
      -- * XKeyEvent
    , XKeyEvent (..)
    , XMod (..)
    , ToXKeyEvent (..)
    , addXMod
    , press
    , release
    , shift
    , ctrl
    , alt
    , super
      -- * Grabs
    , xGrabKey
    , xUngrabKey
      -- * Event generation
    , xSendKeyEvent
    ) where

import           Control.Applicative       ((<$>), (<*>), (<|>))
import           Control.Monad             (forM_)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import           Data.Bits                 ((.&.), (.|.))
import qualified Data.Bits                 as Bits
import           Data.Foldable             (fold, foldr)
import           Data.List                 (nub)
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.Map                  as M
import           Data.Maybe                (listToMaybe, mapMaybe)
import           Data.Monoid               ((<>))
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Foreign
import qualified Graphics.X11.Xlib         as Xlib
import qualified Graphics.X11.Xlib.Extras  as XlibE

import           WildBind.Description      (Describable (..))
import qualified WildBind.Input.NumPad     as NumPad

-- | Whether the key is pressed or released.
--
-- @since 0.2.0.0
data KeyEventType = KeyPress | KeyRelease deriving (Bounded, Enum, Eq, Ord, Show)

-- | 'Xlib.KeyMask' values assigned to each modifier keys/states. If
-- the modifier doesn't exist, the mask is 0.
--
-- @since 0.2.0.0
data KeyMaskMap
  = KeyMaskMap
      { maskShift      :: Xlib.KeyMask
      , maskControl    :: Xlib.KeyMask
      , maskAlt        :: Xlib.KeyMask
      , maskSuper      :: Xlib.KeyMask
      , maskNumLock    :: Xlib.KeyMask
      , maskCapsLock   :: Xlib.KeyMask
      , maskShiftLock  :: Xlib.KeyMask
      , maskScrollLock :: Xlib.KeyMask
      }
  deriving (Eq, Ord, Show)

isMasked :: KeyMaskMap -> (KeyMaskMap -> Xlib.KeyMask) -> Xlib.KeyMask -> Bool
isMasked kmmap accessor target = if (target .&. accessor kmmap) == 0
                                 then False
                                 else True

-- | Class of data types that can be handled by X11. The data type can
-- tell X11 to grab key with optional modifiers, and it can be
-- extracted from a X11 Event object.
--
-- @since 0.2.0.0
class XKeyInput k where
  toKeySym :: k -> Xlib.KeySym
  -- ^ Get the X11 keysym for this input.
  toModifierMasks :: KeyMaskMap -> k -> NonEmpty Xlib.KeyMask
  -- ^ Get modifer masks to grab the keysym. The grab action is
  -- repeated for all modifier masks. By default, it just returns 0.
  toModifierMasks _ _ = return 0
  fromKeyEvent :: KeyMaskMap -> KeyEventType -> Xlib.KeySym -> Xlib.KeyMask -> Maybe k
  -- ^ Create the input object from a key event type, a keysym and a
  -- modifier (got from XEvent.)

-- | Partial inverse of 'toKeySym'.
fromKeySymDef :: (Bounded k, Enum k) => (k -> Xlib.KeySym) -> Xlib.KeySym -> Maybe k
fromKeySymDef to_conv ks = M.lookup ks $ M.fromList $ map (\n -> (to_conv n, n)) $ enumFromTo minBound maxBound

-- | This input event captures the 'KeyRelease' event only. That way,
-- you can deliver events to the window that originally has the
-- keyboard focus.
instance XKeyInput NumPad.NumPadUnlocked where
  toKeySym n = case n of
    NumPad.NumUp       -> Xlib.xK_KP_Up
    NumPad.NumDown     -> Xlib.xK_KP_Down
    NumPad.NumLeft     -> Xlib.xK_KP_Left
    NumPad.NumRight    -> Xlib.xK_KP_Right
    NumPad.NumHome     -> Xlib.xK_KP_Home
    NumPad.NumPageUp   -> Xlib.xK_KP_Page_Up
    NumPad.NumPageDown -> Xlib.xK_KP_Page_Down
    NumPad.NumEnd      -> Xlib.xK_KP_End
    NumPad.NumCenter   -> Xlib.xK_KP_Begin
    NumPad.NumInsert   -> Xlib.xK_KP_Insert
    NumPad.NumDelete   -> Xlib.xK_KP_Delete
    NumPad.NumEnter    -> Xlib.xK_KP_Enter
    NumPad.NumDivide   -> Xlib.xK_KP_Divide
    NumPad.NumMulti    -> Xlib.xK_KP_Multiply
    NumPad.NumMinus    -> Xlib.xK_KP_Subtract
    NumPad.NumPlus     -> Xlib.xK_KP_Add
  fromKeyEvent _ KeyPress _ _ = Nothing
  fromKeyEvent kmmask KeyRelease keysym mask = if is_numlocked
                                               then Nothing
                                               else fromKeySymDef toKeySym keysym
    where
      is_numlocked = isMasked kmmask maskNumLock mask

-- | This input event captures the 'KeyRelease' event only. That way,
-- you can deliver events to the window that originally has the
-- keyboard focus.
instance XKeyInput NumPad.NumPadLocked where
  toKeySym n = case n of
    NumPad.NumL0      -> Xlib.xK_KP_0
    NumPad.NumL1      -> Xlib.xK_KP_1
    NumPad.NumL2      -> Xlib.xK_KP_2
    NumPad.NumL3      -> Xlib.xK_KP_3
    NumPad.NumL4      -> Xlib.xK_KP_4
    NumPad.NumL5      -> Xlib.xK_KP_5
    NumPad.NumL6      -> Xlib.xK_KP_6
    NumPad.NumL7      -> Xlib.xK_KP_7
    NumPad.NumL8      -> Xlib.xK_KP_8
    NumPad.NumL9      -> Xlib.xK_KP_9
    NumPad.NumLDivide -> Xlib.xK_KP_Divide
    NumPad.NumLMulti  -> Xlib.xK_KP_Multiply
    NumPad.NumLMinus  -> Xlib.xK_KP_Subtract
    NumPad.NumLPlus   -> Xlib.xK_KP_Add
    NumPad.NumLEnter  -> Xlib.xK_KP_Enter
    NumPad.NumLPeriod -> Xlib.xK_KP_Delete
    -- XKeysymToKeycode() didn't return the correct keycode for XK_KP_Decimal in numpaar code...

  toModifierMasks kmmap _ = return $ maskNumLock kmmap

  -- Xlib handles the [(.) (Delete)] key in a weird way. In the input
  -- event, it brings XK_KP_Decimal when NumLock enabled, XK_KP_Delete
  -- when NumLock disabled. However, XKeysymToKeycode() function won't
  -- return the correct keycode for XK_KP_Decimal. (I'm not sure how
  -- much this behavior depends on user's environment...) As a
  -- workaround in this instance, we map NumLPeriod -> XK_KP_Delete,
  -- but in the reverse map, we also respond to XK_KP_Decimal.
  fromKeyEvent _ KeyPress _ _ = Nothing
  fromKeyEvent kmmap KeyRelease keysym mask =
    if not $ is_num_locked
    then Nothing
    else if keysym == Xlib.xK_KP_Decimal
         then Just NumPad.NumLPeriod
         else fromKeySymDef toKeySym keysym
    where
      is_num_locked = isMasked kmmap maskNumLock mask

-- | 'fromKeyEvent' first tries to create 'Left' (type @a@). If it
-- fails, then it tries to create 'Right' (type @b@).
instance (XKeyInput a, XKeyInput b) => XKeyInput (Either a b) where
  toKeySym = either toKeySym toKeySym
  toModifierMasks kmmap = either (toModifierMasks kmmap) (toModifierMasks kmmap)
  fromKeyEvent kmmap ev_type keysym mask =
    (fmap Left $ fromKeyEvent kmmap ev_type keysym mask) <|> (fmap Right $ fromKeyEvent kmmap ev_type keysym mask)

-- | Extract the 'XKeyInput' from the XKeyEvent.
--
-- @since 0.2.0.0
xKeyEventToXKeyInput :: XKeyInput k => KeyMaskMap -> KeyEventType -> Xlib.XKeyEventPtr -> MaybeT IO k
xKeyEventToXKeyInput kmmap ev_type kev = do
  keysym <- MaybeT (fst <$> Xlib.lookupString kev)
  (_, _, _, _, _, _, _, status, _, _) <- liftIO $ Xlib.get_KeyEvent $ Foreign.castPtr kev
  MaybeT $ return $ fromKeyEvent kmmap ev_type keysym status

type XModifierMap = [(Xlib.Modifier, [Xlib.KeyCode])]

-- | Get current 'KeyMaskMap'.
--
-- @since 0.2.0.0
getKeyMaskMap :: Xlib.Display -> IO KeyMaskMap
getKeyMaskMap disp = do
  xmodmap <- getXModifierMap disp
  let maskFor = lookupModifierKeyMask disp xmodmap
  numlock_mask <- maskFor Xlib.xK_Num_Lock
  capslock_mask <- maskFor Xlib.xK_Caps_Lock
  shiftlock_mask <- maskFor Xlib.xK_Shift_Lock
  scrolllock_mask <- maskFor Xlib.xK_Scroll_Lock
  alt_mask <- maskFor Xlib.xK_Alt_L
  super_mask <- maskFor Xlib.xK_Super_L
  return KeyMaskMap { maskShift = Xlib.shiftMask,
                      maskControl = Xlib.controlMask,
                      maskAlt = alt_mask,
                      maskSuper = super_mask,
                      maskNumLock = numlock_mask,
                      maskCapsLock = capslock_mask,
                      maskShiftLock = shiftlock_mask,
                      maskScrollLock = scrolllock_mask
                    }

getXModifierMap :: Xlib.Display -> IO XModifierMap
getXModifierMap = XlibE.getModifierMapping

-- | Look up a modifier keymask associated to the given keysym. This
-- is necessary especially for NumLock modifier, because it is highly
-- dynamic in KeyCode realm. If no modifier is associated with the
-- 'ModifierKey', it returns 0.
--
-- c.f:
--
-- * grab_key.c of xbindkey package
-- * http://tronche.com/gui/x/xlib/input/keyboard-grabbing.html
-- * http://tronche.com/gui/x/xlib/input/keyboard-encoding.html
lookupModifierKeyMask :: Xlib.Display -> XModifierMap -> Xlib.KeySym -> IO Xlib.KeyMask
lookupModifierKeyMask disp xmmap keysym = do
  keycode <- Xlib.keysymToKeycode disp keysym
  return $ maybe 0 modifierToKeyMask $ listToMaybe $ mapMaybe (lookupXMod' keycode) xmmap
  where
    lookupXMod' key_code (xmod, codes) = if key_code `elem` codes
                                         then Just xmod
                                         else Nothing

modifierToKeyMask :: Xlib.Modifier -> Xlib.KeyMask
modifierToKeyMask = Bits.shift 1 . fromIntegral

-- | Grab the specified key on the specified window. The key is
-- captured from now on, so the window won't get that.
--
-- @since 0.2.0.0
xGrabKey :: Xlib.Display -> Xlib.Window -> Xlib.KeySym -> Xlib.KeyMask -> IO ()
xGrabKey disp win key mask = do
  code <- Xlib.keysymToKeycode disp key
  Xlib.grabKey disp code mask win False Xlib.grabModeAsync Xlib.grabModeAsync

-- grabKey throws an exception if that key for the window is already
-- grabbed by another X client. For now, we don't handle that
-- exception.

-- | Release the grab on the specified key.
--
-- @since 0.2.0.0
xUngrabKey :: Xlib.Display -> Xlib.Window -> Xlib.KeySym -> Xlib.KeyMask -> IO ()
xUngrabKey disp win key mask = do
  code <- Xlib.keysymToKeycode disp key
  Xlib.ungrabKey disp code mask win

-- | X11 key modifiers.
--
-- @since 0.2.0.0
data XMod = Shift | Ctrl | Alt | Super deriving (Bounded, Enum, Eq, Ord, Show)

-- | High-level X11 key event.
--
-- @since 0.2.0.0
data XKeyEvent
  = XKeyEvent
      { xKeyEventType   :: KeyEventType
      , xKeyEventMods   :: S.Set XMod
        -- ^ set of key modifiers enabled.
      , xKeyEventKeySym :: Xlib.KeySym
        -- ^ X11 KeySym for the key. "WildBind.X11.KeySym" re-exports
        -- 'KeySym' values.
      }
  deriving (Eq, Ord, Show)

-- | 'fromKeyEvent' always returns 'Just'.
instance XKeyInput XKeyEvent where
  toKeySym (XKeyEvent _ _ ks) = ks
  toModifierMasks kmmap (XKeyEvent _ mods _) =
    fmap (.|. xModsToKeyMask kmmap mods) $ lockVariations kmmap
  fromKeyEvent kmmap ev_type keysym mask = Just $ XKeyEvent ev_type (keyMaskToXMods kmmap mask) keysym

-- | Something that can converted to 'XKeyEvent'.
--
-- @since 0.2.0.0
class ToXKeyEvent k where
  toXKeyEvent :: k -> XKeyEvent

instance ToXKeyEvent XKeyEvent where
  toXKeyEvent = id

-- | 'KeyPress' event of KeySym with empty 'XMod' set.
instance ToXKeyEvent Xlib.KeySym where
  toXKeyEvent keysym = XKeyEvent KeyPress mempty keysym

instance (ToXKeyEvent a, ToXKeyEvent b) => ToXKeyEvent (Either a b) where
  toXKeyEvent = either toXKeyEvent toXKeyEvent

instance Describable XKeyEvent where
  describe (XKeyEvent ev mods keysym) = ev_txt <> T.pack (mods_str ++ Xlib.keysymToString keysym)
    where
      mods_str = fold $ S.map (\m -> show m ++ "+") mods
      ev_txt = case ev of
        KeyPress   -> "press "
        KeyRelease -> "release "

xModToKeyMask :: KeyMaskMap -> XMod -> Xlib.KeyMask
xModToKeyMask kmmap modi = case modi of
  Shift -> maskShift kmmap
  Ctrl  -> maskControl kmmap
  Alt   -> maskAlt kmmap
  Super -> maskSuper kmmap

xModsToKeyMask :: KeyMaskMap -> S.Set XMod -> Xlib.KeyMask
xModsToKeyMask kmmap = foldr f 0
  where
    f modi mask = xModToKeyMask kmmap modi .|. mask

lockVariations :: KeyMaskMap -> NonEmpty Xlib.KeyMask
lockVariations kmmap = toNonEmpty $ nub $ do
  numl <- [0, maskNumLock kmmap]
  capsl <- [0, maskCapsLock kmmap]
  shiftl <- [0, maskShiftLock kmmap]
  scl <- [0, maskScrollLock kmmap]
  return (numl .|. capsl .|. shiftl .|. scl)
  where
    toNonEmpty []       = return 0
    -- the result should always include 0, so the above case is not really necessary.
    toNonEmpty (x:rest) = x :| rest

keyMaskToXMods :: KeyMaskMap -> Xlib.KeyMask -> S.Set XMod
keyMaskToXMods kmmap mask = S.fromList$ toXMod =<< [ (maskShift, Shift),
                                                     (maskControl, Ctrl),
                                                     (maskAlt, Alt),
                                                     (maskSuper, Super)
                                                   ]
  where
    toXMod (acc, mod_symbol) = if isMasked kmmap acc mask
                               then [mod_symbol]
                               else []

-- | Add a 'XMod' to 'xKeyEventMods'.
--
-- @since 0.2.0.0
addXMod :: ToXKeyEvent k => XMod -> k -> XKeyEvent
addXMod modi mkey = case toXKeyEvent mkey of
  XKeyEvent ev_type mods ks -> XKeyEvent ev_type (S.insert modi mods) ks

-- | Set 'KeyPress' to 'xKeyEventType'.
--
-- @since 0.2.0.0
press :: ToXKeyEvent k => k -> XKeyEvent
press k = (toXKeyEvent k) { xKeyEventType = KeyPress }

-- | Set 'KeyRelease' to 'xKeyEventType'.
--
-- @since 0.2.0.0
release :: ToXKeyEvent k => k -> XKeyEvent
release k = (toXKeyEvent k) { xKeyEventType = KeyRelease }

-- | Add 'Shift' modifier to 'xKeyEventMods'.
--
-- @since 0.2.0.0
shift :: ToXKeyEvent k => k -> XKeyEvent
shift = addXMod Shift

-- | Add 'Ctrl' modifier to 'xKeyEventMods'.
--
-- @since 0.2.0.0
ctrl :: ToXKeyEvent k => k -> XKeyEvent
ctrl = addXMod Ctrl

-- | Add 'Alt' modifier to 'xKeyEventMods'.
--
-- @since 0.2.0.0
alt :: ToXKeyEvent k => k -> XKeyEvent
alt = addXMod Alt

-- | Add 'Super' modifier to 'xKeyEventMods'.
--
-- @since 0.2.0.0
super :: ToXKeyEvent k => k -> XKeyEvent
super = addXMod Super

-- | Send a 'XKeyEvent' to the window.
--
-- @since 0.2.0.0
xSendKeyEvent :: KeyMaskMap -> Xlib.Display -> Xlib.Window -> XKeyEvent -> IO ()
xSendKeyEvent kmmap disp target_win key_event = Xlib.allocaXEvent $ \xev -> do
  setupXEvent xev
  Xlib.sendEvent disp target_win propagate event_mask xev
  Xlib.sync disp False
  where
    propagate = True
    event_type = xKeyEventType key_event
    event_mask = case event_type of
      KeyPress   -> Xlib.keyPressMask
      KeyRelease -> Xlib.keyReleaseMask
    xevent_type = case event_type of
      KeyPress   -> Xlib.keyPress
      KeyRelease -> Xlib.keyRelease
    setupXEvent xev = do
      key_code <- Xlib.keysymToKeycode disp $ xKeyEventKeySym key_event
      XlibE.setEventType xev xevent_type
      XlibE.setKeyEvent xev target_win (Xlib.defaultRootWindow disp) subwindow key_mask key_code is_same_screen
    subwindow = 0 -- I mean, 'None' in Xlib. Graphics.X11 does not define 'None' window ID, I think...
    is_same_screen = True
    key_mask = xModsToKeyMask kmmap $ xKeyEventMods key_event

-- c.f. create_key_event function in xlib_wrapper.c from 'xremap'
-- https://github.com/k0kubun/xremap

