{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: WildBind.X11.Internal.GrabMan
-- Description: internal key grab manager
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. Package users should not rely on this.__
--
-- 'GrabMan' is a \"grab manager\". It manages the state of key grabs.
--
-- Of course X11 server itself manages key grabs. The reason why we
-- need 'GrabMan' is that the input symbols for X11 FrontEnd do not
-- map one-to-one to X11 'GrabField's. For example, @(press $ ctrl xK_x)@
-- actually corresponds to multiple 'GrabField's, each with a different
-- 'Xlib.KeyMask'. In addition, @(release $ ctrl xK_x)@ has exactly the
-- same set of 'GrabField's as @(press $ ctrl xK_x)@. For each possible
-- 'GrabField', we need to grab it if and only if there is at least
-- one grabbed input symbol for the 'GrabField'.
module WildBind.X11.Internal.GrabMan
    ( GrabMan
    , GrabOp (..)
    , new
    , modify
    ) where

import           Control.Monad             (forM_)
import           Data.Foldable             (foldr)
import           Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.Map.Strict           as M
import           Data.Monoid               (Monoid (..), (<>))
import qualified Data.Set                  as S
import qualified Graphics.X11.Xlib         as Xlib

import           WildBind.X11.Internal.Key (KeyEventType (..), KeyMaskMap, XKeyEvent (..),
                                            XKeyInput (..), press, xGrabKey, xUngrabKey)

-- | Unit of key grabs in X11. X server manages state of key grabs
-- independently for each 'GrabField'.
type GrabField = (Xlib.KeySym, Xlib.KeyMask)

-- | High-level grab state. For each 'GrabField', the 'M.Map' value is
-- non-empty set of input symbols (type @k@) currently grabbed.
type GrabbedInputs k = M.Map GrabField (S.Set k)

insertG :: Ord k => GrabField -> k -> GrabbedInputs k -> (GrabbedInputs k, Bool)
insertG field key inputs = (new_inputs, is_new_entry)
  where
    is_new_entry = not $ M.member field inputs
    new_inputs = M.insertWith S.union field (S.singleton key) inputs

deleteG :: Ord k => GrabField -> k -> GrabbedInputs k -> (GrabbedInputs k, Bool)
deleteG field key inputs = (new_inputs, is_entry_deleted)
  where
    (new_inputs, is_entry_deleted) = case M.lookup field inputs of
      Nothing -> (inputs, False)
      Just cur_grabbed -> let new_grabbed = S.delete key cur_grabbed
                              removed = new_grabbed == mempty
                          in ( if removed
                               then M.delete field inputs
                               else M.insert field new_grabbed inputs,

                               removed
                             )

-- | Grab operation. Either \"set grab\" or \"unset grab\".
data GrabOp = DoSetGrab | DoUnsetGrab deriving (Eq, Ord, Show)

modifyG :: Ord k => GrabOp -> GrabField -> k -> GrabbedInputs k -> (GrabbedInputs k, Bool)
modifyG op = case op of
  DoSetGrab   -> insertG
  DoUnsetGrab -> deleteG

-- | The key grab manager.
data GrabMan k
  = GrabMan
      { gmKeyMaskMap    :: KeyMaskMap
      , gmDisplay       :: Xlib.Display
      , gmRootWindow    :: Xlib.Window
      , gmGrabbedInputs :: GrabbedInputs k
      }
  deriving (Eq, Ord, Show)

-- | Create a new 'GrabMan'.
new :: KeyMaskMap -> Xlib.Display -> Xlib.Window -> IO (IORef (GrabMan k))
new kmm disp win = newIORef $ GrabMan { gmKeyMaskMap = kmm,
                                        gmDisplay = disp,
                                        gmRootWindow = win,
                                        gmGrabbedInputs = mempty
                                      }

grabFieldsFor :: XKeyInput k => KeyMaskMap -> k -> NonEmpty GrabField
grabFieldsFor kmmap k = do
  sym <- return $ toKeySym k
  modmask <- toModifierMasks kmmap k
  return (sym, modmask)

-- | Pure version of 'modify'.
modifyGM :: (XKeyInput k, Ord k) => GrabOp -> k -> GrabMan k
         -> (GrabMan k, [GrabField])
         -- ^ the next state of 'GrabMan', and the list of
         -- 'GrabField's which needs modifying with the X server.
modifyGM op input gm = foldr modifySingle (gm, []) fields
  where
    fields = grabFieldsFor (gmKeyMaskMap gm) input
    modifySingle field (cur_gm, cur_changed) = (new_gm, new_changed)
      where
        (new_gi, modified) = modifyG op field input $ gmGrabbedInputs cur_gm
        new_gm = cur_gm { gmGrabbedInputs = new_gi }
        new_changed = if modified then (field : cur_changed) else cur_changed

-- | Modify the grab state. The modification operation is specified by
-- 'GrabOp'. It controls grabs of the X server if necessary.
modify :: (XKeyInput k, Ord k) => IORef (GrabMan k) -> GrabOp -> k -> IO ()
modify gm_ref op input = do
  cur_gm <- readIORef gm_ref
  let (new_gm, changed_fields) = modifyGM op input cur_gm
      disp = gmDisplay cur_gm
      rwin = gmRootWindow cur_gm
  writeIORef gm_ref new_gm
  forM_ changed_fields $ \(keysym, mask) -> do
    case op of
     DoSetGrab   -> xGrabKey disp rwin keysym mask
     DoUnsetGrab -> xUngrabKey disp rwin keysym mask

