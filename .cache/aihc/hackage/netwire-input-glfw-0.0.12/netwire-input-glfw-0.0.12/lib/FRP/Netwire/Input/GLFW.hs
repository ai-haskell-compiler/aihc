{-|
Module      : FRP.Netwire.Input.GLFW
Description : netwire-input instances for use with GLFW
Copyright   : (c) Pavel Krajcevski, 2018
License     : MIT
Maintainer  : Krajcevski@gmail.com
Stability   : experimental
Portability : POSIX

This module contains data types with instances  needed to create wires
that can be used with the netwire-input combinators. In particular, this
package implements 'GLFWInputT' which has instances of 'MonadKeyboard' and
'MonadMouse'

-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module FRP.Netwire.Input.GLFW (
  -- * GLFW Input

  -- ** Basic Input Monad
  GLFWInput, runGLFWInput,

  -- ** Monad Transformer
  GLFWInputT, runGLFWInputT,

  -- * Typeclass
  MonadGLFWInput(..),

  -- * State Types
  GLFWInputControl, GLFWInputState,
  emptyGLFWState,
  getInput, mkInputControl, pollGLFW
) where

--------------------------------------------------------------------------------
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW as GLFW
import Control.Applicative
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Control.Monad.Fix
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Cont
import Control.Monad.Identity
import GHC.Float hiding (clamp)
import GHC.Generics

import FRP.Netwire.Input
--------------------------------------------------------------------------------

clamp :: Ord a => a -> a -> a -> a
clamp x a b
  | x < a = a
  | x > b = b
  | otherwise = x

newRange :: Floating a => a -> (a, a) -> (a, a) -> a
newRange x (omin, omax) (nmin, nmax) =
  nmin + (nmax - nmin) * ((x - omin) / (omax - omin))

newRangeC :: (Ord a, Floating a) => a -> (a, a) -> (a, a) -> a
newRangeC x o n@(nmin, nmax) = clamp (newRange x o n) nmin nmax

modeToGLFWMode :: CursorMode -> GLFW.CursorInputMode
modeToGLFWMode CursorMode'Reset = GLFW.CursorInputMode'Disabled
modeToGLFWMode CursorMode'Disabled = GLFW.CursorInputMode'Disabled
modeToGLFWMode CursorMode'Hidden = GLFW.CursorInputMode'Hidden
modeToGLFWMode CursorMode'Enabled = GLFW.CursorInputMode'Normal

-- | The GLFW input state is a record that keeps track of which buttons and keys
-- are currently pressed. Because GLFW works with callbacks, a call to pollEvents
-- must be made in order to process any of the events. At this time, all of the
-- appropriate callbacks are fired in order of the events received, and this record
-- is updated to reflect the most recent input state.
data GLFWInputState = GLFWInputState {
  keysPressed :: Map.Map GLFW.Key Int,
  keysReleased :: Set.Set GLFW.Key,
  mbPressed :: Map.Map GLFW.MouseButton Int,
  mbReleased :: Set.Set GLFW.MouseButton,
  cursorPos :: (Float, Float),
  cmode :: CursorMode,
  scrollAmt :: (Double, Double)
} deriving(Show, Generic)

instance NFData GLFWInputState

instance Key GLFW.Key
instance MouseButton GLFW.MouseButton

-- | The 'GLFWInputT' monad transformer is simply a state monad transformer using
-- 'GLFWInputState'
newtype GLFWInputT m a =
  GLFWInputT (StateT GLFWInputState m a)
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadFix
           , MonadIO
           , MonadWriter w
           , MonadReader r
           , MonadError e
           , MonadPlus
           , MonadCont
           , MonadTrans
           )

instance MonadState s m => MonadState s (GLFWInputT m) where
  get = lift get
  put = lift . put
  state = lift . state

-- | Describes a monad that provides stateful access to a 'GLFWInputState'. By
-- being able to modify the state, the context that satisfies this typeclass
-- can decide to debounce or "take ownership" of the button presses at a
-- specific point of the computation. This should be done via the 'MonadKey' and
-- 'MonadMouse' instances.
class Monad m => MonadGLFWInput m where
  -- | Retrieves the current input state
  getGLFWInput :: m GLFWInputState
  -- | Places a modified input state back into the context. This should probably
  -- not be called directly.
  putGLFWInput :: GLFWInputState -> m ()

instance Monad m => MonadGLFWInput (GLFWInputT m) where
  getGLFWInput :: GLFWInputT m GLFWInputState
  getGLFWInput = GLFWInputT get

  putGLFWInput :: GLFWInputState -> GLFWInputT m ()
  putGLFWInput = GLFWInputT . put

-- | To execute a computation with the current input snapshot, we need to give
-- supply the current 'GLFWInputState'. This comes from the 'GLFWInputControl'
-- associated with the given window.
runGLFWInputT :: GLFWInputT m a -> GLFWInputState -> m (a, GLFWInputState)
runGLFWInputT (GLFWInputT m) = runStateT m

-- | The 'GLFWInput' monad is simply the 'GLFWInputT' transformer around the
-- identity monad.
type GLFWInput = GLFWInputT Identity

-- | Runs the 'GLFWInput' computation with a current input snapshot and returns
-- the potentially modified input.
runGLFWInput :: GLFWInput a -> GLFWInputState -> (a, GLFWInputState)
runGLFWInput m = runIdentity . runGLFWInputT m

instance MonadGLFWInput m => MonadKeyboard GLFW.Key m where
  keyIsPressed :: GLFW.Key -> m Bool
  keyIsPressed key = liftM (isKeyDown key) getGLFWInput

  releaseKey :: GLFW.Key -> m ()
  releaseKey key = getGLFWInput >>= (putGLFWInput . debounceKey key)

instance MonadGLFWInput m => MonadMouse GLFW.MouseButton m where
  mbIsPressed :: GLFW.MouseButton -> m Bool
  mbIsPressed mb = liftM (isButtonPressed mb) getGLFWInput

  releaseButton :: GLFW.MouseButton -> m ()
  releaseButton mb = getGLFWInput >>= (putGLFWInput . debounceButton mb)

  cursor :: m (Float, Float)
  cursor = liftM cursorPos getGLFWInput

  setCursorMode :: CursorMode -> m ()
  setCursorMode mode = do
    ipt <- getGLFWInput
    putGLFWInput (ipt { cmode = mode })

  scroll :: m (Double, Double)
  scroll = liftM scrollAmt getGLFWInput

isKeyDown :: GLFW.Key -> GLFWInputState -> Bool
isKeyDown key = (Map.member key) . keysPressed

withPressedKey :: GLFWInputState -> GLFW.Key -> (a -> a) -> a -> a
withPressedKey input key fn
  | isKeyDown key input = fn
  | otherwise = id

debounceKey :: GLFW.Key -> GLFWInputState -> GLFWInputState
debounceKey key input = input { keysPressed = Map.delete key (keysPressed input) }

isButtonPressed :: GLFW.MouseButton -> GLFWInputState -> Bool
isButtonPressed mb = Map.member mb . mbPressed

withPressedButton :: GLFWInputState -> GLFW.MouseButton -> (a -> a) -> a -> a
withPressedButton input mb fn = if isButtonPressed mb input then fn else id

debounceButton :: GLFW.MouseButton -> GLFWInputState -> GLFWInputState
debounceButton mb input = input { mbPressed = Map.delete mb (mbPressed input) }

-- | This is an 'STM' variable that holds the current input state. It cannot be
-- manipulated directly, but it is updated by GLFW each time 'pollGLFW' is called.
data GLFWInputControl = IptCtl (TVar GLFWInputState) GLFW.Window

setCursorToWindowCenter :: GLFW.Window -> IO ()
setCursorToWindowCenter win = do
  (w, h) <- GLFW.getWindowSize win
  GLFW.setCursorPos win (fromIntegral w / 2.0) (fromIntegral h / 2.0)

-- | Returns a current snapshot of the input
getInput :: GLFWInputControl -> IO GLFWInputState
getInput (IptCtl var _) = readTVarIO var

setInput :: GLFWInputControl -> GLFWInputState -> IO ()
setInput (IptCtl var win) ipt = do
  -- Do we need to change the cursor mode?
  curMode <- GLFW.getCursorInputMode win
  let newMode = modeToGLFWMode (cmode ipt)
  unless (newMode == curMode) $
    GLFW.setCursorInputMode win newMode

  -- Write the new input
  atomically $ writeTVar var $!! (ipt { scrollAmt = (0, 0) })

resetCursorPos :: GLFWInputState -> GLFWInputState
resetCursorPos input = input { cursorPos = (0, 0) }

resolveReleased :: GLFWInputState -> GLFWInputState
resolveReleased input = input {
  keysPressed =
     (+1) <$>
     foldl (flip Map.delete) (keysPressed input) (Set.elems $ keysReleased input),
  keysReleased = Set.empty,
  mbPressed =
    (+1) <$>
    foldl (flip Map.delete) (mbPressed input) (Set.elems $ mbReleased input),
  mbReleased = Set.empty
  }

--------------------------

scrollCallback :: GLFWInputControl -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback (IptCtl ctl _) _ xoff yoff =
  atomically $ modifyTVar' ctl updateScroll
  where
    updateScroll :: GLFWInputState -> GLFWInputState
    updateScroll = (\input -> input { scrollAmt = (xoff, yoff) })

keyCallback :: GLFWInputControl -> GLFW.Window ->
               GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback (IptCtl ctl _) _ key _ keystate _ =
  atomically $ modifyTVar' ctl modifyKeys
  where
    modifyKeys :: GLFWInputState -> GLFWInputState
    modifyKeys input = case keystate of
      GLFW.KeyState'Pressed -> input {
        keysPressed = Map.union (keysPressed input) (Map.singleton key 0) }
      GLFW.KeyState'Released -> input {
        keysPressed = Map.update removeReleased key (keysPressed input),
        keysReleased =
          case Map.lookup key (keysPressed input) of
            -- If the key was just added... queue it up
            Just 0 -> Set.insert key (keysReleased input)
            -- If the key isn't pressed then it must have been debounced... do nothing
            -- If the key wasn't just added we're removing it above... do nothing...
            _ -> keysReleased input
        }
      _ -> input

    removeReleased :: Int -> Maybe Int
    removeReleased 0 = Just 0
    removeReleased _ = Nothing

mouseButtonCallback :: GLFWInputControl -> GLFW.Window ->
                       GLFW.MouseButton -> GLFW.MouseButtonState ->
                       GLFW.ModifierKeys -> IO ()
mouseButtonCallback (IptCtl ctl _) _ button state _ =
  atomically $ modifyTVar' ctl modify
  where
    modify :: GLFWInputState -> GLFWInputState
    modify input = case state of
      GLFW.MouseButtonState'Pressed -> input {
        mbPressed = Map.union (mbPressed input) (Map.singleton button 0) }
      GLFW.MouseButtonState'Released -> input {
        mbPressed = Map.update removeReleased button (mbPressed input),
        mbReleased =
          case Map.lookup button (mbPressed input) of
            Just 0 -> Set.insert button (mbReleased input)
            _ -> mbReleased input
        }

    removeReleased :: Int -> Maybe Int
    removeReleased 0 = Just 0
    removeReleased _ = Nothing

cursorPosCallback :: GLFWInputControl -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback (IptCtl ctl _) win x y = do
  (w, h) <- GLFW.getWindowSize win
  let xf = newRangeC (double2Float x) (0, fromIntegral w) (-1, 1)
      yf = newRangeC (double2Float y) (0, fromIntegral h) (-1, 1)
  atomically $ modifyTVar' ctl (\ipt -> ipt { cursorPos = (xf, yf)})

-- | Returns the empty GLFW state. In this state, no buttons are pressed, and
-- the mouse and scroll positions are set to zero. The cursor is also placed in
-- the disabled state.
emptyGLFWState :: GLFWInputState
emptyGLFWState = GLFWInputState
  { keysPressed = Map.empty
  , keysReleased = Set.empty
  , mbPressed = Map.empty
  , mbReleased = Set.empty
  , cursorPos = (0, 0)
  , cmode = CursorMode'Disabled
  , scrollAmt = (0, 0)
  }

-- | Creates and returns an 'STM' variable for the window that holds all of the
-- most recent input state information
mkInputControl :: GLFW.Window -> IO GLFWInputControl
mkInputControl win = do
  ctlvar <- newTVarIO (emptyGLFWState { cmode = CursorMode'Enabled })
  let ctl = IptCtl ctlvar win
  GLFW.setScrollCallback win (Just $ scrollCallback ctl)
  GLFW.setKeyCallback win (Just $ keyCallback ctl)
  GLFW.setCursorPosCallback win (Just $ cursorPosCallback ctl)
  GLFW.setMouseButtonCallback win (Just $ mouseButtonCallback ctl)
  return ctl

-- | Allows GLFW to interact with the windowing system to update the current
-- state. The old state must be passed in order to properly reset certain
-- properties such as the scroll wheel. The returned input state is identical
-- to a subsequent call to 'getInput' right after a call to 'GLFW.pollEvents'
pollGLFW :: GLFWInputState -> GLFWInputControl -> IO GLFWInputState
pollGLFW ipt iptctl@(IptCtl _ win) = do
  let ipt' = resolveReleased ipt

  -- Do we need to reset the cursor?
  if cmode ipt' == CursorMode'Reset
    then do
    setCursorToWindowCenter win
    setInput iptctl (resetCursorPos ipt')
    else setInput iptctl ipt'

  GLFW.pollEvents
  getInput iptctl
