{-|
Module      : FRP.Netwire.Input
Description : Common wires to be used to support input in netwire
Copyright   : (c) Pavel Krajcevski, 2014
License     : MIT
Maintainer  : Krajcevski@gmail.com
Stability   : experimental
Portability : POSIX

This module contains definitions for typeclasses and wires to be used in FRP
programs that use netwire. In order to use this module, an implementation that
provides an instance of one of the underlying typeclasses 'MonadMouse' or
'MonadKeyboard' must be used. In order to not require the GLFW or SDL libraries
as dependencies, these instances are provided in separate libraries.

-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FRP.Netwire.Input where

--------------------------------------------------------------------------------
-- Requried modules
import Prelude hiding ((.), id)
import Control.DeepSeq
import Control.Monad (liftM)
import Control.Monad.Fix
import Control.Wire

import FRP.Netwire.Input.Util

import GHC.Generics
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * Mouse input

-- | Mouse button typeclass. This is used to constrain the type of Monad used
-- | to provide mouse input.
class MouseButton a

-- | The mouse cursor mode. This mode is usually dependent on whether or not
-- the mouse is in the bounds of the application window.
data CursorMode = CursorMode'Disabled -- ^ The mouse cursor is disabled
                | CursorMode'Reset    -- ^ Reset the cursor to zero between computations
                | CursorMode'Hidden   -- ^ The mouse cursor is hidden when over the application
                | CursorMode'Enabled  -- ^ The mouse cursor is enabed and
                                      -- visible over the application
                deriving (Ord, Enum, Eq, Show, Read, Generic)

instance NFData CursorMode

-- ** Mouse input

-- | This monad describes computations that involve mouse input. 
class (MouseButton mb, Monad m) => MonadMouse mb m | m -> mb where
  -- | Sets the cursor mode for all subsequent computations. Note, that many
  -- | implementations require some sort of "poll" to read the IO
  setCursorMode :: CursorMode -> m ()
  -- | Returns true if the given mouse button is pressed
  mbIsPressed :: mb -> m (Bool)
  -- | Resets the pressed state of the mouse button
  releaseButton :: mb -> m ()
  -- | Get the current cursor location
  cursor :: m (Float, Float)
  -- | Get the amount of scrolling done in the x and y directions
  scroll :: m (Double, Double)

-- ** Mouse input wires

-- | Ignores its input and returns
-- the current normalized mouse coordinates. Regardless of window size,
-- each of the returned coordinates will be in the range @[-1, 1]@.
-- 
-- * Depends: now
-- * Inhibits: never
mouseCursor :: MonadMouse mb m => Wire s e m a (Float, Float)
mouseCursor = mkGen_ $ \_ -> liftM Right cursor

-- | Returns the change in mouse coordinates between subsequent time instants
-- 
-- * Depends: before now
-- * Inhibits: never
mouseDelta :: (MonadFix m, MonadMouse mb m) => Wire s e m a (Float, Float)
mouseDelta = let
  delta (x, y) (x', y') = (x - x', y - y')
  in
   (mouseCursor >>>) $ loop $ second (delay (0, 0)) >>>
   (arr $ \(cur, last) -> (delta cur last, cur))

-- | The mouse mickies are the offset from zero at each time instant. If this
-- wire is being used, then it is assuming that the cursor mode is set to
-- 'CursorMode'Reset'
-- 
-- * Depends: now
-- * Inhibits: never
mouseMickies :: MonadMouse mb m => Wire s e m a (Float, Float)
mouseMickies =
  (mkGen_ $ \a -> do { setCursorMode CursorMode'Reset; return (Right a) }) >>>
  mouseCursor

-- | Behaves like the identity wire when the mouse button is pressed
-- and inhibits otherwise
-- 
-- * Inhibits: when the mouse button is not pressed
mousePressed :: (Monoid e, MonadMouse mb m) => mb -> Wire s e m a a
mousePressed mouse = mkGen_ $ \x -> do
  pressed <- mbIsPressed mouse
  if pressed
    then return (Right x)
    else return (Left mempty)

-- | Ignores its input and returns 'True' whenever the mouse button is pressed,
-- 'False' otherwise.
--
-- * Inhibits: never
isMousePressed :: MonadMouse mb m => mb -> Wire s e m a Bool
isMousePressed = mkGen_ . const . liftM Right . mbIsPressed

-- | Behaves like the identity wire for a signle instant when the mouse button
-- is pressed and otherwise inhibits. Note that this wire causing the button
-- to be treated as released by all other wires after the instant when it is
-- pressed.
--
-- * Depends: the instant at which the mouse button is pressed
-- * Inhibits: when the mouse button is not pressed or after it has been pressed
mouseDebounced :: (Monoid e, MonadMouse mb m) => mb -> Wire s e m a a
mouseDebounced mouse = mkGen_ $ \x -> do
  pressed <- mbIsPressed mouse
  if pressed
    then releaseButton mouse >> return (Right x)
    else return (Left mempty)

-- | Fires an event the instant the given mouse button is pressed after not
-- being pressed.
--
-- * Inhibits: never
mousePressedEvent :: MonadMouse mb m => mb -> Wire s e m a (Event a)
mousePressedEvent mb = eventToId (became id . isMousePressed mb)

-- | Fires an event the instant the given mouse button is released after being
-- pressed.
--
-- * Inhibits: never
mouseReleasedEvent :: MonadMouse mb m => mb -> Wire s e m a (Event a)
mouseReleasedEvent mb = eventToId (noLonger id . isMousePressed mb)

-- | The mouse scroll is the offset from zero at each time instant.
--
-- * Depends: now
-- * Inhibits: never
mouseScroll :: (Monoid e, MonadMouse mb m) => Wire s e m a (Double, Double)
mouseScroll = mkGen_ $ \_ -> liftM Right scroll

-- | The amount that the mouse has scrolled over the course of the entire wire.
--
-- * Depends: now
-- * Inhibits: never
mouseScrolled :: (Monoid e, MonadMouse mb m) => Wire s e m a (Double, Double)
mouseScrolled = mouseScroll >>> fn (0, 0)
  where
    fn (x, y) = mkSFN $ \(dx, dy) ->
      let result = (x + dx, y + dy)
      in (result, fn result)

-- | Behaves like the identity wire, and inhibits immediately after
-- setting the cursor mode. Common uses of this wire are to switch it
-- to the identity wire:
-- @
--   cursorMode CursorMode'Disabled --> mkId
-- @
--
-- * Inhibits: after now
cursorMode :: (MonadMouse mb m, Monoid e) => CursorMode -> Wire s e m a a
cursorMode mode =
  (mkGenN $ \x -> setCursorMode mode >> (return $ (Right x, mkEmpty)))

--------------------------------------------------------------------------------
-- * Keyboard input

-- | Key typeclass. This is used to constrain the type of Monad used
-- | to provide keyboard input.
class Key a

-- | This monad describes computations that involve keyboard input. 
class (Key k, Monad m) => MonadKeyboard k m | m -> k where
  -- | Returns true if the given key is currently pressed
  keyIsPressed :: k -> m (Bool)
  -- | Resets the pressed state of the given key.
  releaseKey :: k -> m ()

-- * Keyboard input wires

-- | Behaves like the identity wire when the key is pressed
-- and inhibits otherwise
--
-- * Inhibits: when the key is not pressed
keyPressed :: (Monoid e, MonadKeyboard k m) => k -> Wire s e m a a
keyPressed key = mkGen_ $ \x -> do
  pressed <- keyIsPressed key
  if pressed
    then return (Right x)
    else return (Left mempty)

-- | Behaves like the identity wire when the key is not pressed
-- and inhibits otherwise
--
-- * Inhibits: when the key is pressed
keyNotPressed :: (Monoid e, MonadKeyboard k m) => k -> Wire s e m a a
keyNotPressed key = mkGen_ $ \x -> do
  pressed <- keyIsPressed key
  if pressed
    then return (Left mempty)
    else return (Right x)

-- | Ignores its input and returns 'True' whenever the key is pressed, 'False'
-- otherwise.
--
-- * Inhibits: never
isKeyPressed :: MonadKeyboard k m => k -> Wire s e m a Bool
isKeyPressed = mkGen_ . const . liftM Right . keyIsPressed

-- | Behaves like the identity wire for a single instant when the key
-- is pressed and otherwise inhibits. Note that this wire causes the key
-- to be treated as released by all other wires after the instant when it is
-- pressed.
--
-- * Inhibits: when the key is not pressed or after it has been pressed
keyDebounced :: (Monoid e, MonadKeyboard k m) => k -> Wire s e m a a
keyDebounced key = mkGen_ $ \x -> do
  pressed <- keyIsPressed key
  if pressed
    then releaseKey key >> return (Right x)
    else return (Left mempty)

-- | Fires an event the instant the given key is pressed after not being pressed.
--
-- * Inhibits: never
keyPressedEvent :: MonadKeyboard k m => k -> Wire s e m a (Event a)
keyPressedEvent key = eventToId (became id . isKeyPressed key)

-- | Fires an event the instant the given key is released after being pressed.
--
-- * Inhibits: never
keyReleasedEvent :: MonadKeyboard k m => k -> Wire s e m a (Event a)
keyReleasedEvent key = eventToId (noLonger id . isKeyPressed key)
