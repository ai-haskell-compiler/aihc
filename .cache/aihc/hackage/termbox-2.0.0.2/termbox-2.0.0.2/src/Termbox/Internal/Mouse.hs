module Termbox.Internal.Mouse
  ( Mouse (..),
    MouseButton
      ( MouseButton,
        LeftClick,
        MiddleClick,
        RightClick,
        ReleaseClick,
        WheelDown,
        WheelUp
      ),
  )
where

import GHC.Generics (Generic)
import Termbox.Bindings.Hs
  ( Tb_key
      ( TB_KEY_MOUSE_LEFT,
        TB_KEY_MOUSE_MIDDLE,
        TB_KEY_MOUSE_RELEASE,
        TB_KEY_MOUSE_RIGHT,
        TB_KEY_MOUSE_WHEEL_DOWN,
        TB_KEY_MOUSE_WHEEL_UP
      ),
  )
import Termbox.Internal.Pos (Pos)

-- | A mouse event.
data Mouse = Mouse
  { button :: !MouseButton,
    pos :: !Pos
  }
  deriving stock (Eq, Generic, Ord, Show)

-- | A mouse button.
newtype MouseButton
  = MouseButton Tb_key
  deriving stock (Eq, Ord)

instance Show MouseButton where
  show = \case
    LeftClick -> "LeftClick"
    MiddleClick -> "MiddleClick"
    ReleaseClick -> "ReleaseClick"
    RightClick -> "RightClick"
    WheelDown -> "WheelDown"
    WheelUp -> "WheelUp"

pattern LeftClick :: MouseButton
pattern LeftClick = MouseButton TB_KEY_MOUSE_LEFT

pattern MiddleClick :: MouseButton
pattern MiddleClick = MouseButton TB_KEY_MOUSE_MIDDLE

pattern RightClick :: MouseButton
pattern RightClick = MouseButton TB_KEY_MOUSE_RIGHT

pattern ReleaseClick :: MouseButton
pattern ReleaseClick = MouseButton TB_KEY_MOUSE_RELEASE

pattern WheelDown :: MouseButton
pattern WheelDown = MouseButton TB_KEY_MOUSE_WHEEL_DOWN

pattern WheelUp :: MouseButton
pattern WheelUp = MouseButton TB_KEY_MOUSE_WHEEL_UP

{-# COMPLETE LeftClick, MiddleClick, ReleaseClick, RightClick, WheelDown, WheelUp #-}
