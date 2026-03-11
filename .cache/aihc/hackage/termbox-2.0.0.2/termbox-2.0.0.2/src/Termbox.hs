-- |
-- This module provides a high-level wrapper around @termbox@, a simple C library for writing text-based user
-- interfaces: <https://github.com/termbox/termbox>
--
-- You may prefer to use one of the following interfaces instead:
--
-- * @<https://hackage.haskell.org/package/termbox-banana termbox-banana>@, a @reactive-banana@ FRP interface.
-- * @<https://hackage.haskell.org/package/termbox-tea termbox-tea>@, an Elm Architecture interface.
module Termbox
  ( -- * Main
    run,
    initialize,
    finalize,
    InitError (..),

    -- * Terminal contents

    -- ** Scene
    Scene,
    image,
    fill,
    cursor,
    render,

    -- ** Image
    Image,
    char,

    -- *** Color
    fg,
    bg,

    -- *** Style
    bold,
    underline,
    blink,

    -- *** Translation
    at,
    atRow,
    atCol,

    -- ** Colors
    Color,

    -- *** Basic colors
    defaultColor,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    bright,

    -- *** 216 miscellaneous colors
    color,

    -- *** 24 monochrome colors
    gray,

    -- * Event handling
    Event (..),
    Key
      ( ..,
        KeyCtrlH,
        KeyCtrlLsqBracket,
        KeyCtrl2,
        KeyCtrl3,
        KeyCtrl4,
        KeyCtrl5,
        KeyCtrl7,
        KeyCtrlM,
        KeyCtrlI,
        KeyCtrlUnderscore
      ),
    Mouse (..),
    MouseButton (..),
    poll,

    -- * Miscellaneous types
    Pos (..),
    posUp,
    posDown,
    posLeft,
    posRight,
    Size (..),
    getSize,
  )
where

import Termbox.Internal.Color
  ( Color,
    blue,
    bright,
    color,
    cyan,
    defaultColor,
    gray,
    green,
    magenta,
    red,
    white,
    yellow,
  )
import Termbox.Internal.Event (Event (..), poll)
import Termbox.Internal.Image (Image, at, atCol, atRow, bg, blink, bold, char, fg, underline)
import Termbox.Internal.Key
  ( Key (..),
    pattern KeyCtrl2,
    pattern KeyCtrl3,
    pattern KeyCtrl4,
    pattern KeyCtrl5,
    pattern KeyCtrl7,
    pattern KeyCtrlH,
    pattern KeyCtrlI,
    pattern KeyCtrlLsqBracket,
    pattern KeyCtrlM,
    pattern KeyCtrlUnderscore,
  )
import Termbox.Internal.Main (InitError (..), finalize, initialize, run)
import Termbox.Internal.Mouse
  ( Mouse (..),
    MouseButton
      ( LeftClick,
        MiddleClick,
        ReleaseClick,
        RightClick,
        WheelDown,
        WheelUp
      ),
  )
import Termbox.Internal.Pos (Pos (..), posDown, posLeft, posRight, posUp)
import Termbox.Internal.Scene (Scene, cursor, fill, image, render)
import Termbox.Internal.Size (Size (..), getSize)
