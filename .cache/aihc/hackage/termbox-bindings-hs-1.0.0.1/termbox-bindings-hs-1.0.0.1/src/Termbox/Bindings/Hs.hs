-- |
-- This module provides a low-level wrapper around @termbox@, a simple C library for writing text-based user
-- interfaces: <https://github.com/termbox/termbox>
--
-- You may prefer to use one of the following interfaces instead:
--
-- * @<https://hackage.haskell.org/package/termbox termbox>@, a higher-level interface.
-- * @<https://hackage.haskell.org/package/termbox-banana termbox-banana>@, a @reactive-banana@ FRP interface.
-- * @<https://hackage.haskell.org/package/termbox-tea termbox-tea>@, an Elm Architecture interface.
module Termbox.Bindings.Hs
  ( -- * Functions

    -- ** Initialize / shutdown
    tb_init,
    tb_init_fd,
    tb_init_file,
    tb_shutdown,

    -- ** Get or set input mode
    tb_get_input_mode,
    tb_select_input_mode,

    -- ** Get or set output mode
    tb_get_output_mode,
    tb_select_output_mode,

    -- ** Get terminal dimensions
    tb_width,
    tb_height,

    -- ** Poll for events
    tb_peek_event,
    tb_poll_event,

    -- ** Set a cell
    tb_set_cursor,
    tb_put_cell,
    tb_change_cell,

    -- ** Clear and synchronize the back buffer
    tb_clear,
    tb_set_clear_attributes,
    tb_present,

    -- * Types
    Tb_cell (..),
    Tb_attrs (Tb_attrs),
    _TB_DEFAULT,
    _TB_BLACK,
    _TB_BLUE,
    _TB_CYAN,
    _TB_GREEN,
    _TB_MAGENTA,
    _TB_RED,
    _TB_WHITE,
    _TB_YELLOW,
    _TB_BOLD,
    _TB_REVERSE,
    _TB_UNDERLINE,
    Tb_event (..),
    Tb_event_mod
      ( TB_MOD_ALT,
        TB_MOD_MOTION
      ),
    Tb_event_type
      ( TB_EVENT_KEY,
        TB_EVENT_MOUSE,
        TB_EVENT_RESIZE
      ),
    Tb_init_error
      ( TB_EFAILED_TO_OPEN_TTY,
        TB_EPIPE_TRAP_ERROR,
        TB_EUNSUPPORTED_TERMINAL
      ),
    Tb_input_mode (Tb_input_mode),
    _TB_INPUT_ALT,
    _TB_INPUT_ESC,
    _TB_INPUT_MOUSE,
    Tb_key
      ( TB_KEY_ARROW_DOWN,
        TB_KEY_ARROW_LEFT,
        TB_KEY_ARROW_RIGHT,
        TB_KEY_ARROW_UP,
        TB_KEY_BACKSPACE,
        TB_KEY_BACKSPACE2,
        TB_KEY_CTRL_2,
        TB_KEY_CTRL_3,
        TB_KEY_CTRL_4,
        TB_KEY_CTRL_5,
        TB_KEY_CTRL_6,
        TB_KEY_CTRL_7,
        TB_KEY_CTRL_8,
        TB_KEY_CTRL_A,
        TB_KEY_CTRL_B,
        TB_KEY_CTRL_BACKSLASH,
        TB_KEY_CTRL_C,
        TB_KEY_CTRL_D,
        TB_KEY_CTRL_E,
        TB_KEY_CTRL_F,
        TB_KEY_CTRL_G,
        TB_KEY_CTRL_H,
        TB_KEY_CTRL_I,
        TB_KEY_CTRL_J,
        TB_KEY_CTRL_K,
        TB_KEY_CTRL_L,
        TB_KEY_CTRL_LSQ_BRACKET,
        TB_KEY_CTRL_M,
        TB_KEY_CTRL_N,
        TB_KEY_CTRL_O,
        TB_KEY_CTRL_P,
        TB_KEY_CTRL_Q,
        TB_KEY_CTRL_R,
        TB_KEY_CTRL_RSQ_BRACKET,
        TB_KEY_CTRL_S,
        TB_KEY_CTRL_SLASH,
        TB_KEY_CTRL_T,
        TB_KEY_CTRL_TILDE,
        TB_KEY_CTRL_U,
        TB_KEY_CTRL_UNDERSCORE,
        TB_KEY_CTRL_V,
        TB_KEY_CTRL_W,
        TB_KEY_CTRL_X,
        TB_KEY_CTRL_Y,
        TB_KEY_CTRL_Z,
        TB_KEY_DELETE,
        TB_KEY_END,
        TB_KEY_ENTER,
        TB_KEY_ESC,
        TB_KEY_F1,
        TB_KEY_F10,
        TB_KEY_F11,
        TB_KEY_F12,
        TB_KEY_F2,
        TB_KEY_F3,
        TB_KEY_F4,
        TB_KEY_F5,
        TB_KEY_F6,
        TB_KEY_F7,
        TB_KEY_F8,
        TB_KEY_F9,
        TB_KEY_HOME,
        TB_KEY_INSERT,
        TB_KEY_MOUSE_LEFT,
        TB_KEY_MOUSE_MIDDLE,
        TB_KEY_MOUSE_RELEASE,
        TB_KEY_MOUSE_RIGHT,
        TB_KEY_MOUSE_WHEEL_DOWN,
        TB_KEY_MOUSE_WHEEL_UP,
        TB_KEY_PGDN,
        TB_KEY_PGUP,
        TB_KEY_SPACE,
        TB_KEY_TAB
      ),
    Tb_output_mode
      ( TB_OUTPUT_216,
        TB_OUTPUT_256,
        TB_OUTPUT_GRAYSCALE,
        TB_OUTPUT_NORMAL
      ),
  )
where

import Termbox.Bindings.C (tb_clear, tb_present, tb_shutdown)
import Termbox.Bindings.Hs.Internal.Attrs
  ( Tb_attrs (..),
    _TB_BLACK,
    _TB_BLUE,
    _TB_BOLD,
    _TB_CYAN,
    _TB_DEFAULT,
    _TB_GREEN,
    _TB_MAGENTA,
    _TB_RED,
    _TB_REVERSE,
    _TB_UNDERLINE,
    _TB_WHITE,
    _TB_YELLOW,
  )
import Termbox.Bindings.Hs.Internal.Cell (Tb_cell (..))
import Termbox.Bindings.Hs.Internal.Event (Tb_event (..))
import Termbox.Bindings.Hs.Internal.EventMod (Tb_event_mod (..))
import Termbox.Bindings.Hs.Internal.EventType (Tb_event_type (..))
import Termbox.Bindings.Hs.Internal.Functions
  ( tb_change_cell,
    tb_get_input_mode,
    tb_get_output_mode,
    tb_height,
    tb_init,
    tb_init_fd,
    tb_init_file,
    tb_peek_event,
    tb_poll_event,
    tb_put_cell,
    tb_select_input_mode,
    tb_select_output_mode,
    tb_set_clear_attributes,
    tb_set_cursor,
    tb_width,
  )
import Termbox.Bindings.Hs.Internal.InitError (Tb_init_error (..))
import Termbox.Bindings.Hs.Internal.InputMode
  ( Tb_input_mode (Tb_input_mode),
    _TB_INPUT_ALT,
    _TB_INPUT_ESC,
    _TB_INPUT_MOUSE,
  )
import Termbox.Bindings.Hs.Internal.Key (Tb_key (..))
import Termbox.Bindings.Hs.Internal.OutputMode (Tb_output_mode (..))
