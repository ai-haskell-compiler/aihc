module Termbox.Bindings.C
  ( -- * Functions

    -- ** Initialize / shutdown
    tb_init,
    tb_init_fd,
    tb_init_file,
    tb_shutdown,

    -- ** Get or set the input or output mode
    tb_select_input_mode,
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
    tb_cell_buffer,

    -- ** Clear and synchronize the back buffer
    tb_clear,
    tb_set_clear_attributes,
    tb_present,

    -- * Objects
    Tb_cell (..),
    Tb_event (..),

    -- * Constants

    -- ** Keys
    _TB_KEY_ARROW_DOWN,
    _TB_KEY_ARROW_LEFT,
    _TB_KEY_ARROW_RIGHT,
    _TB_KEY_ARROW_UP,
    _TB_KEY_BACKSPACE,
    _TB_KEY_BACKSPACE2,
    _TB_KEY_CTRL_2,
    _TB_KEY_CTRL_3,
    _TB_KEY_CTRL_4,
    _TB_KEY_CTRL_5,
    _TB_KEY_CTRL_6,
    _TB_KEY_CTRL_7,
    _TB_KEY_CTRL_8,
    _TB_KEY_CTRL_A,
    _TB_KEY_CTRL_B,
    _TB_KEY_CTRL_BACKSLASH,
    _TB_KEY_CTRL_C,
    _TB_KEY_CTRL_D,
    _TB_KEY_CTRL_E,
    _TB_KEY_CTRL_F,
    _TB_KEY_CTRL_G,
    _TB_KEY_CTRL_H,
    _TB_KEY_CTRL_I,
    _TB_KEY_CTRL_J,
    _TB_KEY_CTRL_K,
    _TB_KEY_CTRL_L,
    _TB_KEY_CTRL_LSQ_BRACKET,
    _TB_KEY_CTRL_M,
    _TB_KEY_CTRL_N,
    _TB_KEY_CTRL_O,
    _TB_KEY_CTRL_P,
    _TB_KEY_CTRL_Q,
    _TB_KEY_CTRL_R,
    _TB_KEY_CTRL_RSQ_BRACKET,
    _TB_KEY_CTRL_S,
    _TB_KEY_CTRL_SLASH,
    _TB_KEY_CTRL_T,
    _TB_KEY_CTRL_TILDE,
    _TB_KEY_CTRL_U,
    _TB_KEY_CTRL_UNDERSCORE,
    _TB_KEY_CTRL_V,
    _TB_KEY_CTRL_W,
    _TB_KEY_CTRL_X,
    _TB_KEY_CTRL_Y,
    _TB_KEY_CTRL_Z,
    _TB_KEY_DELETE,
    _TB_KEY_END,
    _TB_KEY_ENTER,
    _TB_KEY_ESC,
    _TB_KEY_F1,
    _TB_KEY_F10,
    _TB_KEY_F11,
    _TB_KEY_F12,
    _TB_KEY_F2,
    _TB_KEY_F3,
    _TB_KEY_F4,
    _TB_KEY_F5,
    _TB_KEY_F6,
    _TB_KEY_F7,
    _TB_KEY_F8,
    _TB_KEY_F9,
    _TB_KEY_HOME,
    _TB_KEY_INSERT,
    _TB_KEY_MOUSE_LEFT,
    _TB_KEY_MOUSE_MIDDLE,
    _TB_KEY_MOUSE_RELEASE,
    _TB_KEY_MOUSE_RIGHT,
    _TB_KEY_MOUSE_WHEEL_DOWN,
    _TB_KEY_MOUSE_WHEEL_UP,
    _TB_KEY_PGDN,
    _TB_KEY_PGUP,
    _TB_KEY_SPACE,
    _TB_KEY_TAB,

    -- ** Colors
    _TB_DEFAULT,
    _TB_BLACK,
    _TB_BLUE,
    _TB_CYAN,
    _TB_GREEN,
    _TB_MAGENTA,
    _TB_RED,
    _TB_WHITE,
    _TB_YELLOW,

    -- ** Attributes
    _TB_BOLD,
    _TB_REVERSE,
    _TB_UNDERLINE,

    -- ** Event types
    _TB_EVENT_KEY,
    _TB_EVENT_MOUSE,
    _TB_EVENT_RESIZE,

    -- ** Event modifiers
    _TB_MOD_ALT,
    _TB_MOD_MOTION,

    -- ** 'tb_init' error codes
    _TB_EFAILED_TO_OPEN_TTY,
    _TB_EPIPE_TRAP_ERROR,
    _TB_EUNSUPPORTED_TERMINAL,

    -- ** Hide cursor
    _TB_HIDE_CURSOR,

    -- ** Input modes
    _TB_INPUT_CURRENT,
    _TB_INPUT_ALT,
    _TB_INPUT_ESC,
    _TB_INPUT_MOUSE,

    -- ** Output modes
    _TB_OUTPUT_CURRENT,
    _TB_OUTPUT_216,
    _TB_OUTPUT_256,
    _TB_OUTPUT_GRAYSCALE,
    _TB_OUTPUT_NORMAL,
  )
where

import Data.Int (Int32)
import Data.Word (Word16, Word32, Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (CInt))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable
import GHC.Generics (Generic)
import Prelude hiding (mod)

------------------------------------------------------------------------------------------------------------------------
-- Functions

-- | Get a pointer to the back buffer.
foreign import capi unsafe "termbox.h tb_cell_buffer"
  tb_cell_buffer :: IO (Ptr Tb_cell)

-- | Set a cell value in the back buffer.
foreign import capi unsafe "termbox.h tb_change_cell"
  tb_change_cell ::
    -- | x
    CInt ->
    -- | y
    CInt ->
    -- | ch
    Word32 ->
    -- | fg
    Word16 ->
    -- | bg
    Word16 ->
    IO ()

-- | Clear the back buffer.
foreign import capi unsafe "termbox.h tb_clear"
  tb_clear :: IO ()

-- | Get the terminal height.
foreign import capi unsafe "termbox.h tb_height"
  tb_height :: IO CInt

-- | Initialize the @termbox@ library.
foreign import capi unsafe "termbox.h tb_init"
  tb_init :: IO CInt

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_file("/dev/tty")
foreign import capi unsafe "termbox.h tb_init_file"
  tb_init_file :: CString -> IO CInt

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_fd(0)
foreign import capi unsafe "termbox.h tb_init_fd"
  tb_init_fd :: CInt -> IO CInt

-- | Wait up to a number of milliseconds for an event.
foreign import capi interruptible "termbox.h tb_peek_event"
  tb_peek_event :: Ptr Tb_event -> CInt -> IO CInt

-- | Wait for an event.
foreign import capi interruptible "termbox.h tb_poll_event"
  tb_poll_event :: Ptr Tb_event -> IO CInt

-- | Synchronize the back buffer with the terminal.
foreign import capi unsafe "termbox.h tb_present"
  tb_present :: IO ()

-- | Set a cell value in the back buffer.
foreign import capi unsafe "termbox.h tb_put_cell"
  tb_put_cell ::
    -- | x
    CInt ->
    -- | y
    CInt ->
    -- | cell
    Ptr Tb_cell ->
    IO ()

-- | Get or set the input mode.
foreign import capi unsafe "termbox.h tb_select_input_mode"
  tb_select_input_mode :: CInt -> IO CInt

-- | Get or set the output mode.
foreign import capi unsafe "termbox.h tb_select_output_mode"
  tb_select_output_mode :: CInt -> IO CInt

-- | Set the foreground and background attributes that 'tb_clear' clears the back buffer with.
foreign import capi unsafe "termbox.h tb_set_clear_attributes"
  tb_set_clear_attributes ::
    -- | fg
    Word16 ->
    -- | bg
    Word16 ->
    IO ()

-- | Set or hide the cursor location.
foreign import capi unsafe "termbox.h tb_set_cursor"
  tb_set_cursor ::
    -- | x
    CInt ->
    -- | y
    CInt ->
    IO ()

-- | Shutdown the @termbox@ library.
foreign import capi unsafe "termbox.h tb_shutdown"
  tb_shutdown :: IO ()

-- | Get the terminal width.
foreign import capi unsafe "termbox.h tb_width"
  tb_width :: IO CInt

------------------------------------------------------------------------------------------------------------------------
-- Objects

-- | A cell.
data {-# CTYPE "termbox.h" "struct tb_cell" #-} Tb_cell = Tb_cell
  { -- | A unicode character.
    ch :: {-# UNPACK #-} !Word32,
    -- | Foreground attributes.
    fg :: {-# UNPACK #-} !Word16,
    -- | Background attributes.
    bg :: {-# UNPACK #-} !Word16
  }
  deriving stock (Eq, Generic, Ord, Show)

instance Storable Tb_cell where
  sizeOf _ = 8
  alignment _ = 4

  peek :: Ptr Tb_cell -> IO Tb_cell
  peek ptr =
    Tb_cell
      <$> Storable.peekByteOff ptr 0
      <*> Storable.peekByteOff ptr 4
      <*> Storable.peekByteOff ptr 6

  poke :: Ptr Tb_cell -> Tb_cell -> IO ()
  poke ptr Tb_cell {ch, fg, bg} = do
    Storable.pokeByteOff ptr 0 ch
    Storable.pokeByteOff ptr 4 fg
    Storable.pokeByteOff ptr 6 bg

-- | An event.
data {-# CTYPE "termbox.h" "struct tb_event" #-} Tb_event = Tb_event
  { type_ :: {-# UNPACK #-} !Word8,
    mod :: {-# UNPACK #-} !Word8,
    key :: {-# UNPACK #-} !Word16,
    ch :: {-# UNPACK #-} !Word32,
    w :: {-# UNPACK #-} !Int32,
    h :: {-# UNPACK #-} !Int32,
    x :: {-# UNPACK #-} !Int32,
    y :: {-# UNPACK #-} !Int32
  }
  deriving stock (Eq, Generic, Ord, Show)

instance Storable Tb_event where
  sizeOf _ = 24
  alignment _ = 4

  peek :: Ptr Tb_event -> IO Tb_event
  peek ptr =
    Tb_event
      <$> Storable.peekByteOff ptr 0
      <*> Storable.peekByteOff ptr 1
      <*> Storable.peekByteOff ptr 2
      <*> Storable.peekByteOff ptr 4
      <*> Storable.peekByteOff ptr 8
      <*> Storable.peekByteOff ptr 12
      <*> Storable.peekByteOff ptr 16
      <*> Storable.peekByteOff ptr 20

  poke :: Ptr Tb_event -> Tb_event -> IO ()
  poke ptr Tb_event {type_, mod, key, ch, w, h, x, y} = do
    Storable.pokeByteOff ptr 0 type_
    Storable.pokeByteOff ptr 1 mod
    Storable.pokeByteOff ptr 2 key
    Storable.pokeByteOff ptr 4 ch
    Storable.pokeByteOff ptr 8 w
    Storable.pokeByteOff ptr 12 h
    Storable.pokeByteOff ptr 16 x
    Storable.pokeByteOff ptr 20 y

------------------------------------------------------------------------------------------------------------------------
-- Constants

_TB_KEY_ARROW_DOWN, _TB_KEY_ARROW_LEFT, _TB_KEY_ARROW_RIGHT, _TB_KEY_ARROW_UP, _TB_KEY_BACKSPACE, _TB_KEY_BACKSPACE2, _TB_KEY_CTRL_2, _TB_KEY_CTRL_3, _TB_KEY_CTRL_4, _TB_KEY_CTRL_5, _TB_KEY_CTRL_6, _TB_KEY_CTRL_7, _TB_KEY_CTRL_8, _TB_KEY_CTRL_A, _TB_KEY_CTRL_B, _TB_KEY_CTRL_BACKSLASH, _TB_KEY_CTRL_C, _TB_KEY_CTRL_D, _TB_KEY_CTRL_E, _TB_KEY_CTRL_F, _TB_KEY_CTRL_G, _TB_KEY_CTRL_H, _TB_KEY_CTRL_I, _TB_KEY_CTRL_J, _TB_KEY_CTRL_K, _TB_KEY_CTRL_L, _TB_KEY_CTRL_LSQ_BRACKET, _TB_KEY_CTRL_M, _TB_KEY_CTRL_N, _TB_KEY_CTRL_O, _TB_KEY_CTRL_P, _TB_KEY_CTRL_Q, _TB_KEY_CTRL_R, _TB_KEY_CTRL_RSQ_BRACKET, _TB_KEY_CTRL_S, _TB_KEY_CTRL_SLASH, _TB_KEY_CTRL_T, _TB_KEY_CTRL_TILDE, _TB_KEY_CTRL_U, _TB_KEY_CTRL_UNDERSCORE, _TB_KEY_CTRL_V, _TB_KEY_CTRL_W, _TB_KEY_CTRL_X, _TB_KEY_CTRL_Y, _TB_KEY_CTRL_Z, _TB_KEY_DELETE, _TB_KEY_END, _TB_KEY_ENTER, _TB_KEY_ESC, _TB_KEY_F1, _TB_KEY_F10, _TB_KEY_F11, _TB_KEY_F12, _TB_KEY_F2, _TB_KEY_F3, _TB_KEY_F4, _TB_KEY_F5, _TB_KEY_F6, _TB_KEY_F7, _TB_KEY_F8, _TB_KEY_F9, _TB_KEY_HOME, _TB_KEY_INSERT, _TB_KEY_MOUSE_LEFT, _TB_KEY_MOUSE_MIDDLE, _TB_KEY_MOUSE_RELEASE, _TB_KEY_MOUSE_RIGHT, _TB_KEY_MOUSE_WHEEL_DOWN, _TB_KEY_MOUSE_WHEEL_UP, _TB_KEY_PGDN, _TB_KEY_PGUP, _TB_KEY_SPACE, _TB_KEY_TAB :: Word16
_TB_KEY_ARROW_DOWN = 0xFFFF - 19
_TB_KEY_ARROW_LEFT = 0xFFFF - 20
_TB_KEY_ARROW_RIGHT = 0xFFFF - 21
_TB_KEY_ARROW_UP = 0xFFFF - 18
_TB_KEY_BACKSPACE = 0x08
_TB_KEY_BACKSPACE2 = 0x7F
_TB_KEY_CTRL_2 = 0x00
_TB_KEY_CTRL_3 = 0x1B
_TB_KEY_CTRL_4 = 0x1C
_TB_KEY_CTRL_5 = 0x1D
_TB_KEY_CTRL_6 = 0x1E
_TB_KEY_CTRL_7 = 0x1F
_TB_KEY_CTRL_8 = 0x7F
_TB_KEY_CTRL_A = 0x01
_TB_KEY_CTRL_B = 0x02
_TB_KEY_CTRL_BACKSLASH = 0x1C
_TB_KEY_CTRL_C = 0x03
_TB_KEY_CTRL_D = 0x04
_TB_KEY_CTRL_E = 0x05
_TB_KEY_CTRL_F = 0x06
_TB_KEY_CTRL_G = 0x07
_TB_KEY_CTRL_H = 0x08
_TB_KEY_CTRL_I = 0x09
_TB_KEY_CTRL_J = 0x0A
_TB_KEY_CTRL_K = 0x0B
_TB_KEY_CTRL_L = 0x0C
_TB_KEY_CTRL_LSQ_BRACKET = 0x1B
_TB_KEY_CTRL_M = 0x0D
_TB_KEY_CTRL_N = 0x0E
_TB_KEY_CTRL_O = 0x0F
_TB_KEY_CTRL_P = 0x10
_TB_KEY_CTRL_Q = 0x11
_TB_KEY_CTRL_R = 0x12
_TB_KEY_CTRL_RSQ_BRACKET = 0x1D
_TB_KEY_CTRL_S = 0x13
_TB_KEY_CTRL_SLASH = 0x1F
_TB_KEY_CTRL_T = 0x14
_TB_KEY_CTRL_TILDE = 0x00
_TB_KEY_CTRL_U = 0x15
_TB_KEY_CTRL_UNDERSCORE = 0x1F
_TB_KEY_CTRL_V = 0x16
_TB_KEY_CTRL_W = 0x17
_TB_KEY_CTRL_X = 0x18
_TB_KEY_CTRL_Y = 0x19
_TB_KEY_CTRL_Z = 0x1A
_TB_KEY_DELETE = 0xFFFF - 13
_TB_KEY_END = 0xFFFF - 15
_TB_KEY_ENTER = 0x0D
_TB_KEY_ESC = 0x1B
_TB_KEY_F1 = 0xFFFF - 0
_TB_KEY_F10 = 0xFFFF - 9
_TB_KEY_F11 = 0xFFFF - 10
_TB_KEY_F12 = 0xFFFF - 11
_TB_KEY_F2 = 0xFFFF - 1
_TB_KEY_F3 = 0xFFFF - 2
_TB_KEY_F4 = 0xFFFF - 3
_TB_KEY_F5 = 0xFFFF - 4
_TB_KEY_F6 = 0xFFFF - 5
_TB_KEY_F7 = 0xFFFF - 6
_TB_KEY_F8 = 0xFFFF - 7
_TB_KEY_F9 = 0xFFFF - 8
_TB_KEY_HOME = 0xFFFF - 14
_TB_KEY_INSERT = 0xFFFF - 12
_TB_KEY_MOUSE_LEFT = 0xFFFF - 22
_TB_KEY_MOUSE_MIDDLE = 0xFFFF - 24
_TB_KEY_MOUSE_RELEASE = 0xFFFF - 25
_TB_KEY_MOUSE_RIGHT = 0xFFFF - 23
_TB_KEY_MOUSE_WHEEL_DOWN = 0xFFFF - 27
_TB_KEY_MOUSE_WHEEL_UP = 0xFFFF - 26
_TB_KEY_PGDN = 0xFFFF - 17
_TB_KEY_PGUP = 0xFFFF - 16
_TB_KEY_SPACE = 0x20
_TB_KEY_TAB = 0x09

_TB_MOD_ALT, _TB_MOD_MOTION :: Word8
_TB_MOD_ALT = 0x01
_TB_MOD_MOTION = 0x02

_TB_DEFAULT, _TB_BLACK, _TB_BLUE, _TB_CYAN, _TB_GREEN, _TB_MAGENTA, _TB_RED, _TB_WHITE, _TB_YELLOW :: Word16
_TB_DEFAULT = 0x00
_TB_BLACK = 0x01
_TB_BLUE = 0x05
_TB_CYAN = 0x07
_TB_GREEN = 0x03
_TB_MAGENTA = 0x06
_TB_RED = 0x02
_TB_WHITE = 0x08
_TB_YELLOW = 0x04

_TB_BOLD, _TB_REVERSE, _TB_UNDERLINE :: Word16
_TB_BOLD = 0x0100
_TB_REVERSE = 0x0400
_TB_UNDERLINE = 0x0200

_TB_EVENT_KEY, _TB_EVENT_MOUSE, _TB_EVENT_RESIZE :: Word8
_TB_EVENT_KEY = 1
_TB_EVENT_MOUSE = 3
_TB_EVENT_RESIZE = 2

_TB_EFAILED_TO_OPEN_TTY, _TB_EPIPE_TRAP_ERROR, _TB_EUNSUPPORTED_TERMINAL :: CInt
_TB_EFAILED_TO_OPEN_TTY = -2
_TB_EPIPE_TRAP_ERROR = -3
_TB_EUNSUPPORTED_TERMINAL = -1

_TB_HIDE_CURSOR :: CInt
_TB_HIDE_CURSOR = -1

_TB_INPUT_CURRENT, _TB_INPUT_ALT, _TB_INPUT_ESC, _TB_INPUT_MOUSE :: CInt
_TB_INPUT_CURRENT = 0b000
_TB_INPUT_ALT = 0b010
_TB_INPUT_ESC = 0b001
_TB_INPUT_MOUSE = 0b100

_TB_OUTPUT_CURRENT, _TB_OUTPUT_216, _TB_OUTPUT_256, _TB_OUTPUT_GRAYSCALE, _TB_OUTPUT_NORMAL :: CInt
_TB_OUTPUT_CURRENT = 0
_TB_OUTPUT_216 = 3
_TB_OUTPUT_256 = 2
_TB_OUTPUT_GRAYSCALE = 4
_TB_OUTPUT_NORMAL = 1
