module Termbox.Bindings.Hs.Internal.Functions
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
where

import Data.Coerce (coerce)
import Data.Functor (void)
import Foreign.C (CInt (..))
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Storable as Storable
import System.Posix.Types (Fd (Fd))
import qualified Termbox.Bindings.C as Termbox
import Termbox.Bindings.Hs.Internal.Attrs (Tb_attrs (..))
import Termbox.Bindings.Hs.Internal.Cell (Tb_cell, cellToCCell)
import Termbox.Bindings.Hs.Internal.Event (Tb_event, ceventToEvent)
import Termbox.Bindings.Hs.Internal.InitError (Tb_init_error (..))
import Termbox.Bindings.Hs.Internal.InputMode (Tb_input_mode (..))
import Termbox.Bindings.Hs.Internal.OutputMode (Tb_output_mode (..))
import Termbox.Bindings.Hs.Internal.Prelude (charToWord32, cintToInt, intToCInt)

-- | Set a cell value in the back buffer.
tb_change_cell ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | ch
  Char ->
  -- | fg
  Tb_attrs ->
  -- | bg
  Tb_attrs ->
  IO ()
tb_change_cell cx cy c (Tb_attrs foreground) (Tb_attrs background) =
  Termbox.tb_change_cell (intToCInt cx) (intToCInt cy) (charToWord32 c) foreground background

-- | Get the input mode.
tb_get_input_mode :: IO Tb_input_mode
tb_get_input_mode =
  coerce Termbox.tb_select_input_mode Termbox._TB_INPUT_CURRENT

-- | Get the output mode.
tb_get_output_mode :: IO Tb_output_mode
tb_get_output_mode =
  coerce Termbox.tb_select_output_mode Termbox._TB_OUTPUT_CURRENT

-- | Get the terminal height.
tb_height :: IO Int
tb_height =
  cintToInt <$> Termbox.tb_height

-- | Initialize the @termbox@ library.
tb_init :: IO (Either Tb_init_error ())
tb_init = do
  code <- Termbox.tb_init
  pure
    if code == 0
      then Right ()
      else Left (Tb_init_error code)

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_fd(0)
tb_init_fd :: Fd -> IO (Either Tb_init_error ())
tb_init_fd (Fd fd) = do
  code <- Termbox.tb_init_fd fd
  pure
    if code == 0
      then Right ()
      else Left (Tb_init_error code)

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_file("/dev/tty")
tb_init_file :: FilePath -> IO (Either Tb_init_error ())
tb_init_file file = do
  code <-
    withCString file \c_file ->
      Termbox.tb_init_file c_file
  pure
    if code == 0
      then Right ()
      else Left (Tb_init_error code)

-- | Wait up to a number of milliseconds for an event.
tb_peek_event :: Int -> IO (Either () (Maybe Tb_event))
tb_peek_event timeout =
  alloca \c_event -> do
    result <- Termbox.tb_peek_event c_event (intToCInt timeout)
    if result < 0
      then pure (Left ())
      else
        if result == 0
          then pure (Right Nothing)
          else Right . Just . ceventToEvent <$> Storable.peek c_event

-- | Wait for an event.
tb_poll_event :: IO (Either () Tb_event)
tb_poll_event =
  alloca \c_event -> do
    result <- Termbox.tb_poll_event c_event
    if result < 0
      then pure (Left ())
      else Right . ceventToEvent <$> Storable.peek c_event

-- | Set a cell value in the back buffer.
tb_put_cell ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | cell
  Tb_cell ->
  IO ()
tb_put_cell cx cy cell =
  alloca \c_cell -> do
    Storable.poke c_cell (cellToCCell cell)
    Termbox.tb_put_cell (intToCInt cx) (intToCInt cy) c_cell

-- | Set the input mode.
tb_select_input_mode :: Tb_input_mode -> IO ()
tb_select_input_mode (Tb_input_mode mode) =
  void (Termbox.tb_select_input_mode mode)

-- | Set the output mode.
tb_select_output_mode :: Tb_output_mode -> IO ()
tb_select_output_mode (Tb_output_mode mode) =
  void (Termbox.tb_select_output_mode mode)

-- | Set the foreground and background attributes that 'tb_clear' clears the back buffer with.
tb_set_clear_attributes ::
  -- | fg
  Tb_attrs ->
  -- | bg
  Tb_attrs ->
  IO ()
tb_set_clear_attributes =
  coerce Termbox.tb_set_clear_attributes

-- | Set or hide the cursor location.
tb_set_cursor ::
  -- | x, y
  Maybe (Int, Int) ->
  IO ()
tb_set_cursor = \case
  Nothing -> Termbox.tb_set_cursor Termbox._TB_HIDE_CURSOR Termbox._TB_HIDE_CURSOR
  Just (cx, cy) -> Termbox.tb_set_cursor (intToCInt cx) (intToCInt cy)

-- | Get the terminal width.
tb_width :: IO Int
tb_width =
  cintToInt <$> Termbox.tb_width
