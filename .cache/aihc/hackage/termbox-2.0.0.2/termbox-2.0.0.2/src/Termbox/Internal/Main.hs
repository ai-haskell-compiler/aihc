module Termbox.Internal.Main
  ( run,
    initialize,
    finalize,
    InitError (..),
  )
where

import Control.Exception (Exception, mask, onException)
import Termbox.Bindings.Hs

-- | @termbox@ initialization errors.
data InitError
  = FailedToOpenTTY
  | PipeTrapError
  | UnsupportedTerminal
  deriving anyclass (Exception)
  deriving stock (Show)

-- | Initialize a @termbox@ program, and if that succeeds, run the provided action, then finalize the @termbox@ program.
run :: IO a -> IO (Either InitError a)
run action =
  mask \unmask ->
    initialize >>= \case
      Left err -> pure (Left err)
      Right () -> do
        result <- unmask action `onException` finalize
        finalize
        pure (Right result)

-- | Initialize a @termbox@ program.
--
-- If @initialize@ succeeds, it must be paired with a call to 'finalize'.
initialize :: IO (Either InitError ())
initialize =
  tb_init >>= \case
    Left err ->
      (pure . Left) case err of
        TB_EFAILED_TO_OPEN_TTY -> FailedToOpenTTY
        TB_EPIPE_TRAP_ERROR -> PipeTrapError
        TB_EUNSUPPORTED_TERMINAL -> UnsupportedTerminal
    Right () -> do
      tb_select_input_mode _TB_INPUT_MOUSE
      tb_select_output_mode TB_OUTPUT_256
      pure (Right ())

-- | Shut down a @termbox@ program.
finalize :: IO ()
finalize = do
  tb_select_output_mode TB_OUTPUT_NORMAL
  tb_shutdown
