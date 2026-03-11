-- |
-- This module provides a @reactive-banana@ FRP interface to @termbox@, a simple C library for writing text-based user
-- interfaces: <https://github.com/termbox/termbox>
--
-- See also:
--
-- * @<https://hackage.haskell.org/package/termbox-tea termbox-tea>@, an Elm Architecture interface.
--
-- ==== __👉 Quick start example__
--
-- This @termbox@ program displays the number of keys pressed.
--
-- @
-- {-\# LANGUAGE BlockArguments \#-}
-- {-\# LANGUAGE DuplicateRecordFields \#-}
-- {-\# LANGUAGE ImportQualifiedPost \#-}
-- {-\# LANGUAGE LambdaCase \#-}
-- {-\# LANGUAGE OverloadedRecordDot \#-}
--
-- module Main (main) where
--
-- import Data.Foldable (fold)
-- import Data.Function ((&))
-- import Reactive.Banana ((\<\@\>))
-- import Reactive.Banana qualified as Banana
-- import Termbox.Banana qualified as Termbox
--
-- main :: IO ()
-- main = do
--   result \<- Termbox.'run' network
--   putStrLn case result of
--     Left err -\> \"Termbox program failed to initialize: \" ++ show err
--     Right state -\> \"Final state: \" ++ show state
--
-- network :: (Banana.MonadMoment m) =\> Termbox.'Inputs' -\> m (Termbox.'Outputs' Int)
-- network inputs = do
--   keysPressed \<- Banana.accumB 0 ((+ 1) \<$ inputs.keys)
--   pure
--     Termbox.'Outputs'
--       { scene = render \<$\> keysPressed,
--         done = Banana.filterJust (isDone \<$\> keysPressed \<\@\> inputs.keys)
--       }
--   where
--     isDone :: Int -\> Termbox.'Key' -\> Maybe Int
--     isDone n = \\case
--       Termbox.'KeyEsc' -\> Just n
--       _ -\> Nothing
--
-- render :: Int -\> Termbox.'Scene'
-- render keysPressed =
--   fold
--     [ string (\"Number of keys pressed: \" ++ show keysPressed),
--       fold
--         [ string \"Press\",
--           string \"Esc\" & Termbox.'bold' & Termbox.'atCol' 6,
--           string \"to quit.\" & Termbox.'atCol' 10
--         ]
--         & Termbox.'atRow' 2
--     ]
--     & Termbox.'at' Termbox.'Pos' {row = 2, col = 4}
--     & Termbox.'image'
--
-- string :: [Char] -\> Termbox.'Image'
-- string chars =
--   zip [0 ..] chars & foldMap \\(i, char) -\>
--     Termbox.'char' char & Termbox.'atCol' i
-- @
module Termbox.Banana
  ( -- * Main
    Inputs (..),
    Outputs (..),
    run,
    InitError (..),

    -- * Terminal contents

    -- ** Scene
    Scene,
    image,
    fill,
    cursor,

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
    Key (..),
    Mouse (..),
    MouseButton (..),

    -- * Miscellaneous types
    Pos (..),
    posUp,
    posDown,
    posLeft,
    posRight,
    Size (..),
  )
where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Data.Void (Void)
import qualified Reactive.Banana as Banana
import qualified Reactive.Banana.Frameworks as Banana
import Termbox
  ( Color,
    Image,
    InitError (..),
    Key (..),
    Mouse (..),
    MouseButton (..),
    Pos (..),
    Scene,
    Size (..),
    at,
    atCol,
    atRow,
    bg,
    blink,
    blue,
    bold,
    bright,
    char,
    color,
    cursor,
    cyan,
    defaultColor,
    fg,
    fill,
    getSize,
    gray,
    green,
    image,
    magenta,
    poll,
    posDown,
    posLeft,
    posRight,
    posUp,
    red,
    underline,
    white,
    yellow,
  )
import qualified Termbox (Event (..), render, run)

-- | The inputs to a @termbox@ FRP network.
data Inputs = Inputs
  { -- | The initial terminal size.
    initialSize :: !Size,
    -- | Key events.
    keys :: !(Banana.Event Key),
    -- | Resize events.
    resizes :: !(Banana.Event Size),
    -- | Mouse events.
    mouses :: !(Banana.Event Mouse)
  }

-- | The outputs from a @termbox@ FRP network.
data Outputs a = Outputs
  { -- | The scene to render.
    scene :: !(Banana.Behavior Scene),
    -- | The events of arbitrary values, on the first of which is relevant, which causes 'run' to return.
    --
    -- /Note/: Wrapping this event in 'Banana.once' is not necessary, as this library does so internally.
    done :: !(Banana.Event a)
  }

-- | Run a @termbox@ FRP network.
--
-- @run@ either:
--
--   * Returns immediately with an 'InitError'.
--   * Returns the first value emitted by @done@.
run ::
  -- | The FRP network.
  (Inputs -> Banana.MomentIO (Outputs a)) ->
  -- | The result of the FRP network.
  IO (Either InitError a)
run program =
  Termbox.run (run_ program)

run_ :: (Inputs -> Banana.MomentIO (Outputs a)) -> IO a
run_ program = do
  initialSize <- getSize

  doneVar <- newEmptyMVar
  (keysAddHandler, fireKey) <- Banana.newAddHandler
  (resizesAddHandler, fireResize) <- Banana.newAddHandler
  (mousesAddHandler, fireMouse) <- Banana.newAddHandler

  network <-
    Banana.compile do
      keys <- Banana.fromAddHandler keysAddHandler
      resizes <- Banana.fromAddHandler resizesAddHandler
      mouses <- Banana.fromAddHandler mousesAddHandler

      Outputs {scene, done} <- program Inputs {initialSize, keys, resizes, mouses}
      let render = Termbox.render <$> scene

      -- Render the first scene, and again every time it changes.
      liftIO =<< Banana.valueB render
      Banana.reactimate' =<< Banana.changes render

      -- Smuggle `done` values out via `doneVar` (only the first matters)
      done1 <- Banana.once done
      Banana.reactimate (putMVar doneVar <$> done1)

  Banana.actuate network

  let loop = do
        poll @Void >>= \case
          Termbox.EventKey key -> fireKey key
          Termbox.EventResize size -> fireResize size
          Termbox.EventMouse mouse -> fireMouse mouse
        tryReadMVar doneVar >>= \case
          Nothing -> loop
          Just result -> pure result

  loop
