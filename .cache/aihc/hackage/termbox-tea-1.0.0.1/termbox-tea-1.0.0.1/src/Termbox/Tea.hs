-- |
-- This module provides an Elm Architecture interface to @termbox@, a simple C library for writing text-based user
-- interfaces: <https://github.com/termbox/termbox>
--
-- See also:
--
-- * @<https://hackage.haskell.org/package/termbox-banana termbox-banana>@, a @reactive-banana@ FRP interface.
--
-- ==== __👉 Quick start example__
--
-- This @termbox@ program displays the number of keys pressed.
--
-- @
-- {-\# LANGUAGE BlockArguments \#-}
-- {-\# LANGUAGE DerivingStrategies \#-}
-- {-\# LANGUAGE DuplicateRecordFields \#-}
-- {-\# LANGUAGE ImportQualifiedPost \#-}
-- {-\# LANGUAGE LambdaCase \#-}
-- {-\# LANGUAGE NamedFieldPuns \#-}
-- {-\# LANGUAGE OverloadedRecordDot \#-}
--
-- import Data.Foldable (fold)
-- import Data.Function ((&))
-- import Data.Void (Void)
-- import Termbox.Tea qualified as Termbox
--
-- main :: IO ()
-- main = do
--   result \<-
--     Termbox.'run'
--       Termbox.'Program'
--         { initialize,
--           pollEvent,
--           handleEvent,
--           render,
--           finished
--         }
--   putStrLn case result of
--     Left err -\> \"Termbox program failed to initialize: \" ++ show err
--     Right state -\> \"Final state: \" ++ show state
--
-- data MyState = MyState
--   { keysPressed :: !Int,
--     pressedEsc :: !Bool
--   }
--   deriving stock (Show)
--
-- initialize :: Termbox.'Size' -\> MyState
-- initialize _size =
--   MyState
--     { keysPressed = 0,
--       pressedEsc = False
--     }
--
-- pollEvent :: Maybe (IO Void)
-- pollEvent =
--   Nothing
--
-- handleEvent :: MyState -\> Termbox.'Event' Void -\> IO MyState
-- handleEvent state = \\case
--   Termbox.'EventKey' key -\>
--     pure
--       MyState
--         { keysPressed = state.keysPressed + 1,
--           pressedEsc =
--             case key of
--               Termbox.'KeyEsc' -\> True
--               _ -\> False
--         }
--   _ -\> pure state
--
-- render :: MyState -\> Termbox.'Scene'
-- render state =
--   fold
--     [ string (\"Number of keys pressed: \" ++ show state.keysPressed),
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
-- finished :: MyState -\> Bool
-- finished state =
--   state.pressedEsc
--
-- string :: [Char] -\> Termbox.'Image'
-- string chars =
--   zip [0 ..] chars & foldMap \\(i, char) -\>
--     Termbox.char char & Termbox.atCol i
-- @
module Termbox.Tea
  ( -- * Main
    Program (..),
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
    Event (..),
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

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import qualified Ki
import Termbox
  ( Color,
    Event (..),
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
import qualified Termbox (render, run)

-- | A @termbox@ program, parameterized by state __@s@__.
data Program s = forall e.
  Program
  { -- | The initial state, given the initial terminal size.
    initialize :: Size -> s,
    -- | Poll for a user event. Every value that this @IO@ action returns is provided to @handleEvent@.
    pollEvent :: Maybe (IO e),
    -- | Handle an event.
    handleEvent :: s -> Event e -> IO s,
    -- | Render the current state.
    render :: s -> Scene,
    -- | Is the current state finished?
    finished :: s -> Bool
  }

-- | Run a @termbox@ program.
--
-- @run@ either:
--
--   * Returns immediately with an @InitError@.
--   * Returns the final state, once it's @finished@.
run :: Program s -> IO (Either InitError s)
run program =
  Termbox.run (run_ program)

run_ :: Program s -> IO s
run_ Program {initialize, pollEvent, handleEvent, render, finished} = do
  state0 <- initialize <$> getSize

  let loop0 doPoll =
        let loop s0 =
              if finished s0
                then pure s0
                else do
                  Termbox.render (render s0)
                  event <- doPoll
                  s1 <- handleEvent s0 event
                  loop s1
         in loop

  case pollEvent of
    Nothing -> loop0 poll state0
    Just pollEvent1 -> do
      eventVar <- newEmptyMVar

      Ki.scoped \scope -> do
        Ki.fork_ scope do
          forever do
            event <- pollEvent1
            putMVar eventVar (EventUser event)

        Ki.fork_ scope do
          forever do
            event <- Termbox.poll
            putMVar eventVar event

        loop0 (takeMVar eventVar) state0
