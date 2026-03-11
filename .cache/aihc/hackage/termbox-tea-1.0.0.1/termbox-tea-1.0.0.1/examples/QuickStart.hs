{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.Foldable (fold)
import Data.Function ((&))
import Data.Void (Void)
import Termbox.Tea qualified as Termbox

main :: IO ()
main = do
  result <-
    Termbox.run
      Termbox.Program
        { initialize,
          pollEvent,
          handleEvent,
          render,
          finished
        }
  putStrLn case result of
    Left err -> "Termbox program failed to initialize: " ++ show err
    Right state -> "Final state: " ++ show state

data MyState = MyState
  { keysPressed :: !Int,
    pressedEsc :: !Bool
  }
  deriving stock (Show)

initialize :: Termbox.Size -> MyState
initialize _size =
  MyState
    { keysPressed = 0,
      pressedEsc = False
    }

pollEvent :: Maybe (IO Void)
pollEvent =
  Nothing

handleEvent :: MyState -> Termbox.Event Void -> IO MyState
handleEvent state = \case
  Termbox.EventKey key ->
    pure
      MyState
        { keysPressed = state.keysPressed + 1,
          pressedEsc =
            case key of
              Termbox.KeyEsc -> True
              _ -> False
        }
  _ -> pure state

render :: MyState -> Termbox.Scene
render state =
  fold
    [ string ("Number of keys pressed: " ++ show state.keysPressed),
      fold
        [ string "Press",
          string "Esc" & Termbox.bold & Termbox.atCol 6,
          string "to quit." & Termbox.atCol 10
        ]
        & Termbox.atRow 2
    ]
    & Termbox.at Termbox.Pos {row = 2, col = 4}
    & Termbox.image

finished :: MyState -> Bool
finished state =
  state.pressedEsc

string :: [Char] -> Termbox.Image
string chars =
  zip [0 ..] chars & foldMap \(i, char) ->
    Termbox.char char & Termbox.atCol i
