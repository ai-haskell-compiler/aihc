{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Foldable (fold)
import Data.Function ((&))
import Reactive.Banana ((<@>))
import Reactive.Banana qualified as Banana
import Termbox.Banana qualified as Termbox

main :: IO ()
main = do
  result <- Termbox.run network
  putStrLn case result of
    Left err -> "Termbox program failed to initialize: " ++ show err
    Right state -> "Final state: " ++ show state

network :: (Banana.MonadMoment m) => Termbox.Inputs -> m (Termbox.Outputs Int)
network inputs = do
  keysPressed <- Banana.accumB 0 ((+ 1) <$ inputs.keys)
  pure
    Termbox.Outputs
      { scene = render <$> keysPressed,
        done = Banana.filterJust (isDone <$> keysPressed <@> inputs.keys)
      }
  where
    isDone :: Int -> Termbox.Key -> Maybe Int
    isDone n = \case
      Termbox.KeyEsc -> Just n
      _ -> Nothing

render :: Int -> Termbox.Scene
render keysPressed =
  fold
    [ string ("Number of keys pressed: " ++ show keysPressed),
      fold
        [ string "Press",
          string "Esc" & Termbox.bold & Termbox.atCol 6,
          string "to quit." & Termbox.atCol 10
        ]
        & Termbox.atRow 2
    ]
    & Termbox.at Termbox.Pos {row = 2, col = 4}
    & Termbox.image

string :: [Char] -> Termbox.Image
string chars =
  zip [0 ..] chars & foldMap \(i, char) ->
    Termbox.char char & Termbox.atCol i
