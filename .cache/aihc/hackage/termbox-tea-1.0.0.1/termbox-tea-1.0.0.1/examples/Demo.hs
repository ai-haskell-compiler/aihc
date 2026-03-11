{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever, guard)
import Data.Function ((&))
import GHC.Clock (getMonotonicTime)
import Ki qualified
import Termbox.Tea qualified as Termbox
import Text.Printf (printf)

main :: IO ()
main = do
  t0 <- getMonotonicTime
  result <-
    Ki.scoped \scope -> do
      timeVar <- newEmptyMVar
      Ki.fork_ scope do
        forever do
          threadDelay 100_000
          getMonotonicTime >>= putMVar timeVar
      Termbox.run
        Termbox.Program
          { initialize,
            pollEvent = pollEvent t0 timeVar,
            handleEvent,
            render,
            finished
          }
  putStrLn case result of
    Left err -> "Termbox program failed to initialize: " ++ show err
    Right state -> "Final state: " ++ show state

data State = State
  { elapsed :: !Double,
    terminalSize :: !Termbox.Size,
    lastKey :: !(Maybe Termbox.Key),
    lastMouse :: !(Maybe Termbox.Mouse),
    cursor :: !(Maybe (Termbox.Pos, Double)),
    bright :: !Bool
  }
  deriving stock (Show)

initialize :: Termbox.Size -> State
initialize terminalSize =
  State
    { elapsed = 0,
      terminalSize,
      lastKey = Nothing,
      lastMouse = Nothing,
      cursor = Nothing,
      bright = False
    }

pollEvent :: Double -> MVar Double -> Maybe (IO Double)
pollEvent t0 timeVar =
  Just do
    t1 <- takeMVar timeVar
    pure (t1 - t0)

handleEvent :: (Applicative m) => State -> Termbox.Event Double -> m State
handleEvent state = \case
  Termbox.EventKey key ->
    pure
      state
        { lastKey = Just key,
          bright =
            case key of
              Termbox.KeyChar '*' -> not state.bright
              _ -> state.bright
        }
  Termbox.EventMouse mouse ->
    pure
      state
        { lastMouse = Just mouse,
          cursor =
            case mouse.button of
              Termbox.ReleaseClick -> Just (mouse.pos, state.elapsed + 1)
              _ -> state.cursor
        }
  Termbox.EventResize terminalSize -> pure state {terminalSize}
  Termbox.EventUser elapsed ->
    pure
      state
        { elapsed,
          cursor = do
            (pos, erase) <- state.cursor
            guard (erase > elapsed)
            Just (pos, erase)
        }

render :: State -> Termbox.Scene
render state =
  vcat
    [ string "Welcome to Termbox. Try typing, clicking, and resizing the terminal!",
      string " ",
      hcat
        [ string "Elapsed time: ",
          string (printf "%.1fs" state.elapsed)
        ],
      hcat [string "Terminal size: ", string (show state.terminalSize)],
      hcat
        [ string "Latest key press: ",
          case state.lastKey of
            Nothing -> emptyBox
            Just event -> string (show event) & boldBox
        ],
      hcat
        [ string "Latest mouse click: ",
          case state.lastMouse of
            Nothing -> emptyBox
            Just event -> string (show event) & boldBox
        ],
      string " ",
      hcat
        [ let selected = bgBox (Termbox.gray 20) . fgBox (Termbox.gray 0)
           in vcat
                if state.bright
                  then [string " ", string "normal", selected (string "bright")]
                  else [string " ", selected (string "normal"), string "bright"],
          string " ",
          vcat
            [ string "default red green yellow blue magenta cyan white",
              hcat
                [ rect Termbox.Size {width = 7, height = 2} (brighten Termbox.defaultColor),
                  string " ",
                  rect Termbox.Size {width = 3, height = 2} (brighten Termbox.red),
                  string " ",
                  rect Termbox.Size {width = 5, height = 2} (brighten Termbox.green),
                  string " ",
                  rect Termbox.Size {width = 6, height = 2} (brighten Termbox.yellow),
                  string " ",
                  rect Termbox.Size {width = 4, height = 2} (brighten Termbox.blue),
                  string " ",
                  rect Termbox.Size {width = 7, height = 2} (brighten Termbox.magenta),
                  string " ",
                  rect Termbox.Size {width = 4, height = 2} (brighten Termbox.cyan),
                  string " ",
                  rect Termbox.Size {width = 5, height = 2} (brighten Termbox.white),
                  string " ",
                  hcat [string "Press ", boldBox (string "*"), string " to toggle brightness."]
                ]
            ]
        ],
      string " ",
      string "color 0 .. color 215",
      vcat $
        chunksOf 24 [0 .. 215] & map \is ->
          hcat $
            is & map \i ->
              acat
                [ rect Termbox.Size {width = 4, height = 2} (Termbox.color i),
                  string (show i) & bgBox (Termbox.color i)
                ],
      string " ",
      string "gray 0 .. gray 23",
      hcat $
        [0 .. 23] & map \i ->
          acat
            [ rect (Termbox.Size {width = 4, height = 2}) (Termbox.gray i),
              string (show i) & bgBox (Termbox.gray i)
            ],
      string " ",
      hcat
        [ string "This text is bold." & boldBox,
          string " ",
          string "This text is underlined." & underlineBox,
          string " ",
          string "This text is blinking (maybe)." & blinkBox
        ],
      string " ",
      hcat
        [ string "Press ",
          string "Esc" & boldBox,
          string " to quit!"
        ]
    ]
    & boxImage
    & Termbox.at Termbox.Pos {row = 1, col = 2}
    & Termbox.image
    & case state.cursor of
      Nothing -> id
      Just (pos, _) -> Termbox.cursor pos
    & Termbox.fill (Termbox.gray 4)
  where
    brighten :: Termbox.Color -> Termbox.Color
    brighten =
      if state.bright
        then Termbox.bright
        else id

finished :: State -> Bool
finished State {lastKey} =
  case lastKey of
    Just Termbox.KeyEsc -> True
    _ -> False

data Box
  = Box !Termbox.Size !Termbox.Image

emptyBox :: Box
emptyBox =
  Box (Termbox.Size 0 0) mempty

boxImage :: Box -> Termbox.Image
boxImage (Box _size image) =
  image

fgBox :: Termbox.Color -> Box -> Box
fgBox color (Box size image) =
  Box size (Termbox.fg color image)

bgBox :: Termbox.Color -> Box -> Box
bgBox color (Box size image) =
  Box size (Termbox.bg color image)

boldBox :: Box -> Box
boldBox (Box size image) =
  Box size (Termbox.bold image)

underlineBox :: Box -> Box
underlineBox (Box size image) =
  Box size (Termbox.underline image)

blinkBox :: Box -> Box
blinkBox (Box size image) =
  Box size (Termbox.blink image)

one :: Char -> Box
one char =
  Box (Termbox.Size 1 1) (Termbox.char char)

acat :: [Box] -> Box
acat =
  foldr f emptyBox
  where
    f :: Box -> Box -> Box
    f (Box (Termbox.Size w1 h1) i1) (Box (Termbox.Size w2 h2) i2) =
      Box (Termbox.Size (max w1 w2) (max h1 h2)) (i1 <> i2)

hcat :: [Box] -> Box
hcat =
  foldr f emptyBox
  where
    f :: Box -> Box -> Box
    f (Box (Termbox.Size w1 h1) i1) (Box (Termbox.Size w2 h2) i2) =
      Box (Termbox.Size (w1 + w2) (max h1 h2)) (i1 <> Termbox.atCol w1 i2)

vcat :: [Box] -> Box
vcat =
  foldr f emptyBox
  where
    f :: Box -> Box -> Box
    f (Box (Termbox.Size w1 h1) i1) (Box (Termbox.Size w2 h2) i2) =
      Box (Termbox.Size (max w1 w2) (h1 + h2)) (i1 <> Termbox.atRow h1 i2)

string :: [Char] -> Box
string =
  hcat . map one

rect :: Termbox.Size -> Termbox.Color -> Box
rect Termbox.Size {width, height} color =
  bgBox color (vcat (replicate height (string (replicate width ' '))))

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = \case
  [] -> []
  xs ->
    let (ys, zs) = splitAt n xs
     in ys : chunksOf n zs
