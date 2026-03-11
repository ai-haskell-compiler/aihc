module ParkBench.Internal.Terminal
  ( withTerminal,
    Terminal,
    renderToTerminal,
    blue,
    green,
    red,
  )
where

import Control.Exception (bracket_)
import qualified Data.ByteString as ByteString
import Data.IORef
import qualified Data.Text.Encoding as Text (encodeUtf8)
import ParkBench.Internal.Builder (Builder)
import qualified ParkBench.Internal.Builder as Builder
import ParkBench.Internal.Prelude
import System.IO (hSetEcho, stdin)

withTerminal :: (Terminal -> IO a) -> IO a
withTerminal action = do
  terminal <- newTerminal
  bracket_ hideCursor showCursor do
    bracket_ (hSetEcho stdin False) (hSetEcho stdin True) do
      ByteString.putStr (ByteString.singleton newline)
      action terminal

newtype Terminal
  = Terminal (IORef Int)

newTerminal :: IO Terminal
newTerminal =
  coerce (newIORef (0 :: Int))

renderToTerminal :: Terminal -> Builder -> IO ()
renderToTerminal (Terminal lastLineCountRef) bytes0 = do
  lastLineCount <- readIORef lastLineCountRef
  let bytes = Text.encodeUtf8 (Builder.build (cursorUp lastLineCount <> clearFromCursor <> bytes0))
  ByteString.putStr bytes
  writeIORef lastLineCountRef $! ByteString.count newline bytes

newline :: Word8
newline = 10

hideCursor :: IO ()
hideCursor =
  ByteString.putStr "\ESC[?25l"

showCursor :: IO ()
showCursor =
  ByteString.putStr "\ESC[?25h"

clearFromCursor :: Builder
clearFromCursor =
  "\ESC[0J"

cursorUp :: Int -> Builder
cursorUp n =
  "\ESC[" <> Builder.decimal n <> "F"

blue :: Builder -> Builder
blue s =
  "\ESC[34m" <> s <> "\ESC[39m"

green :: Builder -> Builder
green s =
  "\ESC[32m" <> s <> "\ESC[39m"

red :: Builder -> Builder
red s =
  "\ESC[31m" <> s <> "\ESC[39m"
