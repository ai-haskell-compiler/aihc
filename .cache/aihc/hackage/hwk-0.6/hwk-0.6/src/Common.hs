module Common (
  setNoBuffering,
  warn
  )
where

import System.IO

warn :: String -> IO ()
warn = hPutStrLn stderr

setNoBuffering :: IO ()
setNoBuffering =
  hSetBuffering stdout NoBuffering
