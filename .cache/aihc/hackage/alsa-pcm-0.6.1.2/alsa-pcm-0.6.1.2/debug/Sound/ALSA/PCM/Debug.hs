module Sound.ALSA.PCM.Debug where

import qualified System.IO as IO
import Control.Concurrent (myThreadId, )

put :: String -> IO ()
put s =
   do t <- myThreadId
      IO.hPutStrLn IO.stderr $ "Sound.ALSA.PCM, " ++ show t ++ ": " ++ s
