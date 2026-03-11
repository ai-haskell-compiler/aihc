{- |
Primitive verbosity controlled logging.
-}
module Shell.Utility.Log (
   warn, notice, info, debug,
   wrapWords,
   ) where

import Shell.Utility.Verbosity

import qualified System.IO as IO
import Control.Monad (when)

import qualified Data.List as List


{- |
Non fatal condition that may indicate a problem.

Display on 'IO.stderr' at 'normal' verbosity and above.
-}
warn :: Verbosity -> String -> IO ()
warn = atLevel normal $ IO.hPutStr IO.stderr . ("Warning: " ++)

{- |
Useful status message.

Display at 'normal' verbosity and above.

This is for the ordinary helpful status messages that users see.
Just enough information to know that things are working
but not floods of detail.
-}
notice :: Verbosity -> String -> IO ()
notice = atLevel normal putStr

{- |
More detail on the operation of some action.

Display at 'verbose' verbosity and above.
-}
info :: Verbosity -> String -> IO ()
info = atLevel verbose putStr

{- |
Detailed internal debugging information

Display for 'deafening' verbosity.
-}
debug :: Verbosity -> String -> IO ()
debug = atLevel deafening putStr

atLevel ::
   (Monad m, Ord verbosity) =>
   verbosity -> (msg -> m ()) -> verbosity -> msg -> m ()
atLevel minVerbosity act verbosity msg =
   when (verbosity >= minVerbosity) $ act msg


wrapWords :: Int -> [String] -> String
wrapWords width =
   drop 1 . concat . snd .
   List.mapAccumL
      (\pos w ->
         let len = length w
             newPos = pos + 1 + len
         in if newPos <= width
               then (newPos, ' ':w)
               else (len, '\n':w))
      (-1)
