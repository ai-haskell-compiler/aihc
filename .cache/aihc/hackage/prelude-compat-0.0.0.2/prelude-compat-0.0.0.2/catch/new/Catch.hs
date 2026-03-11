module Catch where

import System.IO.Error (catchIOError)

catch :: IO a -> (IOError -> IO a) -> IO a
catch = catchIOError
