{-# LANGUAGE CPP #-}

module Common (
  warning,
  noTrailingSlash
  ) where

import qualified Data.Text as T

#if MIN_VERSION_simple_cmd(0,2,0)
import SimpleCmd (warning)
#else
import System.IO (hPutStrLn, stderr)

warning :: String -> IO ()
warning = hPutStrLn stderr
#endif

noTrailingSlash :: T.Text -> T.Text
noTrailingSlash = T.dropWhileEnd (== '/')
