{-|
This Rpm module currently only provides @rpmspec@.
-}

module SimpleCmd.Rpm (
  rpmspec
  ) where

import SimpleCmd (cmdLines)

-- | @rpmspec args mqueryformat specfile@ runs rpmspec query on file with optional args (a newline is appended to any queryformat)
--
-- @since 0.1.1
rpmspec :: [String] -> Maybe String -> FilePath -> IO [String]
rpmspec args mqf spec = do
  let qf = maybe [] (\ q -> ["--queryformat", q ++ "\n"]) mqf
  cmdLines "rpmspec" (["-q"] ++ args ++ qf ++ [spec])
