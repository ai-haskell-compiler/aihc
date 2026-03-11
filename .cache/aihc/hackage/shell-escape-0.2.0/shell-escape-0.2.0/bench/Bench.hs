
{-# LANGUAGE OverloadedStrings
  #-}

import Data.ByteString.Char8 (ByteString)

import Criterion.Main

import Data.ByteString.ShellEscape


strings                     ::  [ByteString]
strings =
  [ "echo * $PWD"
  , ""
  , "~/Music/M.I.A. & Diplo - Piracy Funds Terrorism Vol. 1 (2004)"
  , "abcds"
  , "\x00\n\204\DEL"
  , "\x00\n\204\DELecho * $PWD" ]

main = (defaultMain . concat . fmap (`fmap` strings)) [benchBash, benchSh]
 where
  bench' d f s = bench (d ++ show s) (whnf f s)
  benchBash = bench' "bash:" (escape :: ByteString -> Bash)
  benchSh = bench' "sh:" (escape :: ByteString -> Sh)

