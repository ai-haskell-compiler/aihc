-- mark.hs: openpgp-asciiarmor benchmark suite
-- Copyright © 2018-2019 Clint Adams
-- This software is released under the terms of the Expat license.
-- (See the LICENSE file).

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

import Criterion.Main

import Data.Digest.CRC24

main :: IO ()
main = defaultMain [
  bgroup "crc" [
                 bench "crc24"  $ whnf crc24 "test"
               , bench "crc24Lazy"  $ whnf crc24Lazy "test"
               ]
                   ]
