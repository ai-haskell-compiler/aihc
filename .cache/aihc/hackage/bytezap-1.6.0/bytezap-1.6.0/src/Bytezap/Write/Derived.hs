module Bytezap.Write.Derived where

import Bytezap.Write.Internal
import Bytezap.Poke.Derived qualified as Poke

import Data.ByteString.Short qualified as SBS
import Data.Text.Internal qualified as T
import Data.Char ( ord )

-- | Write a 'SBS.ShortByteString'.
shortByteString :: SBS.ShortByteString -> Write ExactLength s
shortByteString sbs = Write (SBS.length sbs) (Poke.shortByteString sbs)

-- | Write a 'T.Text'.
text :: T.Text -> Write ExactLength s
text t@(T.Text _arr _off len) = Write len (Poke.text t)

-- | Write a 'Char'.
--
-- Adapted from utf8-string.
char :: Char -> Write ExactLength s
char c = Write (go (ord c)) (Poke.char c)
 where
  go oc
   | oc <= 0x7f       = 1
   | oc <= 0x7ff      = 2
   | oc <= 0xffff     = 3
   | otherwise        = 4
