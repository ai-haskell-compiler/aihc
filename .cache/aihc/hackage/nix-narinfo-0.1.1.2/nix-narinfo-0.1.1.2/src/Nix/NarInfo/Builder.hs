{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.NarInfo.Builder
    ( -- * Builder
      buildNarInfo
    , buildNarInfoWith
    ) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Nix.NarInfo.Types

import qualified Control.Applicative
import qualified Data.Char
import qualified Data.Set
import qualified Data.List
import qualified Data.Text
import qualified Data.Text.Lazy.Builder

buildNarInfo :: (NarInfo FilePath Text Text) -> Builder
buildNarInfo = buildNarInfoWith
  (\_needPrefix fp -> Data.Text.Lazy.Builder.fromString fp)
  Data.Text.Lazy.Builder.fromText
  Data.Text.Lazy.Builder.fromText

buildNarInfoWith :: (Ord fp)
                 => (Bool -> fp -> Builder)
                 -> (txt -> Builder)
                 -> (hash -> Builder)
                 -> NarInfo fp txt hash
                 -> Builder
buildNarInfoWith filepath string hash (NarInfo{..}) =
     keyPath "StorePath"   storePath
  <> key     "URL"         url
  <> key     "Compression" compression
  <> keyHash "FileHash"    fileHash
  <> keyNum  "FileSize"    fileSize
  <> keyHash "NarHash"     narHash
  <> keyNum  "NarSize"     narSize

  <> key'    "References"  (mconcat
      $ Data.List.intersperse " "
        $ map (filepath False)
          $ Data.List.sort $ Data.Set.toList references)

  <> optKey  "Deriver"     deriver
  <> optKey  "System"      system
  <> optKey  "Sig"         sig
  <> optKey  "Ca"          ca
  where
    key' k v    = k <> ": " <> v <> "\n"
    key k v     = key' k (string v)
    keyNum k v  = key' k (Data.Text.Lazy.Builder.fromString . show $ v)
    keyPath k v = key' k (filepath True v)
    keyHash k v = key' k (hash v)
    optKey k    = maybe mempty (key k)
