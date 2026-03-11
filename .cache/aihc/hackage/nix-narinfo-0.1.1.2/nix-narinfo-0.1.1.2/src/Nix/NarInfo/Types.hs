{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}


module Nix.NarInfo.Types
    ( -- * Types
      NarInfo(..)
    , SimpleNarInfo
    ) where

import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics

-- NarInfo URL includes storePath hash and .narinfo suffix
-- Note: storePath is with prefix but references are shortRefs (without /nix/store prefix)
--
-- Both `parseNarInfoWith` and `buildNarInfoWith` need
-- a path parser/printer which takes an argument
-- whether the path is prefixed or not.
--
data NarInfo fp txt hash = NarInfo
  { -- | Absolute path of the derivation in nix store.
    storePath   :: fp
  , -- | Relative url (to current domain) to download nar file.
    url         :: txt
  , -- | Name of the compression algorithm, eg. xz.
    compression :: txt
  , -- | Hash of the compressed nar file.
    -- NOTE: to compute use "nix-hash --type sha256 --flat"
    -- (srk) this isn't fixed to sha256 but a prefix indicates the type e.g.: sha256:1a6lzf...
    -- default is sha256 thought
    fileHash    :: hash
  , -- | File size of compressed nar file.
    -- NOTE: du -b
    fileSize    :: Integer
  , -- | Hash of the decompressed nar file.
    -- NOTE: to compute use "nix-hash --type sha256 --flat --base32"
    -- (srk) this isn't fixed to sha256 but a prefix indicates the type e.g.: sha256:1a6lzf...
    -- default is sha256 thought
    narHash     :: hash
  , -- | File size of decompressed nar file.
    -- NOTE: du -b
    narSize     :: Integer
  , -- | Immediate dependencies of the storePath.
    -- NOTE: nix-store -q --references
    references  :: Set fp
  , -- | Relative store path (to nix store root) of the deriver.
    -- NOTE: nix-store -q --deriver
    deriver     :: Maybe txt
  , -- | System
    system      :: Maybe txt
  , -- | Signature of fields: storePath, narHash, narSize, refs.
    sig         :: Maybe txt
  , -- | Content-addressed
    -- Store path is computed from a cryptographic hash
    -- of the contents of the path, plus some other bits of data like
    -- the "name" part of the path.
    ca          :: Maybe txt
  }
  deriving (Eq, Generic, Show)

type SimpleNarInfo = NarInfo FilePath Text Text
