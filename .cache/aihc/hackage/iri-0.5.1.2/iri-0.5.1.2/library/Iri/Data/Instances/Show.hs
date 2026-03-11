{-# OPTIONS_GHC -Wno-orphans #-}

module Iri.Data.Instances.Show where

import qualified Data.Text as B
import Iri.Data.Types
import Iri.Prelude
import qualified Iri.Rendering.Text.Internal as A

instance Show Iri where
  show =
    mappend "\"" . flip mappend "\"" . B.unpack . A.iri

instance Show HttpIri where
  show =
    mappend "\"" . flip mappend "\"" . B.unpack . A.httpIri

deriving instance Show Scheme

deriving instance Show Hierarchy

deriving instance Show Authority

deriving instance Show UserInfo

deriving instance Show User

deriving instance Show Password

deriving instance Show Host

deriving instance Show RegName

deriving instance Show DomainLabel

deriving instance Show Port

deriving instance Show Path

deriving instance Show PathSegment

deriving instance Show Query

deriving instance Show Fragment

deriving instance Show Security
