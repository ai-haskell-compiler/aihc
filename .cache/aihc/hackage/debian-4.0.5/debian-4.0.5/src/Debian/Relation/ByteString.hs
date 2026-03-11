{-# OPTIONS -fno-warn-orphans #-}
-- |A module for working with debian relationships <http://www.debian.org/doc/debian-policy/ch-relationships.html>
module Debian.Relation.ByteString
    ( -- * Types
      AndRelation
    , OrRelation
    , Relations
    , Relation(..)
    , ArchitectureReq(..)
    , VersionReq(..)
    -- * Helper Functions
    , checkVersionReq
    -- * Relation Parser
    , RelParser
    , ParseRelations(..)
    ) where

import qualified Data.ByteString.Char8 as C

-- Local Modules

--import Debian.Relation.Common
import Debian.Relation.String
--import Debian.Version

-- * ParseRelations

-- For now we just wrap the string version
instance ParseRelations C.ByteString where
    parseRelations byteStr = parseRelations (C.unpack byteStr)
