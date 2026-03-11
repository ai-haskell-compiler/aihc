{-# OPTIONS -fno-warn-orphans #-}
-- |A module for working with debian relationships <http://www.debian.org/doc/debian-policy/ch-relationships.html>
module Debian.Relation.Text
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

import qualified Data.Text as T

-- Local Modules

--import Debian.Relation.Common
import Debian.Relation.String
--import Debian.Version

-- * ParseRelations

-- For now we just wrap the string version
instance ParseRelations T.Text where
    parseRelations text = parseRelations (T.unpack text)
