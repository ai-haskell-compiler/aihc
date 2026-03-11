-- |A module for working with debian relationships <http://www.debian.org/doc/debian-policy/ch-relationships.html>
module Debian.Relation
    ( -- * Types
      PkgName(..)
    , SrcPkgName(..)
    , BinPkgName(..)
    , Relations
    , AndRelation
    , OrRelation
    , Relation(..)
    , ArchitectureReq(..)
    , Arch(..)
    , ArchOS(..)
    , ArchCPU(..)
    , VersionReq(..)
    -- * Helper Functions
    , checkVersionReq
    -- * Relation Parser
    , RelParser
    , ParseRelations(..)
    ) where

import Debian.Arch (Arch(..), ArchOS(..), ArchCPU(..))
import Debian.Relation.Common (SrcPkgName(..), BinPkgName(..), PkgName(pkgNameFromString))
import Debian.Relation.String
