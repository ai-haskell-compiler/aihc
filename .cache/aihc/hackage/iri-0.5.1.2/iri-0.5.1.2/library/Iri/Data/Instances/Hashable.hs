{-# OPTIONS_GHC -Wno-orphans #-}

module Iri.Data.Instances.Hashable where

import Iri.Data.Instances.Eq ()
import Iri.Data.Instances.Generic ()
import Iri.Data.Types
import Iri.Prelude

instance Hashable Iri

instance Hashable Scheme

instance Hashable Hierarchy

instance Hashable Authority

instance Hashable UserInfo

instance Hashable User

instance Hashable Password

instance Hashable Host

instance Hashable RegName

instance Hashable DomainLabel

instance Hashable Port

instance Hashable Path

instance Hashable PathSegment

instance Hashable Query

instance Hashable Fragment

instance Hashable HttpIri

instance Hashable Security
