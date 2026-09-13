{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Names, sorts, and scopes for System FC.
module Aihc.Fc.Name
  ( Sort (..),
    NameClass (..),
    nameClass,
    Origin (..),
    Name (..),
    ScopeTable (..),
    emptyScopeTable,
    lookupScope,
    insertScope,
    scopeEntries,
    localUnique,
    Vis (..),
  )
where

import Aihc.Resolve (PackageId)
import Aihc.Tc.Types (Unique (..))
import Control.DeepSeq (NFData (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- | The sort of one name in the single namespace.
data Sort
  = SortTypeConstructor
  | SortDataConstructor
  | SortValue
  | SortTypeVariable
  | SortAxiom
  | SortSynonym
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

-- | Equality class for a name. A free @t@ name may match a synonym.
data NameClass
  = NameClassType
  | NameClassValue
  | NameClassAxiom
  | NameClassTypeVar
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

nameClass :: Sort -> NameClass
nameClass sort =
  case sort of
    SortTypeConstructor -> NameClassType
    SortSynonym -> NameClassType
    SortValue -> NameClassValue
    SortDataConstructor -> NameClassValue
    SortAxiom -> NameClassAxiom
    SortTypeVariable -> NameClassTypeVar

-- | Where a name is bound.
data Origin
  = OriginLocal Unique
  | OriginTop PackageId Text
  deriving stock (Eq, Ord, Show, Read, Generic)

-- A unique is an 'Int' and a package identifier is a strict 'Text', so
-- weak head normal form is normal form for both.
instance NFData Origin where
  rnf origin =
    case origin of
      OriginLocal unique -> unique `seq` ()
      OriginTop package name -> package `seq` rnf name

-- | A name. Equality is structural: two names are equal only when their
-- text, sort, and origin all agree, so a type constructor never equals a
-- synonym and a value never equals a data constructor of the same name.
data Name = Name
  { nameText :: Text,
    nameSort :: Sort,
    nameOrigin :: Origin
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

newtype ScopeTable = ScopeTable (Map Int (PackageId, Text))
  deriving stock (Eq, Ord, Show, Read, Generic)

instance NFData ScopeTable where
  rnf (ScopeTable table) = rnf [package `seq` rnf name | (package, name) <- Map.elems table]

emptyScopeTable :: ScopeTable
emptyScopeTable = ScopeTable Map.empty

lookupScope :: Int -> ScopeTable -> Maybe (PackageId, Text)
lookupScope scopeId (ScopeTable table) = Map.lookup scopeId table

insertScope :: Int -> PackageId -> Text -> ScopeTable -> ScopeTable
insertScope scopeId package moduleName (ScopeTable table) =
  ScopeTable (Map.insert scopeId (package, moduleName) table)

scopeEntries :: ScopeTable -> [(Int, PackageId, Text)]
scopeEntries (ScopeTable table) =
  [(scopeId, package, moduleName) | (scopeId, (package, moduleName)) <- Map.toAscList table]

localUnique :: Name -> Maybe Unique
localUnique name =
  case nameOrigin name of
    OriginLocal unique -> Just unique
    OriginTop {} -> Nothing

-- | Export visibility.
data Vis
  = Pub
  | Private
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)
