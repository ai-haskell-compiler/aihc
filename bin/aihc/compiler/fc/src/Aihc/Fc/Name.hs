-- | Names, sorts, and scopes for System FC.
module Aihc.Fc.Name
  ( Sort (..),
    NameClass (..),
    nameClass,
    Origin (..),
    Name (..),
    nameEquals,
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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | The sort of one name in the single namespace.
data Sort
  = SortTypeConstructor
  | SortDataConstructor
  | SortValue
  | SortTypeVariable
  | SortAxiom
  | SortSynonym
  deriving (Eq, Ord, Show, Read)

-- | Equality class for a name. A free @t@ name may match a synonym.
data NameClass
  = NameClassType
  | NameClassValue
  | NameClassAxiom
  | NameClassTypeVar
  deriving (Eq, Ord, Show, Read)

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
  deriving (Eq, Ord, Show, Read)

-- | A name. Equality uses origin, text, and name class.
data Name = Name
  { nameText :: Text,
    nameSort :: Sort,
    nameOrigin :: Origin
  }
  deriving (Show, Read)

instance Eq Name where
  left == right = nameEquals left right

instance Ord Name where
  compare left right =
    compare
      (nameClass (nameSort left), nameText left, nameOrigin left)
      (nameClass (nameSort right), nameText right, nameOrigin right)

nameEquals :: Name -> Name -> Bool
nameEquals left right =
  nameClass (nameSort left) == nameClass (nameSort right)
    && nameText left == nameText right
    && nameOrigin left == nameOrigin right

newtype ScopeTable = ScopeTable (Map Int (PackageId, Text))
  deriving (Eq, Ord, Show, Read)

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
  deriving (Eq, Ord, Show, Read)
