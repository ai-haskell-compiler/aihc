{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Names and sorts for System FC.
module Aihc.Fc.Name
  ( Sort (..),
    NameClass (..),
    nameClass,
    Origin (..),
    Name (..),
    Vis (..),
    Home,
    derivedLocalText,
    derivedTopName,
    constructorWrapperName,
    isConstructorWrapperName,
  )
where

import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Control.DeepSeq (NFData (..))
import Data.Bits (xor)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import Numeric (showHex)

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

-- | Export visibility.
data Vis
  = Pub
  | Private
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

-- | The text of a local function that a pass makes from another local
-- function, such as a worker (@$w@) or a specialised copy (@$s@). The new
-- text has only the prefix of the last pass. Thus a worker of the
-- specialised copy of @go@ is @$wgo@, and not @$w$sgo@.
derivedLocalText :: Text -> Text -> Text
derivedLocalText prefix text = prefix <> base text
  where
    base current =
      case [rest | derived <- ["$w", "$s"], Just rest <- [T.stripPrefix derived current], isDerivedBase rest] of
        rest : _ -> base rest
        [] -> current
    -- Some names that the compiler makes also start with @$s@ or @$w@.
    -- Such a name is not the result of an earlier pass.
    isDerivedBase rest =
      not (T.null rest)
        && not (any (`T.isPrefixOf` rest) ["crutinee", "ith_dict"])

-- | The module that a pass makes new top-level values in. A pass that
-- runs on a whole program has no home module: each declaration of the
-- program keeps the module it came from.
type Home = Maybe Origin

-- | The name of a top-level value that a pass makes from another value,
-- such as a worker (@$w@), a specialised copy (@$s@) or a constant. The
-- new value is private to the module that the pass runs in.
--
-- A pass can make a new value from a value that another module declares,
-- because the optimizer interface of that module can give its body to the
-- module that uses it. The new value then belongs to the home module, and
-- not to the module of the original. Two modules that each copy the same
-- imported value must not make the same name, because each one defines
-- its own copy in its own object.
--
-- The text of such a name keeps the origin of the original apart, so that
-- two originals of equal text give two names. A pass that has no home
-- module, or that makes a value from a value of its home module, gets the
-- text as it asks for it.
derivedTopName :: Home -> Name -> Text -> Name
derivedTopName home original text =
  case home of
    Just origin
      | origin /= nameOrigin original ->
          original {nameText = text <> foreignOriginInfix (nameOrigin original), nameOrigin = origin}
    _ -> original {nameText = text}

-- | What the text of a derived name holds to keep the origin of a value
-- of another module apart: a digest of the origin. The digest holds
-- letters and digits only, as the rest of a generated name does.
foreignOriginInfix :: Origin -> Text
foreignOriginInfix origin = "$x" <> T.pack (showHex (digest (originText origin)) "")
  where
    originText (OriginTop package moduleName') = packageIdText package <> ":" <> moduleName'
    originText (OriginLocal (Unique unique)) = T.pack (show unique)
    -- FNV-1a, so that the digest of an origin is the same in every build.
    digest :: Text -> Word64
    digest = T.foldl' step 14695981039346656037
    step hash character = (hash `xor` fromIntegral (fromEnum character)) * 1099511628211

-- | The name of the wrapper function @$W@ of a data constructor. The
-- module that declares the constructor defines the wrapper.
constructorWrapperName :: Name -> Name
constructorWrapperName constructor = Name ("$W" <> nameText constructor) SortValue (nameOrigin constructor)

-- | Whether a name is the wrapper function of a data constructor. A
-- source name cannot start with @$@, so only the desugarer makes it.
isConstructorWrapperName :: Name -> Bool
isConstructorWrapperName name = nameSort name == SortValue && "$W" `T.isPrefixOf` nameText name
