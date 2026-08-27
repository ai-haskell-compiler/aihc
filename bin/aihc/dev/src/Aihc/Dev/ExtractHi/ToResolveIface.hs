{-# LANGUAGE OverloadedStrings #-}

-- | Convert a 'PackageInterface' (rich GHC .hi extraction) to the minimal
-- JSON format needed by the aihc-resolve name resolver.
--
-- The resolver only needs to know which names exist in each module and
-- namespace (terms vs types). It does not need type signatures, kinds,
-- or other rich information.
--
-- Output JSON schema:
--
-- @
-- { "package": "base-4.21.0.0"
-- , "modules":
--     [ { "module": "Data.List"
--       , "terms": ["map", "filter", ...]
--       , "types": ["NonEmpty", ...]
--       , "constructors": {"Maybe": ["Just", "Nothing"], ...}
--       , "methods": {"Functor": ["fmap", "<$"], ...}
--       }
--     ]
-- }
-- @
module Aihc.Dev.ExtractHi.ToResolveIface
  ( toResolveIface,
    ResolvePackageIface (..),
    ResolveModuleIface (..),
  )
where

import Aihc.Dev.ExtractHi.Types
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | Minimal package interface for the resolver.
data ResolvePackageIface = ResolvePackageIface
  { rpiPackage :: Text,
    rpiModules :: [ResolveModuleIface]
  }
  deriving (Show)

instance ToJSON ResolvePackageIface where
  toJSON rpi =
    object
      [ "package" .= rpiPackage rpi,
        "modules" .= rpiModules rpi
      ]

-- | Minimal module interface for the resolver.
data ResolveModuleIface = ResolveModuleIface
  { rmiModule :: Text,
    rmiTerms :: [Text],
    rmiTypes :: [Text],
    rmiConstructors :: Map Text [Text],
    rmiMethods :: Map Text [Text]
  }
  deriving (Show)

instance ToJSON ResolveModuleIface where
  toJSON rmi =
    object
      [ "module" .= rmiModule rmi,
        "terms" .= rmiTerms rmi,
        "types" .= rmiTypes rmi,
        "constructors" .= rmiConstructors rmi,
        "methods" .= rmiMethods rmi
      ]

-- | Convert a rich PackageInterface to the minimal resolver format.
toResolveIface :: PackageInterface -> ResolvePackageIface
toResolveIface pi' =
  ResolvePackageIface
    { rpiPackage = piPackage pi',
      rpiModules = map convertModule (piModules pi')
    }

-- | Convert a single module interface.
convertModule :: ModuleInterface -> ResolveModuleIface
convertModule mi =
  ResolveModuleIface
    { rmiModule = miModule mi,
      rmiTerms = plainValues,
      rmiTypes = typeNames,
      rmiConstructors = constructorMap,
      rmiMethods = methodMap
    }
  where
    -- Plain values: functions and variables (not constructors or methods)
    plainValues = map evName (miValues mi)

    -- Type names: data types, type synonyms, type families
    typeNames = map etName (miTypes mi)

    -- Constructors grouped by their parent type
    constructorMap =
      Map.fromList
        [ (etName et, etConstructors et)
        | et <- miTypes mi,
          not (null (etConstructors et))
        ]

    -- Methods grouped by their parent class
    methodMap =
      Map.fromList
        [ (ecName ec, map cmName (ecMethods ec))
        | ec <- miClasses mi,
          not (null (ecMethods ec))
        ]
