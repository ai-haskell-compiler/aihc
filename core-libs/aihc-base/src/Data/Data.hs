{-# LANGUAGE RankNTypes #-}

module Data.Data
  ( Data (..),
    Constr,
    DataType,
    Fixity (..),
    constrFields,
    constrFixity,
    constrIndex,
    dataTypeConstrs,
    dataTypeName,
    mkConstr,
    mkDataType,
    mkNoRepType,
    showConstr,
  )
where

import Data.Typeable (Typeable)
import GHC.Base (String)
import GHC.Int (Int)
import GHC.Num ((+))

-- | Generic operations on a data type.
class (Typeable a) => Data a where
  gfoldl ::
    (forall d b. (Data d) => c (d -> b) -> d -> c b) ->
    (forall g. g -> c g) ->
    a ->
    c a
  gunfold ::
    (forall b r. (Data b) => c (b -> r) -> c r) ->
    (forall r. r -> c r) ->
    Constr ->
    c a
  toConstr :: a -> Constr
  dataTypeOf :: a -> DataType

-- | The fixity of a data constructor.
data Fixity = Prefix | Infix

-- | The description of a data type.
data DataType = DataType String [Constr]

-- | The description of one data constructor.
data Constr = Constr String [String] Fixity Int

-- | Make a data type that lists its constructors.
mkDataType :: String -> [Constr] -> DataType
mkDataType = DataType

-- | Make a data type that has no generic representation.
mkNoRepType :: String -> DataType
mkNoRepType name = DataType name []

-- | Make a constructor description.
-- The standin gives the constructor the index that comes after the
-- constructors that the data type already lists. A no-rep data type lists no
-- constructors, thus its first constructor gets index 1.
mkConstr :: DataType -> String -> [String] -> Fixity -> Constr
mkConstr dataType name fields fixity =
  Constr name fields fixity (countConstrs (dataTypeConstrs dataType) + 1)

countConstrs :: [Constr] -> Int
countConstrs [] = 0
countConstrs (_ : rest) = 1 + countConstrs rest

-- | Give the name of a data type.
dataTypeName :: DataType -> String
dataTypeName (DataType name _) = name

-- | Give the constructors that a data type lists.
dataTypeConstrs :: DataType -> [Constr]
dataTypeConstrs (DataType _ constrs) = constrs

-- | Give the name of a constructor.
showConstr :: Constr -> String
showConstr (Constr name _ _ _) = name

-- | Give the field names of a constructor.
constrFields :: Constr -> [String]
constrFields (Constr _ fields _ _) = fields

-- | Give the fixity of a constructor.
constrFixity :: Constr -> Fixity
constrFixity (Constr _ _ fixity _) = fixity

-- | Give the index of a constructor in its data type.
constrIndex :: Constr -> Int
constrIndex (Constr _ _ _ index) = index
