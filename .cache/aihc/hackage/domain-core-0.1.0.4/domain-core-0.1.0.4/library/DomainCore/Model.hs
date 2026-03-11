-- |
-- High-level model of schema.
module DomainCore.Model where

import DomainCore.Prelude

-- |
-- Declaration of a type.
data TypeDec
  = -- |
    --  Name of the type and its definition.
    TypeDec Text TypeDef
  deriving (Generic, Show, Eq, Ord, Lift)

-- |
-- Definition of a type.
data TypeDef
  = -- |
    --  Sum.
    --  A list of pairs of names of its members
    --  (which will be mapped to constructors) and
    --  types which will populate the according constructors.
    SumTypeDef [(Text, [Type])]
  | -- |
    --  Product.
    --  Think of it as a record.
    --  Carries a list of associations of field names with types.
    ProductTypeDef [(Text, Type)]
  deriving (Generic, Show, Eq, Ord, Lift)

-- |
-- Type.
data Type
  = -- |
    --  Fully applied tuple of the listed types.
    TupleType [Type]
  | -- |
    --  List of type applications.
    AppType (NonEmpty Type)
  | -- |
    --  List type with the type of its element.
    ListType Type
  | -- |
    --  Possibly qualified reference to another type.
    RefType Text
  deriving (Generic, Show, Eq, Ord, Lift)
