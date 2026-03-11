module DomainCore.TH where

import DomainCore.Model
import DomainCore.Prelude
import qualified DomainCore.Text as Text
import qualified Language.Haskell.TH as TH
import qualified THLego.Helpers as TH

-- |
-- Convert a model type definition into Template Haskell.
typeType ::
  -- | Model type.
  Type ->
  -- | Template Haskell type.
  TH.Type
typeType =
  \case
    AppType a ->
      foldl1 TH.AppT (fmap typeType a)
    RefType a ->
      TH.ConT (TH.textName a)
    ListType a ->
      TH.AppT TH.ListT (typeType a)
    TupleType a ->
      TH.multiAppT (TH.TupleT (length a)) (fmap typeType a)

-- |
-- Assemble a record field name.
recordFieldName ::
  -- | Prepend with underscore.
  Bool ->
  -- | Prefix with type name.
  Bool ->
  -- | Type name.
  Text ->
  -- | Label.
  Text ->
  -- | Template Haskell name.
  TH.Name
recordFieldName underscore prefixWithTypeName a b =
  TH.textName (Text.recordField underscore prefixWithTypeName a b)

-- |
-- Assemble a sum constructor name.
sumConstructorName ::
  -- | Type name.
  Text ->
  -- | Label.
  Text ->
  -- | Template Haskell name.
  TH.Name
sumConstructorName a b =
  TH.textName (Text.sumConstructor a b)
