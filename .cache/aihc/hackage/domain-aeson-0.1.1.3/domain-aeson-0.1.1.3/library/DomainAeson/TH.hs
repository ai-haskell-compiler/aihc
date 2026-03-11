module DomainAeson.TH where

import DomainAeson.Prelude
import qualified DomainAeson.Util.AesonTH as AesonTH
import qualified DomainCore.Model as Model
import qualified DomainCore.TH as DomainTH
import Language.Haskell.TH.Syntax
import THLego.Helpers

toJsonDec :: Model.TypeDec -> Dec
toJsonDec (Model.TypeDec typeName typeDef) =
  case typeDef of
    Model.ProductTypeDef members ->
      AesonTH.productToJsonInstanceDec
        (ConT (textName typeName))
        (textName typeName)
        (fmap fst members)
    Model.SumTypeDef members ->
      AesonTH.sumToJsonInstanceDec
        (ConT (textName typeName))
        (fmap member members)
      where
        member (memberName, memberComponentTypes) =
          ( memberName,
            DomainTH.sumConstructorName typeName memberName,
            length memberComponentTypes
          )

fromJsonDec :: Model.TypeDec -> Dec
fromJsonDec (Model.TypeDec typeName typeDef) =
  case typeDef of
    Model.ProductTypeDef members ->
      AesonTH.productFromJsonInstanceDec
        (ConT (textName typeName))
        (textName typeName)
        (fmap (second typeIsRequired) members)
    Model.SumTypeDef members ->
      AesonTH.sumFromJsonInstanceDec
        (ConT (textName typeName))
        (fmap member members)
      where
        member (memberName, memberComponentTypes) =
          ( memberName,
            DomainTH.sumConstructorName typeName memberName,
            length memberComponentTypes
          )

typeIsRequired :: Model.Type -> Bool
typeIsRequired = \case
  Model.AppType (Model.RefType ref :| _) ->
    case ref of
      -- FIXME: get a better detection of maybe
      "Maybe" -> False
      _ -> True
  _ -> True
