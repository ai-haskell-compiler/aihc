module DomainOptics.InstanceDecs where

import qualified DomainCore.Model as Model
import qualified DomainCore.TH as DomainTH
import DomainOptics.Prelude
import qualified DomainOptics.Util.OpticsTH as OpticsTH
import Language.Haskell.TH.Syntax
import THLego.Helpers

labelOptic :: Model.TypeDec -> [Dec]
labelOptic (Model.TypeDec typeName typeDef) =
  case typeDef of
    Model.ProductTypeDef members ->
      zipWith zipper (enumFrom 0) members
      where
        membersLength =
          length members
        zipper fieldIndex (fieldName, fieldType) =
          OpticsTH.fieldLensLabelOpticInstanceDec
            (textTyLit fieldName)
            (textName typeName)
            (DomainTH.typeType fieldType)
            membersLength
            fieldIndex
    Model.SumTypeDef structure ->
      fmap mapper structure
      where
        mapper (memberName, subTypes) =
          OpticsTH.prismLabelOpticInstanceDec
            (textTyLit memberName)
            (textName typeName)
            (DomainTH.sumConstructorName typeName memberName)
            (fmap DomainTH.typeType subTypes)
