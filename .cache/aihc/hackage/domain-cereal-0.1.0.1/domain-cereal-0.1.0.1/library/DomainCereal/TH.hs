module DomainCereal.TH where

import qualified Data.Serialize as Cereal
import qualified Data.Serialize.LEB128.Lenient as Leb128
import DomainCereal.Prelude
import qualified DomainCore.Model as Model
import qualified DomainCore.TH as DomainTH
import Language.Haskell.TH.Syntax
import THLego.Helpers
import qualified THLego.Lambdas as Lambdas
import qualified TemplateHaskell.Compat.V0208 as Compat

serializeInstanceD :: Model.TypeDec -> Dec
serializeInstanceD (Model.TypeDec typeName typeDef) =
  InstanceD Nothing [] headType [putFunD, getFunD]
  where
    headType =
      AppT (ConT ''Cereal.Serialize) (ConT (textName typeName))
    (putFunD, getFunD) =
      case typeDef of
        Model.SumTypeDef members ->
          (sumPutFunD preparedMembers, sumGetFunD preparedMembers)
          where
            preparedMembers =
              fmap prepare members
              where
                prepare (memberName, memberComponentTypes) =
                  ( DomainTH.sumConstructorName typeName memberName,
                    length memberComponentTypes
                  )
        Model.ProductTypeDef members ->
          (productPutFunD conName components, productGetFunD conName components)
          where
            conName =
              textName typeName
            components =
              length members

sumPutFunD :: [(Name, Int)] -> Dec
sumPutFunD members =
  FunD 'Cereal.put clauses
  where
    clauses =
      zipWith memberClause members [0 ..]
      where
        memberClause (conName, components) conIdx =
          Clause [Compat.conP conName componentPList] (NormalB body) []
          where
            componentNameList = enumAlphabeticNames components
            componentPList = componentNameList & fmap VarP
            body = mconcatE $ tagE : fmap namePutE componentNameList
              where
                tagE = AppE (VarE 'Leb128.putLEB128) conIdxLitE
                  where
                    conIdxLitE = signedAsWord32E $ LitE $ IntegerL $ fromIntegral conIdx

productPutFunD :: Name -> Int -> Dec
productPutFunD conName components =
  FunD 'Cereal.put [clause]
  where
    clause =
      Clause [Compat.conP conName componentPList] (NormalB body) []
      where
        componentNameList = enumAlphabeticNames components
        componentPList = componentNameList & fmap VarP
        body = nameListPutE componentNameList

sumGetFunD :: [(Name, Int)] -> Dec
sumGetFunD members =
  FunD 'Cereal.get [clause]
  where
    clause =
      Clause [] (NormalB body) []
      where
        body =
          AppE (AppE (VarE '(>>=)) word32GetLEB128E) tagMatchE
          where
            tagMatchE = Lambdas.matcher $ zipWith memberMatch members [0 ..] <> [defaultMatch]
              where
                memberMatch (conName, components) conIdx =
                  Match (LitP (IntegerL conIdx)) (NormalB body) []
                  where
                    body = applicativeChainE (ConE conName) (replicate components (VarE 'Cereal.get))
                defaultMatch =
                  Match WildP (NormalB body) []
                  where
                    body = AppE (VarE 'fail) (LitE (StringL "Unsupported tag"))

productGetFunD :: Name -> Int -> Dec
productGetFunD conName components =
  FunD 'Cereal.get [clause]
  where
    clause =
      Clause [] (NormalB body) []
      where
        body =
          applicativeChainE (ConE conName) (replicate components (VarE 'Cereal.get))

mconcatE :: [Exp] -> Exp
mconcatE = AppE (VarE 'mconcat) . ListE

nameListPutE :: [Name] -> Exp
nameListPutE = mconcatE . fmap namePutE

namePutE :: Name -> Exp
namePutE name = AppE (VarE 'Cereal.put) (VarE name)

signedAsWord32E :: Exp -> Exp
signedAsWord32E exp = SigE exp (ConT ''Word32)

word32GetLEB128E :: Exp
word32GetLEB128E = SigE (VarE 'Leb128.getLEB128) (AppT (ConT ''Cereal.Get) (ConT ''Word32))
