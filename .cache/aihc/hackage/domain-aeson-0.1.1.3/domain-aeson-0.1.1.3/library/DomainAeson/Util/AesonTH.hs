-- |
-- TH utils for aeson.
module DomainAeson.Util.AesonTH where

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Key as AeKey
import qualified Data.Aeson.KeyMap as AeKeyMap
import qualified Data.Vector as Vector
import DomainAeson.Prelude
import Language.Haskell.TH.Syntax
import THLego.Helpers
import qualified THLego.Lambdas as Lambdas
import qualified TemplateHaskell.Compat.V0208 as Compat

-- * FromJSON

productFromJsonInstanceDec :: Type -> Name -> [(Text, Bool)] -> Dec
productFromJsonInstanceDec type_ conName fields =
  fromJsonInstanceDec type_
    $ productParseJsonDec conName fields

sumFromJsonInstanceDec :: Type -> [(Text, Name, Int)] -> Dec
sumFromJsonInstanceDec type_ variants =
  fromJsonInstanceDec type_
    $ parseJsonDec
  where
    parseJsonDec =
      FunD 'Ae.parseJSON [clause]
      where
        clause =
          Clause [] (NormalB bodyExp) []
          where
            bodyExp =
              Lambdas.matcher
                [ Match
                    (Compat.conP 'Ae.Object [VarP aName])
                    (NormalB (sumObjectParserE (VarE aName) variants))
                    [],
                  Match
                    (Compat.conP 'Ae.String [VarP aName])
                    (NormalB stringBody)
                    []
                ]
              where
                stringBody =
                  CaseE (VarE aName) matches
                  where
                    matches =
                      foldr step [failure] variants
                      where
                        step (jsonName, conName, members) next =
                          case members of
                            0 ->
                              Match
                                (LitP (StringL (toList jsonName)))
                                (NormalB bodyExp)
                                []
                                : next
                              where
                                bodyExp =
                                  AppE (VarE 'pure) (ConE conName)
                            _ -> next

                        failure =
                          Match WildP (NormalB bodyExp) []
                          where
                            bodyExp =
                              AppE (VarE 'fail) (LitE (StringL "Unexpected enum value"))

-- ** FromJSON Helpers

fromJsonInstanceDec :: Type -> Dec -> Dec
fromJsonInstanceDec type_ fromJsonFunDec =
  InstanceD Nothing [] headType [fromJsonFunDec]
  where
    headType =
      AppT (ConT ''Ae.FromJSON) type_

productParseJsonDec :: Name -> [(Text, Bool)] -> Dec
productParseJsonDec conName fields =
  FunD 'Ae.parseJSON [clause]
  where
    clause =
      Clause [] (NormalB exp) []
      where
        exp =
          multiAppE
            (VarE 'Ae.withObject)
            [ LitE (StringL (nameString conName)),
              productObjectParsingLamE conName fields
            ]

productObjectParsingLamE :: Name -> [(Text, Bool)] -> Exp
productObjectParsingLamE conName fields =
  LamE [VarP aName] (productObjectParserE (VarE aName) conName fields)

productObjectParserE :: Exp -> Name -> [(Text, Bool)] -> Exp
productObjectParserE objectE conName fields =
  applicativeChainE (ConE conName) (fmap fieldE fields)
  where
    fieldE (label, required) =
      InfixE (Just objectE) (VarE opName) (Just (textLitE label))
      where
        opName =
          if required
            then '(Ae..:)
            else '(Ae..:?)

sumObjectParserE :: Exp -> [(Text, Name, Int)] -> Exp
sumObjectParserE objectE variants =
  build variants
  where
    build = \case
      (jsonName, conName, components) : tail ->
        CaseE
          ( AppE
              (AppE (VarE 'AeKeyMap.lookup) (textKeyE jsonName))
              objectE
          )
          [ Match
              (Compat.conP 'Just [VarP (mkName "fieldValue")])
              ( NormalB
                  ( case components of
                      0 ->
                        AppE (VarE 'pure) (ConE conName)
                      1 ->
                        applicativeChainE
                          (ConE conName)
                          [AppE (VarE 'Ae.parseJSON) (VarE (mkName "fieldValue"))]
                      _ -> error "TODO: Handle multi-arity"
                  )
              )
              [],
            Match
              (Compat.conP 'Nothing [])
              (NormalB (build tail))
              []
          ]
      _ ->
        AppE
          (VarE 'fail)
          (LitE (StringL "No expected sum-type tag found"))

-- * ToJSON instance declaration

toJsonInstanceDec :: Type -> Dec -> Dec
toJsonInstanceDec type_ toJsonFunDec =
  InstanceD Nothing [] headType [toJsonFunDec]
  where
    headType =
      AppT (ConT ''Ae.ToJSON) type_

productToJsonInstanceDec :: Type -> Name -> [Text] -> Dec
productToJsonInstanceDec type_ conName members =
  toJsonInstanceDec type_ $ productToJsonFunD conName members

sumToJsonInstanceDec :: Type -> [(Text, Name, Int)] -> Dec
sumToJsonInstanceDec type_ members =
  toJsonInstanceDec type_ $ sumToJsonFunD members

enumToJsonInstanceDec :: Type -> [(Text, Name)] -> Dec
enumToJsonInstanceDec type_ members =
  toJsonInstanceDec type_ $ enumToJsonFunD members

-- ** ToJSON Helpers

productToJsonFunD :: Name -> [Text] -> Dec
productToJsonFunD conName members =
  FunD 'Ae.toJSON [clause]
  where
    varNamesAndJsonNames =
      mapWithAlphabeticName (,) members
    clause =
      Clause [Compat.conP conName memberPats] body []
      where
        memberPats = fmap memberPat varNamesAndJsonNames
          where
            memberPat (varName, _) = VarP varName
        body = NormalB $ AppE (ConE 'Ae.Object) mapE
          where
            mapE =
              AppE (VarE 'AeKeyMap.fromList) (ListE (fmap memberPairE varNamesAndJsonNames))
            memberPairE (varName, jsonName) =
              appliedTupleE [textKeyE jsonName, toJsonE (VarE varName)]

sumToJsonFunD :: [(Text, Name, Int)] -> Dec
sumToJsonFunD members =
  FunD 'Ae.toJSON clauses
  where
    clauses = fmap memberClause members
      where
        memberClause (jsonName, conName, components) =
          case components of
            0 ->
              Clause [Compat.conP conName []] (NormalB bodyExp) []
              where
                bodyExp = stringJsonE jsonName
            1 ->
              Clause [Compat.conP conName [VarP varName]] (NormalB bodyExp) []
              where
                varName = aName
                bodyExp =
                  AppE
                    (ConE 'Ae.Object)
                    ( multiAppE
                        (VarE 'AeKeyMap.singleton)
                        [ textKeyE jsonName,
                          toJsonE (VarE varName)
                        ]
                    )
            _ ->
              Clause [Compat.conP conName (fmap VarP varNames)] (NormalB bodyExp) []
              where
                varNames = enumAlphabeticNames components
                bodyExp =
                  AppE
                    (ConE 'Ae.Object)
                    ( multiAppE
                        (VarE 'AeKeyMap.singleton)
                        [ textKeyE jsonName,
                          jsonArrayE (fmap (toJsonE . VarE) varNames)
                        ]
                    )

enumToJsonFunD :: [(Text, Name)] -> Dec
enumToJsonFunD members =
  FunD 'Ae.toJSON clauses
  where
    clauses = fmap memberClause members
      where
        memberClause (jsonName, conName) =
          Clause [Compat.conP conName []] (NormalB bodyExp) []
          where
            bodyExp = stringJsonE jsonName

-- * Helpers

jsonArrayE :: [Exp] -> Exp
jsonArrayE exps =
  AppE (ConE 'Ae.Array) (AppE (VarE 'Vector.fromList) (ListE exps))

stringJsonE :: Text -> Exp
stringJsonE =
  AppE (ConE 'Ae.String) . AppE (VarE 'fromString) . textLitE

textKeyE :: Text -> Exp
textKeyE text =
  AppE (VarE 'AeKey.fromString) (textLitE text)

toJsonE :: Exp -> Exp
toJsonE =
  AppE (VarE 'Ae.toJSON)
