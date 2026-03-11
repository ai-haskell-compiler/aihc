{-# OPTIONS_GHC -Wno-type-defaults #-}

module Data.Registry.Aeson.TH.Decoder where

import Control.Monad.Fail
import Data.List (nub)
import Data.Registry.Aeson.TH.ThOptions
import Data.Registry.Aeson.TH.TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude as P hiding (Type)

{-
  This module uses TemplateHaskell to extract enough type information to be able to
  build a Decoder based on configuration options
-}

-- | Make an Encoder for a given data type
--   Usage: $(makeDecoder ''MyDataType <: otherEncoders)
makeDecoder :: Name -> ExpQ
makeDecoder = makeDecoderWith defaultThOptions

-- | Make an Encoder for a given data type, where all types names are qualified with their module full name
--    -- MyDataType is defined in X.Y.Z
--    import X.Y.Z qualified
--    $(makeDecoderQualified ''MyDataType <: otherEncoders)
makeDecoderQualified :: Name -> ExpQ
makeDecoderQualified = makeDecoderWith (ThOptions qualified)

-- | Make an Encoder for a given data type, where all types names are qualified with their module name
--    -- MyDataType is defined in X.Y.Z
--    import X.Y.Z qualified as Z
--    $(makeDecoderQualifiedLast ''MyDataType <: otherEncoders)
makeDecoderQualifiedLast :: Name -> ExpQ
makeDecoderQualifiedLast = makeDecoderWith (ThOptions qualifyWithLastName)

-- | Make a Decoder for a given data type and pass options to specify how names must be qualified
--   Usage: $(makeDecoderWith options ''MyDataType <: otherDecoders)
makeDecoderWith :: ThOptions -> Name -> ExpQ
makeDecoderWith thOptions typeName = appE (varE $ mkName "fun") $ do
  info <- reify typeName
  case info of
    TyConI (NewtypeD _context _name _typeVars _kind constructor _deriving) ->
      makeConstructorsDecoder thOptions typeName [constructor]
    TyConI (DataD _context _name _typeVars _kind constructors _deriving) -> do
      case constructors of
        [] -> do
          qReport True "can not make an Decoder for an empty data type"
          fail "decoders creation failed"
        _ -> makeConstructorsDecoder thOptions typeName constructors
    other -> do
      qReport True ("can only create decoders for an ADT, got: " <> P.show other)
      fail "decoders creation failed"

-- | Make a decoder for a given data type by extracting just enough metadata about the data type in order to be able
--   to parse a Value
--
--   For example for the data type:
--
--   data T = T1 {f1::Int, f2::Int} | T2 Int Int
--
--   we add this function to the registry:
--
--   \opts d1 d2 d3 -> Decoder $ \v ->
--     decodeFromDefinitions opts v $ \case
--       ToConstructor "T1" [v1, v2]-> T1 <$> d1 v1 <*> d2 v2 ...
--       ToConstructor "T2" [v1, v2]-> T2 <$> d1 v1 <*> d3 v2 ...
--       other -> Left ("cannot decode " <> valueToText v)
--
--   The \case function is the only one which needs to be generated in order to match the exact shape of the
--   constructors to instantiate
makeConstructorsDecoder :: ThOptions -> Name -> [Con] -> ExpQ
makeConstructorsDecoder thOptions typeName cs = do
  ts <- nub . join <$> for cs typesOf
  let decoderParameters = sigP (varP (mkName "os")) (conT $ mkName "Options") : sigP (varP (mkName "cd")) (conT $ mkName "ConstructorsDecoder") : ((\(t, n) -> sigP (varP (mkName $ "d" <> P.show n)) (appT (conT $ mkName "Decoder") (pure t))) <$> zip ts [0 ..])
  -- makeToConstructors os [Constructor "T1" ["f1", "f2"], Constructor "T2" []] v
  let paramP = varP (mkName "v")
  constructorDefs <- for cs $ \c -> do
    cName <- makeName thOptions <$> nameOf c
    fields <- fmap (litE . StringL . P.show . makeName thOptions) <$> fieldsOf c
    fieldTypes <- fmap (litE . StringL . P.show . getSimpleTypeName thOptions) <$> typesOf c
    varE (mkName "makeConstructorDef") `appE` (litE . StringL $ P.show cName) `appE` listE fields `appE` listE fieldTypes
  let matchClauses = makeMatchClause thOptions typeName ts <$> cs
  let matchFunction = lamCaseE (matchClauses <> [makeErrorClause typeName])
  let resolveFunction = varE (mkName "decodeFromDefinitions") `appE` varE (mkName "os") `appE` varE (mkName "cd") `appE` listE (pure <$> constructorDefs) `appE` varE (mkName "v") `appE` matchFunction
  lamE decoderParameters (appE (conE (mkName "Decoder")) (lamE [paramP] resolveFunction))

-- | Decode the nth constructor of a data type
--    ToConstructor "T1" [v1, v2]-> T1 <$> d1 v1 <*> d2 v2 ...
makeMatchClause :: ThOptions -> Name -> [Type] -> Con -> MatchQ
makeMatchClause thOptions typeName allTypes c = do
  ts <- typesOf c
  constructorTypes <- fmap (\(_,n,k) -> (n, k)) <$> indexConstructorTypes allTypes ts
  cName <- makeName thOptions <$> nameOf c
  let fieldsP = listP $ (\(n, _) -> varP $ mkName ("v" <> P.show n)) <$> constructorTypes
  match
    (conP (mkName "ToConstructor") [litP (StringL . P.show $ cName), fieldsP])
    (normalB (applyDecoder thOptions typeName cName constructorTypes))
    []

-- | Return an error the json value cannot be decoded with a constructor name and some values
makeErrorClause :: Name -> MatchQ
makeErrorClause typeName = do
  let errorMessage =
        (varE (mkName "<>") `appE` litE (StringL ("cannot use this constructor to create an instance of type '" <> P.show typeName <> "': ")))
          `appE` (varE (mkName "show") `appE` varE (mkName "_1"))
  match (varP $ mkName "_1") (normalB (appE (conE $ mkName "Left") errorMessage)) []

-- ConstructorName <$> decodeFieldValue d1 o1 <*> decodeFieldValue d2 o2 ...
applyDecoder :: ThOptions -> Name -> Name -> [(Int, Int)] -> ExpQ
applyDecoder _thOptions _typeName cName [] = appE (varE $ mkName "pure") (conE cName)
applyDecoder thOptions typeName cName (nk : nks) = do
  let cons = appE (varE $ mkName "pure") (conE cName)
  foldr (\i r -> appE (appE (varE (mkName "ap")) r) $ decodeAt i) (appE (appE (varE (mkName "ap")) cons) $ decodeAt nk) (reverse nks)
  where
    decodeAt (n, k) =
      varE (mkName "decodeFieldValue")
        `appE` varE (mkName ("d" <> P.show k))
        `appE` (litE . StringL . P.show . makeName thOptions $ typeName)
        `appE` (litE . StringL . P.show . makeName thOptions $ cName)
        `appE` varE (mkName ("v" <> P.show n))
