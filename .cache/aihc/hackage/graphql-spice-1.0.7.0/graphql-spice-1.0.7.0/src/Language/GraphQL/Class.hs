{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

-- | ToGraphQL and FromGraphQL typeclasses used for user-defined type
-- conversion.
module Language.GraphQL.Class
    ( FromGraphQL(..)
    , ToGraphQL(..)
    , deriveFromGraphQL
    , deriveToGraphQL
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Text.Read as Text.Read
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Text as Text
import Data.Time
    ( Day
    , DiffTime
    , LocalTime(..)
    , NominalDiffTime
    , TimeOfDay(..)
    , UTCTime(..)
    , showGregorian
    , secondsToNominalDiffTime
    , secondsToDiffTime
    )
import Data.Time.Format.ISO8601
    ( ISO8601(..)
    , formatParseM
    , iso8601Format
    , iso8601Show
    )
import Language.Haskell.TH
    ( Con(..)
    , Dec(..)
    , Exp(..)
    , Info(..)
    , Quote(..)
    , Name
    , Q
    , VarBangType
    , appT
    , conP
    , conT
    , instanceD
    , recP
    , reify
    , nameBase
    , listE
    , stringL
    , tupE
    , litE
    , varE
    , varP
    , funD
    , clause
    , normalB
    , appE
    , mkName
    , conE
    , integerL
    , litP
    , wildP
    )
import Data.Foldable (Foldable(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Language.GraphQL.Type as Type
import Prelude hiding (id)

fromGraphQLToIntegral :: Integral a => Type.Value -> Maybe a
fromGraphQLToIntegral (Type.Int value) = Just $ fromIntegral value
fromGraphQLToIntegral (Type.String value) =
    case Text.Read.decimal value of
        Right (converted, "") -> Just converted
        _conversionError -> Nothing
fromGraphQLToIntegral _ = Nothing

iso8601ToGraphQL :: ISO8601 t => t -> Type.Value
iso8601ToGraphQL = Type.String . Text.pack . iso8601Show

fromGraphQLToISO8601 :: ISO8601 t => Type.Value -> Maybe t
fromGraphQLToISO8601 (Type.String value') = formatParseM iso8601Format $ Text.unpack value'
fromGraphQLToISO8601 _ = Nothing

-- | Instances of this typeclass can be converted to GraphQL internal
-- representation.
class ToGraphQL a
  where
    toGraphQL :: a -> Type.Value

instance ToGraphQL Type.Value
  where
    toGraphQL a = a

instance ToGraphQL Text
  where
    toGraphQL = Type.String

instance ToGraphQL String
  where
    toGraphQL = Type.String . Text.pack

instance ToGraphQL Int
  where
    toGraphQL = Type.Int . fromIntegral

instance ToGraphQL Int8
  where
    toGraphQL = Type.Int . fromIntegral

instance ToGraphQL Int16
  where
    toGraphQL = Type.Int . fromIntegral

instance ToGraphQL Int32
  where
    toGraphQL = Type.Int

instance ToGraphQL Int64
  where
    toGraphQL = Type.Int . fromIntegral

instance ToGraphQL Word
  where
    toGraphQL = Type.Int . fromIntegral

instance ToGraphQL Word8
  where
    toGraphQL = Type.Int . fromIntegral

instance ToGraphQL Word16
  where
    toGraphQL = Type.Int . fromIntegral

instance ToGraphQL Word32
  where
    toGraphQL = Type.Int . fromIntegral

instance ToGraphQL Word64
  where
    toGraphQL = Type.Int . fromIntegral

instance ToGraphQL a => ToGraphQL [a]
  where
    toGraphQL = Type.List . fmap toGraphQL

instance ToGraphQL a => ToGraphQL (Vector a)
  where
    toGraphQL = Type.List . toList . fmap toGraphQL

instance ToGraphQL a => ToGraphQL (Maybe a)
  where
    toGraphQL (Just justValue) = toGraphQL justValue
    toGraphQL Nothing = Type.Null

instance ToGraphQL Bool
  where
    toGraphQL = Type.Boolean

instance ToGraphQL Float
  where
    toGraphQL = Type.Float . realToFrac

instance ToGraphQL Double
  where
    toGraphQL = Type.Float

instance ToGraphQL Scientific
  where
    toGraphQL = Type.Float . toRealFloat

instance ToGraphQL Day
  where
    toGraphQL = Type.String . Text.pack . showGregorian

instance ToGraphQL DiffTime
  where
    toGraphQL = Type.Int . truncate . (realToFrac :: DiffTime -> Double)

instance ToGraphQL NominalDiffTime
  where
    toGraphQL = Type.Int . truncate . (realToFrac :: NominalDiffTime -> Double)

instance ToGraphQL UTCTime
  where
    toGraphQL = iso8601ToGraphQL

instance ToGraphQL TimeOfDay
  where
    toGraphQL = iso8601ToGraphQL

instance ToGraphQL LocalTime
  where
    toGraphQL = iso8601ToGraphQL

instance ToGraphQL a => ToGraphQL (HashMap.HashMap Text a)
  where
    toGraphQL = Type.Object . fmap toGraphQL

-- | Instances of this typeclass can be used to convert GraphQL internal
-- representation to user-defined type.
class FromGraphQL a
  where
    fromGraphQL :: Type.Value -> Maybe a

instance FromGraphQL Type.Value
  where
    fromGraphQL = Just

instance FromGraphQL Text
  where
    fromGraphQL (Type.String value) = Just value
    fromGraphQL _ = Nothing

instance FromGraphQL String
  where
    fromGraphQL (Type.String value) = Just $ Text.unpack value
    fromGraphQL _ = Nothing

instance FromGraphQL Int
  where
    fromGraphQL = fromGraphQLToIntegral

instance FromGraphQL Int8
  where
    fromGraphQL = fromGraphQLToIntegral

instance FromGraphQL Int16
  where
    fromGraphQL = fromGraphQLToIntegral

instance FromGraphQL Int32
  where
    fromGraphQL = fromGraphQLToIntegral

instance FromGraphQL Int64
  where
    fromGraphQL = fromGraphQLToIntegral

instance FromGraphQL Word
  where
    fromGraphQL = fromGraphQLToIntegral

instance FromGraphQL Word8
  where
    fromGraphQL = fromGraphQLToIntegral

instance FromGraphQL Word16
  where
    fromGraphQL = fromGraphQLToIntegral

instance FromGraphQL Word32
  where
    fromGraphQL = fromGraphQLToIntegral

instance FromGraphQL Word64
  where
    fromGraphQL = fromGraphQLToIntegral

instance FromGraphQL a => FromGraphQL [a]
  where
    fromGraphQL (Type.List value) = traverse fromGraphQL value
    fromGraphQL _ = Nothing

instance FromGraphQL a => FromGraphQL (Vector a)
  where
    fromGraphQL (Type.List value) = Vector.fromList
        <$> traverse fromGraphQL value
    fromGraphQL _ = Nothing

instance FromGraphQL a => FromGraphQL (Maybe a)
  where
    fromGraphQL Type.Null = Just Nothing
    fromGraphQL value = Just <$> fromGraphQL value

instance FromGraphQL Bool
  where
    fromGraphQL (Type.Boolean value) = Just value
    fromGraphQL _ = Nothing

instance FromGraphQL Float
  where
    fromGraphQL (Type.Float value) = Just $ realToFrac value
    fromGraphQL _ = Nothing

instance FromGraphQL Double
  where
    fromGraphQL (Type.Float value) = Just value
    fromGraphQL _ = Nothing

instance FromGraphQL Scientific
  where
    fromGraphQL (Type.Float value) = Just $ realToFrac value
    fromGraphQL _ = Nothing

instance FromGraphQL Day
  where
    fromGraphQL = fromGraphQLToISO8601

instance FromGraphQL DiffTime
  where
    fromGraphQL (Type.Int value') = Just $ secondsToDiffTime $ fromIntegral value'
    fromGraphQL _ = Nothing

instance FromGraphQL NominalDiffTime
  where
    fromGraphQL (Type.Int value') = Just $ secondsToNominalDiffTime $ fromIntegral value'
    fromGraphQL _ = Nothing

instance FromGraphQL UTCTime
  where
    fromGraphQL = fromGraphQLToISO8601

instance FromGraphQL TimeOfDay
  where
    fromGraphQL = fromGraphQLToISO8601

instance FromGraphQL LocalTime
  where
    fromGraphQL = fromGraphQLToISO8601

instance FromGraphQL a => FromGraphQL (HashMap.HashMap Text a)
  where
    fromGraphQL (Type.Object hm) = traverse fromGraphQL hm
    fromGraphQL _ = Nothing

stringLE :: Name -> Q Exp
stringLE = litE . stringL . nameBase

-- | Given a type derives a 'FromGraphQL' instance for it.
--
-- The derivation can only work when all nested types already have 'FromGraphQL'
-- instances.
--
-- The following cases are supported:
--
-- * Records encode input objects.
-- * Sum types with all data constructors without parameters encode Enums.
deriveFromGraphQL :: Name -> Q [Dec]
deriveFromGraphQL typeName = do
    TyConI plainConstructor <- reify typeName
    case plainConstructor of
        DataD _ _ _ _ [cons'] _
            | RecC dataConName varBangTypes <- cons' ->
                withRecordConstructor dataConName varBangTypes
        DataD _ _ _ _ cons' _ -> pure <$> generateEnumInstance cons'
        NewtypeD _ _ _ _ cons' _
            | RecC dataConName varBangTypes <- cons' ->
                withRecordConstructor dataConName varBangTypes
        _ -> error "Only input objects and enums are supported if all member types have a FromGraphQL instance"
  where
    enumMemberPattern (NormalC normalName []) =
        let fromGraphQLF = conP (mkName "Type.Enum") [litP $ stringL $ nameBase normalName]
         in flip (clause [fromGraphQLF]) []
            $ normalB [|Just $(conE normalName)|]
    enumMemberPattern _ =
        error "Enum member should be a normal constructor without parameters"
    generateEnumInstance :: [Con] -> Q Dec
    generateEnumInstance cons'
        = instanceD mempty (appT (conT ''FromGraphQL) conTName)
        $ pure $ funD 'fromGraphQL
        $ (enumMemberPattern <$> cons')
        <> [clause [wildP] (normalB [|Nothing|]) []]
    hashMapLookup fieldName objectName =
        [|HashMap.lookup $(stringLE fieldName) $objectName >>= fromGraphQL|]
    addRecordField objectName accumulator (name', _, _)
        = appE (appE (varE $ mkName "<*>") accumulator)
        $ hashMapLookup name' objectName
    withRecordConstructor dataConName varBangTypes = do
        valueName <- newName "value"
        let objectName = varE valueName
            toGraphQLF = conP (mkName "Type.Object") [varP valueName]
            fBody = makeRecordBody (conE dataConName) objectName varBangTypes
            recordSize = litE $ integerL $ fromIntegral $ length varBangTypes
        [d|
            instance FromGraphQL $conTName
              where
                fromGraphQL $toGraphQLF
                    | HashMap.size $objectName == $recordSize = $fBody
                    | otherwise = Nothing
                fromGraphQL _ = Nothing
         |]
    makeRecordBody dataConE objectName ((headName, _, _) : varBangTypes') =
        let initialExpression = appE (appE (varE $ mkName "<$>") dataConE)
                $ hashMapLookup headName objectName
         in foldl' (addRecordField objectName) initialExpression varBangTypes'
    makeRecordBody dataConE _ [] = dataConE
    conTName = conT typeName

-- | Given a type derives a 'ToGraphQL' instance for it.
--
-- The derivation can only work when all nested types already have 'ToGraphQL'
-- instances.
--
-- The following cases are supported:
--
-- * Records are decoded as objects.
-- * Sum types with all data constructors without parameters are decoded as Enums.
-- * Sum types whose data constructors have exactly one parameter are decoded as Unions.
deriveToGraphQL :: Name -> Q [Dec]
deriveToGraphQL typeName = do
    TyConI plainConstructor <- reify typeName
    case plainConstructor of
        DataD _ _ _ _ [cons'] _
            | RecC dataConName varBangTypes <- cons' ->
                withRecordConstructor dataConName varBangTypes
        DataD _ _ _ _ cons' _ -> fmap pure
            $ instanceD mempty (appT (conT ''ToGraphQL) conTName)
            $ pure $ funD 'toGraphQL
            $ generateSumTypeInstance cons'
        NewtypeD _ _ _ _ cons' _
            | RecC dataConName varBangTypes <- cons' ->
                withRecordConstructor dataConName varBangTypes
        _ -> error "Only objects, unions and enums are supported if all member types have a ToGraphQL instance"
  where
    conTName = conT typeName
    collectEnumMemberNames (NormalC normalName []) = Just normalName
    collectEnumMemberNames _ = Nothing
    collectUnionMembers (NormalC normalName [_]) = Just normalName
    collectUnionMembers _ = Nothing
    enumMemberPattern normalName
        = flip (clause [conP normalName mempty]) []
        $ normalB [|Type.Enum $(stringLE normalName)|]
    unionMemberPattern normalName = do
        dataName <- newName "member"
        flip (clause [conP normalName [varP dataName]]) []
            $ normalB
            $ appE (varE $ mkName "toGraphQL")
            $ varE dataName
    generateSumTypeInstance cons'
        | Just enumMemberNames <- traverse collectEnumMemberNames cons' =
            enumMemberPattern <$> enumMemberNames
        | Just unionMembers <- traverse collectUnionMembers cons' =
            unionMemberPattern <$> unionMembers
        | otherwise =  error "All data constructors should have either no parameters (Enum) or one parameter (Union)"
    withRecordConstructor dataConName varBangTypes = do
        fieldAliases <- traverse newFieldAliases varBangTypes
        let fBody =
                [| Type.Object
                    $ HashMap.insert "__typename" $(stringLE typeName)
                    $ HashMap.fromList $(listE $ resultObjectPairs <$> fieldAliases)
                |]
            toGraphQLF = recP dataConName (newFieldPatterns <$> fieldAliases)
        [d|
            instance ToGraphQL $conTName
              where
                toGraphQL $toGraphQLF = $fBody
         |]
    newFieldAliases :: VarBangType -> Q (Name, Name)
    newFieldAliases (name', _, _) = (name',) <$> newName (nameBase name')
    newFieldPatterns (name', alias) = (name',) <$> varP alias
    resultObjectPairs :: (Name, Name) -> Q Exp
    resultObjectPairs (name', alias) = tupE
        [ litE (stringL $ nameBase name')
        , [|toGraphQL $(varE alias)|]
        ]
