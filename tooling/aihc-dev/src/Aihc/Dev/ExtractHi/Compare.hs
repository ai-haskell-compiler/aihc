{-# LANGUAGE OverloadedStrings #-}

-- | Subset compatibility checks for interfaces extracted from @.hi@ files.
module Aihc.Dev.ExtractHi.Compare
  ( InterfaceMismatch (..),
    comparePackageSubset,
    renderInterfaceMismatch,
  )
where

import Aihc.Dev.ExtractHi.Types
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

data InterfaceMismatch = InterfaceMismatch
  { mismatchPath :: !Text,
    mismatchMessage :: !Text
  }
  deriving (Eq, Show)

comparePackageSubset :: PackageInterface -> PackageInterface -> [InterfaceMismatch]
comparePackageSubset candidate oracle =
  concatMap compareModule (piModules candidate)
  where
    oracleModules = keyed miModule (piModules oracle)

    compareModule candidateModule =
      case Map.lookup (miModule candidateModule) oracleModules of
        Nothing ->
          [mismatch (miModule candidateModule) "module is not exported by oracle"]
        Just oracleModule ->
          compareValues candidateModule oracleModule
            <> compareTypes candidateModule oracleModule
            <> compareClasses candidateModule oracleModule
            <> compareFixities candidateModule oracleModule

compareValues :: ModuleInterface -> ModuleInterface -> [InterfaceMismatch]
compareValues candidate oracle =
  concatMap compareValue (miValues candidate)
  where
    oracleValues = keyed evName (miValues oracle)
    basePath = miModule candidate <> ".value"

    compareValue value =
      case Map.lookup (evName value) oracleValues of
        Nothing -> [mismatch (basePath <> ":" <> evName value) "value is not exported by oracle"]
        Just oracleValue
          | evType value /= evType oracleValue ->
              [mismatch (basePath <> ":" <> evName value <> ".type") "value type differs from oracle"]
          | otherwise -> []

compareTypes :: ModuleInterface -> ModuleInterface -> [InterfaceMismatch]
compareTypes candidate oracle =
  concatMap compareType (miTypes candidate)
  where
    oracleTypes = keyed etName (miTypes oracle)
    basePath = miModule candidate <> ".type"

    compareType typ =
      case Map.lookup (etName typ) oracleTypes of
        Nothing -> [mismatch (basePath <> ":" <> etName typ) "type is not exported by oracle"]
        Just oracleType ->
          [ mismatch (basePath <> ":" <> etName typ <> ".kind") "type kind differs from oracle"
          | etKind typ /= etKind oracleType
          ]
            <> [ mismatch (basePath <> ":" <> etName typ <> ".constructors") "constructors differ from oracle"
               | etConstructors typ /= etConstructors oracleType
               ]

compareClasses :: ModuleInterface -> ModuleInterface -> [InterfaceMismatch]
compareClasses candidate oracle =
  concatMap compareClass (miClasses candidate)
  where
    oracleClasses = keyed ecName (miClasses oracle)
    basePath = miModule candidate <> ".class"

    compareClass klass =
      case Map.lookup (ecName klass) oracleClasses of
        Nothing -> [mismatch (basePath <> ":" <> ecName klass) "class is not exported by oracle"]
        Just oracleClass -> compareMethods klass oracleClass

    compareMethods klass oracleClass =
      concatMap compareMethod (ecMethods klass)
      where
        oracleMethods = keyed cmName (ecMethods oracleClass)
        methodPath = basePath <> ":" <> ecName klass <> ".method"

        compareMethod method =
          case Map.lookup (cmName method) oracleMethods of
            Nothing -> [mismatch (methodPath <> ":" <> cmName method) "method is not exported by oracle"]
            Just oracleMethod
              | cmType method /= cmType oracleMethod ->
                  [mismatch (methodPath <> ":" <> cmName method <> ".type") "method type differs from oracle"]
              | otherwise -> []

compareFixities :: ModuleInterface -> ModuleInterface -> [InterfaceMismatch]
compareFixities candidate oracle =
  concatMap compareFixity (miFixities candidate)
  where
    oracleFixities = keyed fiName (miFixities oracle)
    basePath = miModule candidate <> ".fixity"

    compareFixity fixity =
      case Map.lookup (fiName fixity) oracleFixities of
        Nothing -> [mismatch (basePath <> ":" <> fiName fixity) "fixity is not exported by oracle"]
        Just oracleFixity
          | fixity /= oracleFixity ->
              [mismatch (basePath <> ":" <> fiName fixity) "fixity differs from oracle"]
          | otherwise -> []

keyed :: (Ord k) => (a -> k) -> [a] -> Map k a
keyed key = Map.fromList . map (\value -> (key value, value))

mismatch :: Text -> Text -> InterfaceMismatch
mismatch = InterfaceMismatch

renderInterfaceMismatch :: InterfaceMismatch -> String
renderInterfaceMismatch item =
  T.unpack (mismatchPath item <> ": " <> mismatchMessage item)
