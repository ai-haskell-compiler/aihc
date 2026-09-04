{-# LANGUAGE OverloadedStrings #-}

-- | Record syntax support. The type checker expands record construction,
-- record update, and record patterns into positional constructor syntax
-- before it checks them. GHC does the same expansion in its type checker.
module Aihc.Tc.Generate.Record
  ( lookupRecordConstructor,
    orderRecordFields,
    recordUpdateConstructors,
    synthesizedRecordLocal,
    recordFieldLabel,
    constructorNameSyntax,
  )
where

import Aihc.Parser.Syntax
  ( Name (..),
    NameType (..),
    RecordField (..),
    SourceSpan (..),
    UnqualifiedName (..),
    mkAnnotation,
    mkUnqualifiedName,
    nameText,
  )
import Aihc.Resolve (Identifier (..), ResolutionAnnotation (..), ResolutionNamespace (..), ResolvedName (..))
import Aihc.Tc.Env (DataConFieldInfo (..), DataConInfo (..), DataTypeInfo (..))
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | The field label of a record field occurrence. A qualifier such as
-- @IO.bufR@ names the module, not the label.
recordFieldLabel :: RecordField a -> Text
recordFieldLabel = nameText . recordFieldName

-- | The constructor information of a resolved constructor occurrence.
lookupRecordConstructor :: Name -> TcM DataConInfo
lookupRecordConstructor conSyntax = do
  target <- resolvedTermTarget conSyntax
  case target of
    ResolvedTopLevel packageId resolvedName -> do
      let conName = nameText resolvedName
          origin = (packageId, fromMaybe "" (nameQualifier resolvedName))
      dataTypes <- getDataTypes
      let matches =
            [ con
            | dataType <- dataTypes,
              con <- dtiConstructors dataType,
              dciName con == conName,
              dciOrigin con == origin
            ]
      case matches of
        con : _ -> pure con
        [] -> abortTc ("record constructor missing from type environment: " <> show conName <> " resolved as " <> show target)
    _ -> abortTc ("record constructor is not a top-level name: " <> show (nameText conSyntax) <> " resolved as " <> show target)

-- | Put the record fields of one constructor occurrence in declaration
-- order. A field that the occurrence does not name gets the default value.
orderRecordFields :: SourceSpan -> DataConInfo -> [RecordField a] -> (DataConFieldInfo -> TcM a) -> TcM [a]
orderRecordFields sp con fields defaultValue = do
  let labels = mapMaybe dcfiLabel (dciFields con)
      unknown = filter (`notElem` labels) (map recordFieldLabel fields)
      duplicates = duplicateLabels (map recordFieldLabel fields)
  case unknown of
    label : _ ->
      abortTc (T.unpack (dciName con) <> " has no field named " <> show label <> " at " <> show sp)
    [] -> pure ()
  case duplicates of
    label : _ ->
      abortTc ("record field " <> show label <> " occurs more than once at " <> show sp)
    [] -> pure ()
  mapM pick (dciFields con)
  where
    pick field =
      case [recordFieldValue occurrence | occurrence <- fields, Just (recordFieldLabel occurrence) == dcfiLabel field] of
        value : _ -> pure value
        [] -> defaultValue field

duplicateLabels :: [Text] -> [Text]
duplicateLabels labels = nub [label | (index, label) <- zip [0 :: Int ..] labels, label `elem` take index labels]

-- | The constructors that a record update can rebuild. The data type comes
-- from the scrutinee type when it is known, and otherwise from the field
-- labels. Each constructor in the result has every updated field.
recordUpdateConstructors :: SourceSpan -> Maybe TcType -> [Text] -> TcM [DataConInfo]
recordUpdateConstructors sp scrutineeType labels = do
  dataTypes <- getDataTypes
  let byLabel = filter (any hasAllLabels . dtiConstructors) dataTypes
  candidates <-
    case scrutineeType of
      Just (TcTyCon tyCon _) -> do
        maybeDataType <- lookupDataType tyCon
        pure (maybe byLabel pure maybeDataType)
      _ -> pure byLabel
  case candidates of
    [dataType] ->
      case filter hasAllLabels (dtiConstructors dataType) of
        [] -> abortTc ("no constructor of " <> T.unpack (dtiName dataType) <> " has the fields " <> show labels <> " at " <> show sp)
        constructors -> pure constructors
    [] -> abortTc ("no record type has the fields " <> show labels <> " at " <> show sp)
    _ -> abortTc ("record update with the fields " <> show labels <> " is ambiguous between " <> show (map dtiName candidates) <> " at " <> show sp)
  where
    hasAllLabels con =
      all (`elem` mapMaybe dcfiLabel (dciFields con)) labels

-- | A local binder that the type checker makes for a record expansion. The
-- negative unique does not collide with a resolver local or with the fixed
-- binders of a pattern synonym expansion.
synthesizedRecordLocal :: Text -> TcM UnqualifiedName
synthesizedRecordLocal text = do
  Unique key <- freshUnique
  let unique = negate (1000 + key)
  pure
    ( UnqualifiedName
        NameVarId
        text
        [mkAnnotation (ResolutionAnnotation NoSourceSpan (IdentifierNamed text) ResolutionNamespaceTerm (ResolvedLocal unique (mkUnqualifiedName NameVarId text)))]
    )

-- | A resolved occurrence of a constructor. The pattern and the expression
-- of a record update expansion use it.
constructorNameSyntax :: DataConInfo -> Name
constructorNameSyntax con =
  Name (Just moduleName') nameType' text [mkAnnotation (ResolutionAnnotation NoSourceSpan (IdentifierNamed text) ResolutionNamespaceTerm (ResolvedTopLevel packageId resolved))]
  where
    (packageId, moduleName') = dciOrigin con
    text = dciName con
    nameType' =
      case T.uncons text of
        Just (first, _) | first == ':' -> NameConSym
        _ -> NameConId
    resolved = Name (Just moduleName') nameType' text []
