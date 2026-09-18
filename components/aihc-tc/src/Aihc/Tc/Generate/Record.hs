{-# LANGUAGE OverloadedStrings #-}

-- | Record syntax support. The type checker expands record construction,
-- record update, and record patterns into positional constructor syntax
-- before it checks them. GHC does the same expansion in its type checker.
--
-- The head of the syntax is a data constructor or a record pattern
-- synonym. Both expand the same way, because a bidirectional pattern
-- synonym binds its builder under its own name and matches through a
-- constructor pattern, so only the field order differs.
module Aihc.Tc.Generate.Record
  ( lookupRecordHead,
    orderRecordFields,
    recordUpdateHeads,
    synthesizedRecordLocal,
    recordFieldLabel,
    recordHeadNameSyntax,
  )
where

import Aihc.Parser.Syntax
  ( Name (..),
    NameType (..),
    RecordField (..),
    SourceSpan,
    UnqualifiedName (..),
    mkAnnotation,
    mkUnqualifiedName,
    nameText,
  )
import Aihc.Resolve (Identifier (..), ResolutionAnnotation (..), ResolutionNamespace (..), ResolvedName (..))
import Aihc.Tc.Env (DataConFieldInfo (..), DataConInfo (..), DataTypeInfo (..), RecordHead (..), dataConRecordHead, patSynRecordHead)
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Data.List (nub)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | The field label of a record field occurrence. A qualifier such as
-- @IO.bufR@ names the module, not the label.
recordFieldLabel :: RecordField a -> Text
recordFieldLabel = nameText . recordFieldName

-- | The head of a resolved record occurrence. A data constructor comes
-- first: a pattern synonym never shares a name with one in scope.
lookupRecordHead :: Name -> TcM RecordHead
lookupRecordHead conSyntax = do
  target <- resolvedTermTarget conSyntax
  case target of
    ResolvedTopLevel packageId resolvedModule resolvedName -> do
      let conName = nameText resolvedName
          origin = (packageId, resolvedModule)
      dataTypes <- getDataTypes
      let matches =
            [ con
            | dataType <- dataTypes,
              con <- dtiConstructors dataType,
              dciName con == conName,
              dciOrigin con == origin
            ]
      case matches of
        con : _ -> pure (dataConRecordHead con)
        [] -> do
          mPatSyn <- lookupPatSynTarget target
          case mPatSyn of
            Just info -> pure (patSynRecordHead info)
            Nothing -> abortTc ("record constructor missing from type environment: " <> show conName <> " resolved as " <> show target)
    _ -> abortTc ("record constructor is not a top-level name: " <> show (nameText conSyntax) <> " resolved as " <> show target)

-- | Put the record fields of one occurrence in declaration order. A field
-- that the occurrence does not name gets the default value, which is told
-- the label it stands for.
orderRecordFields :: Maybe SourceSpan -> RecordHead -> [RecordField a] -> (Maybe Text -> TcM a) -> TcM [a]
orderRecordFields sp head' fields defaultValue = do
  let declared = rhFields head'
      labels = catMaybes declared
      unknown = filter (`notElem` labels) (map recordFieldLabel fields)
      duplicates = duplicateLabels (map recordFieldLabel fields)
  case unknown of
    label : _ ->
      abortTc (T.unpack (rhName head') <> " has no field named " <> show label <> " at " <> show sp)
    [] -> pure ()
  case duplicates of
    label : _ ->
      abortTc ("record field " <> show label <> " occurs more than once at " <> show sp)
    [] -> pure ()
  mapM pick declared
  where
    pick label =
      case [recordFieldValue occurrence | occurrence <- fields, Just (recordFieldLabel occurrence) == label] of
        value : _ -> pure value
        [] -> defaultValue label

duplicateLabels :: [Text] -> [Text]
duplicateLabels labels = nub [label | (index, label) <- zip [0 :: Int ..] labels, label `elem` take index labels]

-- | The heads that a record update can rebuild. The data type comes from
-- the scrutinee type when it is known, and otherwise from the field
-- labels. Each head in the result has every updated field. A record
-- pattern synonym is the head when no data constructor owns the labels:
-- its fields do not belong to the scrutinee's own data type, so the
-- scrutinee type cannot select it.
recordUpdateHeads :: Maybe SourceSpan -> Maybe TcType -> [Text] -> TcM [RecordHead]
recordUpdateHeads sp scrutineeType labels = do
  dataTypes <- getDataTypes
  let byLabel = filter (any hasAllLabels . dtiConstructors) dataTypes
  candidates <-
    case scrutineeType of
      Just (TcTyCon tyCon _) -> do
        maybeDataType <- lookupDataType tyCon
        pure (maybe byLabel (filter (any hasAllLabels . dtiConstructors) . pure) maybeDataType)
      _ -> pure byLabel
  case candidates of
    [dataType] ->
      pure (map dataConRecordHead (filter hasAllLabels (dtiConstructors dataType)))
    [] -> do
      patSyns <- filter hasAllFields <$> recordPatSynHeads
      case patSyns of
        [head'] -> pure [head']
        [] -> abortTc ("no record type has the fields " <> show labels <> " at " <> show sp)
        _ -> abortTc ("record update with the fields " <> show labels <> " is ambiguous between the pattern synonyms " <> show (map rhName patSyns) <> " at " <> show sp)
    _ -> abortTc ("record update with the fields " <> show labels <> " is ambiguous between " <> show (map dtiName candidates) <> " at " <> show sp)
  where
    hasAllLabels con =
      all (`elem` mapMaybe dcfiLabel (dciFields con)) labels
    hasAllFields head' =
      all ((`elem` rhFields head') . Just) labels

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
        [mkAnnotation (ResolutionAnnotation Nothing (IdentifierNamed text) ResolutionNamespaceTerm (ResolvedLocal unique (mkUnqualifiedName NameVarId text)))]
    )

-- | A resolved occurrence of a record head. The pattern and the expression
-- of a record update expansion use it.
recordHeadNameSyntax :: RecordHead -> Name
recordHeadNameSyntax head' =
  Name (Just moduleName') nameType' text [mkAnnotation (ResolutionAnnotation Nothing (IdentifierNamed text) ResolutionNamespaceTerm (ResolvedTopLevel packageId moduleName' resolved))]
  where
    (packageId, moduleName') = rhOrigin head'
    text = rhName head'
    nameType' =
      case T.uncons text of
        Just (first, _) | first == ':' -> NameConSym
        _ -> NameConId
    resolved = Name Nothing nameType' text []
