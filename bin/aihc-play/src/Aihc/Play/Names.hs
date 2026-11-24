{-# LANGUAGE OverloadedStrings #-}

-- | Every name occurrence of a module with what it stands for.
--
-- Name resolution attaches a 'Resolved' to each name. This
-- module collects them with their spans, so a page can give each entity
-- one color and paint every occurrence of it.
module Aihc.Play.Names
  ( Occurrence (..),
    Target (..),
    occurrences,
    occurrenceJson,
  )
where

import Aihc.Parser.Syntax (Annotation, SourceSpan, fromAnnotation)
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Play.Json (Json (..))
import Aihc.Resolve
  ( Entity (..),
    GlobalName (..),
    LocalId (..),
    PackageId (..),
    ResolutionNamespace (..),
    ResolveError (..),
    Resolved (..),
  )
import Aihc.Resolve.Syntax (Module, Name (..), ResolutionResult (..), UnqualifiedName (..))
import Aihc.Resolve.Syntax.Traverse (Collect, Walk (..), collected, idWalk, runCollect, walk)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data Target
  = -- | A local binder, by its number in the unit.
    TargetLocal !Int
  | -- | A top-level entity: package, module, name.
    TargetGlobal !Text !Text !Text
  | -- | Built-in syntax such as a tuple constructor.
    TargetSyntax
  | -- | The resolver found nothing, with its message.
    TargetUnresolved !Text
  deriving (Eq, Show)

data Occurrence = Occurrence
  { occurrenceSpan :: !SourceSpan,
    occurrenceText :: !Text,
    occurrenceNamespace :: !ResolutionNamespace,
    occurrenceTarget :: !Target,
    -- | Whether the occurrence binds the name.
    occurrenceBinder :: !Bool
  }
  deriving (Eq, Show)

-- | The occurrences of a resolved module, in source order.
occurrences :: Module -> [Occurrence]
occurrences = sortOn (\o -> (spanStart (occurrenceSpan o), occurrenceBinder o)) . runCollect (walk occurrenceWalk)
  where
    spanStart = fromAnnotationStart

fromAnnotationStart :: SourceSpan -> (Int, Int)
fromAnnotationStart span' = (Syntax.sourceSpanStartLine span', Syntax.sourceSpanStartCol span')

occurrenceWalk :: Walk (Collect Occurrence)
occurrenceWalk =
  idWalk
    { walkName = \children name -> collected (maybe [] pure (fromName name)) *> children name,
      walkUnqualifiedName = \children name -> collected (maybe [] pure (fromBinder name)) *> children name
    }

fromName :: Name -> Maybe Occurrence
fromName name = resolutionOccurrence (nameAnns name) (nameText name) False =<< nameResolution name

fromBinder :: UnqualifiedName -> Maybe Occurrence
fromBinder name = resolutionOccurrence (unqualifiedNameAnns name) (unqualifiedNameText name) True =<< unqualifiedNameResolution name

resolutionOccurrence :: [Annotation] -> Text -> Bool -> ResolutionResult -> Maybe Occurrence
resolutionOccurrence anns text binder result =
  case result of
    ResolutionOk resolution -> resolved anns text binder resolution
    ResolutionError failure -> unresolved anns text binder failure

resolved :: [Annotation] -> Text -> Bool -> Resolved -> Maybe Occurrence
resolved anns text binder resolution = do
  span' <- case resolutionSpan resolution of
    Just span' -> Just span'
    Nothing -> ownSpan anns
  pure
    Occurrence
      { occurrenceSpan = span',
        occurrenceText = text,
        occurrenceNamespace = resolutionNamespace resolution,
        occurrenceTarget = target (resolutionTarget resolution),
        occurrenceBinder = binder
      }

unresolved :: [Annotation] -> Text -> Bool -> ResolveError -> Maybe Occurrence
unresolved anns text binder failure = do
  span' <- case resolveErrorSpan failure of
    Just span' -> Just span'
    Nothing -> ownSpan anns
  pure
    Occurrence
      { occurrenceSpan = span',
        occurrenceText = text,
        occurrenceNamespace = resolveErrorNamespace failure,
        occurrenceTarget = TargetUnresolved (T.pack (resolveErrorMessage failure)),
        occurrenceBinder = binder
      }

ownSpan :: [Annotation] -> Maybe SourceSpan
ownSpan = listToMaybe . mapMaybe fromAnnotation

target :: Entity -> Target
target entity =
  case entity of
    EntityLocal (LocalId unique) -> TargetLocal unique
    EntityGlobal global ->
      TargetGlobal (packageIdText (globalNamePackage global)) (globalNameModule global) (globalNameText global)
    EntitySyntax -> TargetSyntax

occurrenceJson :: Occurrence -> Json
occurrenceJson occurrence =
  JObject
    [ ("line", JInt (Syntax.sourceSpanStartLine span')),
      ("col", JInt (Syntax.sourceSpanStartCol span')),
      ("endLine", JInt (Syntax.sourceSpanEndLine span')),
      ("endCol", JInt (Syntax.sourceSpanEndCol span')),
      ("text", JString (occurrenceText occurrence)),
      ("namespace", JString (namespaceText (occurrenceNamespace occurrence))),
      ("binder", JBool (occurrenceBinder occurrence)),
      ("target", targetJson (occurrenceTarget occurrence))
    ]
  where
    span' = occurrenceSpan occurrence

namespaceText :: ResolutionNamespace -> Text
namespaceText namespace =
  case namespace of
    ResolutionNamespaceTerm -> "term"
    ResolutionNamespaceType -> "type"
    ResolutionNamespaceModule -> "module"

targetJson :: Target -> Json
targetJson t =
  case t of
    TargetLocal unique -> JObject [("kind", JString "local"), ("id", JInt unique)]
    TargetGlobal package modu name ->
      JObject [("kind", JString "global"), ("package", JString package), ("module", JString modu), ("name", JString name)]
    TargetSyntax -> JObject [("kind", JString "syntax")]
    TargetUnresolved message -> JObject [("kind", JString "unresolved"), ("message", JString message)]
