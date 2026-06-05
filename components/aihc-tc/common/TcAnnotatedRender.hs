{-# LANGUAGE OverloadedStrings #-}

-- | Human-readable inline rendering for type-checker annotations.
module TcAnnotatedRender
  ( renderAnnotatedTcResults,
  )
where

import Aihc.Parser.Syntax (Annotation, Module, fromAnnotation, moduleName)
import Aihc.Tc (TcModuleResult (..), TcRenderedAnnotation (..))
import Aihc.Testing.AnnotatedModule (renderAnnotatedModuleSources)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Prettyprinter (Doc, pretty)

renderAnnotatedTcResults :: [Text] -> [TcModuleResult] -> [String]
renderAnnotatedTcResults sources results =
  case compare (length sources) (length results) of
    LT -> error "renderAnnotatedTcResults: fewer source texts than type-checker results"
    GT -> error "renderAnnotatedTcResults: more source texts than type-checker results"
    EQ ->
      let moduleSources = sortOn (moduleDisplayName . tcmModule . snd) (zip sources results)
       in renderAnnotatedModuleSources renderedTcLabelDoc (map fst moduleSources) (map (tcmModule . snd) moduleSources)

moduleDisplayName :: Module -> Text
moduleDisplayName modu =
  fromMaybe "<unnamed>" (moduleName modu)

renderedTcLabelDoc :: Annotation -> Maybe (Doc ann)
renderedTcLabelDoc annotation = do
  TcRenderedAnnotation label <- fromAnnotation annotation
  pure (pretty label)
