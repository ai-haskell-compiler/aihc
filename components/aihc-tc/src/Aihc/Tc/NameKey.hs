{-# LANGUAGE TypeApplications #-}

module Aihc.Tc.NameKey
  ( nameOccurrenceKey,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    Name (..),
    fromAnnotation,
    renderName,
  )
import Aihc.Resolve.Types
  ( ResolutionAnnotation (..),
    ResolutionNamespace (..),
    ResolvedName (..),
  )
import Aihc.Tc.Monad (OccurrenceKey (..), ResolvedOccurrenceKey (..))
import Data.Maybe (listToMaybe, mapMaybe)

nameOccurrenceKey :: Name -> Maybe OccurrenceKey
nameOccurrenceKey =
  termResolution . nameAnns

termResolution :: [Annotation] -> Maybe OccurrenceKey
termResolution anns =
  resolvedOccurrenceKey . resolutionTarget
    =<< listToMaybe
      [ ann
      | ann <- mapMaybe (fromAnnotation @ResolutionAnnotation) anns,
        resolutionNamespace ann == ResolutionNamespaceTerm
      ]

resolvedOccurrenceKey :: ResolvedName -> Maybe OccurrenceKey
resolvedOccurrenceKey resolved =
  OccurrenceResolved
    <$> case resolved of
      ResolvedTopLevel name -> Just (ResolvedTopLevelKey (renderName name))
      ResolvedLocal uniqueId _ -> Just (ResolvedLocalKey uniqueId)
      ResolvedBuiltin name -> Just (ResolvedBuiltinKey name)
      ResolvedError _ -> Nothing
