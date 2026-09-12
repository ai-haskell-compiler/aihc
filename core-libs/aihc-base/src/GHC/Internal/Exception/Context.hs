{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

-- Keep the data constructor and its evaluation behavior compatible with GHC.
{-# HLINT ignore "Use newtype instead of data" #-}

module GHC.Internal.Exception.Context
  ( ExceptionContext (..),
    emptyExceptionContext,
    addExceptionAnnotation,
    getExceptionAnnotations,
    getAllExceptionAnnotations,
    mergeExceptionContext,
    displayExceptionContext,
    SomeExceptionAnnotation (..),
    ExceptionAnnotation (..),
  )
where

import Data.Typeable (Typeable, cast)
import GHC.Base (Maybe (..), String, (++))
import GHC.Prim.Show (Show (..))

data ExceptionContext = ExceptionContext [SomeExceptionAnnotation]

data SomeExceptionAnnotation = forall a. (ExceptionAnnotation a) => SomeExceptionAnnotation a

class (Typeable a) => ExceptionAnnotation a where
  displayExceptionAnnotation :: a -> String
  default displayExceptionAnnotation :: (Show a) => a -> String
  displayExceptionAnnotation = show

emptyExceptionContext :: ExceptionContext
emptyExceptionContext = ExceptionContext []

addExceptionAnnotation :: (ExceptionAnnotation a) => a -> ExceptionContext -> ExceptionContext
addExceptionAnnotation annotation (ExceptionContext annotations) =
  ExceptionContext (SomeExceptionAnnotation annotation : annotations)

getExceptionAnnotations :: (ExceptionAnnotation a) => ExceptionContext -> [a]
getExceptionAnnotations (ExceptionContext annotations) = selectAnnotations annotations

selectAnnotations :: (ExceptionAnnotation a) => [SomeExceptionAnnotation] -> [a]
selectAnnotations [] = []
selectAnnotations (annotation : rest) =
  case selectAnnotation annotation of
    Just value -> value : selectAnnotations rest
    Nothing -> selectAnnotations rest

selectAnnotation :: (ExceptionAnnotation a) => SomeExceptionAnnotation -> Maybe a
selectAnnotation (SomeExceptionAnnotation annotation) = cast annotation

getAllExceptionAnnotations :: ExceptionContext -> [SomeExceptionAnnotation]
getAllExceptionAnnotations (ExceptionContext annotations) = annotations

mergeExceptionContext :: ExceptionContext -> ExceptionContext -> ExceptionContext
mergeExceptionContext (ExceptionContext left) (ExceptionContext right) =
  ExceptionContext (left ++ right)

displayExceptionContext :: ExceptionContext -> String
displayExceptionContext (ExceptionContext annotations) = displayAnnotations annotations

displayAnnotations :: [SomeExceptionAnnotation] -> String
displayAnnotations [] = ""
displayAnnotations [annotation] = displayAnnotation annotation
displayAnnotations (annotation : rest) =
  displayAnnotation annotation ++ "\n" ++ displayAnnotations rest

displayAnnotation :: SomeExceptionAnnotation -> String
displayAnnotation (SomeExceptionAnnotation annotation) = displayExceptionAnnotation annotation
