{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Dynamically typed values: a value paired with a representation of its
-- type, so that a heterogeneous container can hand a value back at the type
-- it was stored with.
module Data.Dynamic
  ( -- * The @Dynamic@ type
    Dynamic (..),

    -- * Converting to and from @Dynamic@
    toDyn,
    fromDyn,
    fromDynamic,

    -- * Applying functions of dynamic type
    dynApply,
    dynApp,
    dynTypeRep,

    -- * Convenience re-exports
    Typeable,
  )
where

import Data.Type.Equality ((:~~:) (..))
import GHC.Base (Maybe (..), (++), (.))
import GHC.Err (errorWithoutStackTrace)
import GHC.Internal.Exception.Type (Exception)
import GHC.Show (Show (..), showString, shows)
import GHC.Types (Type)
import Type.Reflection (SomeTypeRep (..), TypeRep, Typeable (..), eqTypeRep, typeRepKind, pattern Fun)

-- | A value of arbitrary type, carrying the representation of that type.
data Dynamic where
  Dynamic :: forall a. TypeRep a -> a -> Dynamic

instance Show Dynamic where
  showsPrec _ (Dynamic representation _) =
    showString "<<" . shows representation . showString ">>"

instance Exception Dynamic

-- | Wrap a value, remembering its type.
toDyn :: (Typeable a) => a -> Dynamic
toDyn = Dynamic typeRep

-- | Unwrap a value of the expected type, or return the default.
fromDyn :: forall a. (Typeable a) => Dynamic -> a -> a
fromDyn (Dynamic representation value) fallback =
  case eqTypeRep representation (typeRep :: TypeRep a) of
    Just HRefl -> value
    Nothing -> fallback

-- | Unwrap a value of the expected type.
fromDynamic :: forall a. (Typeable a) => Dynamic -> Maybe a
fromDynamic (Dynamic representation value) =
  case eqTypeRep representation (typeRep :: TypeRep a) of
    Just HRefl -> Just value
    Nothing -> Nothing

-- | Apply a dynamic function to a dynamic argument. The result is 'Nothing'
-- unless the function's argument type is the argument's type.
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dynamic (Fun argument result) function) (Dynamic argument' value) =
  case eqTypeRep argument argument' of
    Just HRefl ->
      case eqTypeRep (typeRep :: TypeRep Type) (typeRepKind result) of
        Just HRefl -> Just (Dynamic result (function value))
        Nothing -> Nothing
    Nothing -> Nothing
dynApply _ _ = Nothing

-- | 'dynApply' that raises an error on a type mismatch.
dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp function argument =
  case dynApply function argument of
    Just result -> result
    Nothing ->
      errorWithoutStackTrace
        ( "Type error in dynamic application.\n"
            ++ "Can't apply function "
            ++ show (dynTypeRep function)
            ++ " to argument "
            ++ show (dynTypeRep argument)
        )

-- | The type of the wrapped value.
dynTypeRep :: Dynamic -> SomeTypeRep
dynTypeRep (Dynamic representation _) = SomeTypeRep representation
