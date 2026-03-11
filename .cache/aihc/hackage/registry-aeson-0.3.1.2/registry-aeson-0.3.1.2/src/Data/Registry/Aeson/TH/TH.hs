{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-
  TemplateHaskell utility functions to create encoders and decoders
-}

module Data.Registry.Aeson.TH.TH where

import Control.Monad.Fail
import Data.List (elemIndex)
import Data.Registry.Aeson.TH.ThOptions
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)

-- | From:
--    - the list of all the types of a data type
--    - the list of the parameters types for one of the constructors
--    Return:
--     - the type
--     - its index in the list of parameter types
--     - its index in the list of all the types
indexConstructorTypes :: [Type] -> [Type] -> Q [(Type, Int, Int)]
indexConstructorTypes allTypes constructorTypes =
  for (zip [0..] constructorTypes) $ \(n, t) ->
    case elemIndex t allTypes of
      Just k -> pure (t, n, k)
      Nothing -> fail $ "the type " <> show t <> " cannot be found in the list of all types " <> show allTypes

-- | Get the types of all the fields of a constructor
typesOf :: Con -> Q [Type]
typesOf (NormalC _ types) = pure (snd <$> types)
typesOf (RecC _ types) = pure $ (\(_, _, t) -> t) <$> types
typesOf other = do
  qReport True ("we can only create encoders / decoders for normal constructors and records, got: " <> show other)
  fail "encoders / decoders creation failed"

-- | Get the name of a constructor
nameOf :: Con -> Q Name
nameOf (NormalC n _) = pure n
nameOf (RecC n _) = pure n
nameOf other = do
  qReport True ("we can only create encoders / decoders for normal constructors and records, got: " <> show other)
  fail "encoders / decoders creation failed"

-- | Get the list of names of a constructor
fieldsOf :: Con -> Q [Name]
fieldsOf (NormalC _ _) = pure []
fieldsOf (RecC _ types) = pure $ (\(f, _, _) -> f) <$> types
fieldsOf other = do
  qReport True ("we can only create encoders / decoders for normal constructors and records, got: " <> show other)
  fail "encoders / decoders creation failed"

-- | Remove the module name from a qualified name
makeName :: ThOptions -> Name -> Name
makeName options = mkName . toS . modifyTypeName options . show

-- | Return the name of a given type with a modified name based on options
getSimpleTypeName :: ThOptions -> Type -> Name
getSimpleTypeName options (ForallT _ _ ty) = getSimpleTypeName options ty
getSimpleTypeName options (VarT name) = makeName options name
getSimpleTypeName options (ConT name) = makeName options name
getSimpleTypeName options (TupleT n) = makeName options $ tupleTypeName n
getSimpleTypeName options ArrowT = makeName options ''(->)
getSimpleTypeName options ListT = makeName options ''[]
getSimpleTypeName options (AppT t1 t2) = mkName (show (getSimpleTypeName options t1) <> " " <> show (getSimpleTypeName options t2))
getSimpleTypeName options (SigT t _) = getSimpleTypeName options t
getSimpleTypeName options (UnboxedTupleT n) = makeName options $ unboxedTupleTypeName n
getSimpleTypeName _ t = panic $ "getSimpleTypeName: Unknown type: " <> show t
