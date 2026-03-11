{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--  Untyped implementation of the functionalities in
--    'Data.Registry.Make'
module Data.Registry.Internal.Make where

import Data.List qualified as L hiding (unlines)
import Data.Registry.Internal.Dynamic
import Data.Registry.Internal.Reflection (showSingleType)
import Data.Registry.Internal.Registry
import Data.Registry.Internal.Stack
import Data.Registry.Internal.Types
import Data.Text qualified as T
import Protolude as P hiding (Constructor)
import Type.Reflection

-- * WARNING: HIGHLY UNTYPED IMPLEMENTATION !

-- | Make a value from a desired output type represented by SomeTypeRep
--   and a list of possible constructors
--   A 'Context' is passed in the form of a stack of the types we are trying to build so far
--  Entries is the list of all the constructors in the Registry
--  Specializations is a list of specific values to use in a given context, overriding the normal search
--  Modifiers is a list of functions to apply right before a value is stored in the Registry
makeUntyped :: SomeTypeRep -> Context -> Entries -> Specializations -> Modifiers -> Stack (Maybe Value)
makeUntyped targetType context entries specializations modifiers = do
  values <- getValues
  -- is there already a value with the desired type? Or a specialization
  let foundValue = findValueOrSpecialization targetType context specializations values

  case foundValue of
    Nothing ->
      makeWithConstructor
    -- existing value
    Just (Right v) -> do
      modified <- storeValue modifiers v
      pure (Just modified)
    -- specialization
    Just (Left specialization) -> do
      -- if the specialization is just a value, return it
      case createValueFromSpecialization context specialization of
        UntypedValue v ->
          Just <$> storeValue modifiers v
        UntypedFunction f -> do
          -- we don't fail the building if a specialization cannot be applied
          -- we try to use an already created value or build one from scratch
          catchError (makeWithFunction f $ Just specialization) $ \_ ->
            case findCompatibleCreatedValue targetType specializations values of
              Just v -> pure (Just v)
              Nothing -> makeWithConstructor
  where
    makeWithConstructor :: Stack (Maybe Value)
    makeWithConstructor = do
      -- if not, is there a way to build such value?
      case findUntyped targetType entries of
        Nothing ->
          lift $
            Left $
              "When trying to create the following values\n\n          "
                <> T.intercalate "\nrequiring " (showContextTargets context)
                <> "\n\nNo constructor was found for "
                <> showSingleType targetType
        Just (UntypedFunction f) ->
          makeWithFunction f Nothing
        Just (UntypedValue v) ->
          Just <$> storeValue modifiers v

    makeWithFunction :: Function -> Maybe Specialization -> Stack (Maybe Value)
    makeWithFunction f mSpecialization = do
      let inputTypes = collectInputTypes f
      inputs <- makeInputs f inputTypes context entries specializations modifiers

      if length inputs /= length inputTypes
        then do
          -- report an error if we cannot make enough input parameters to apply the function

          let madeInputTypes = fmap valueDynTypeRep inputs
          let missingInputTypes = inputTypes L.\\ madeInputTypes
          lift . Left . T.unlines $
            ["could not make all the inputs for ", show (funDescription f), ". Only "]
              <> (show <$> inputs)
              <> ["could be made. Missing"]
              <> fmap show missingInputTypes
        else do
          -- else apply the function and store the output value in the registry
          value <- lift $ applyFunction f inputs
          let valueWithContext =
                case (mSpecialization, value) of
                  (Just s, CreatedValue d desc Nothing deps) ->
                    CreatedValue d desc (Just (SpecializationContext context s)) deps
                  _ ->
                    value
          modified <- storeValue modifiers valueWithContext

          functionApplied modified inputs
          pure (Just modified)

-- | Show the target type and possibly the constructor function requiring it
--   for every target type in the context
showContextTargets :: Context -> [Text]
showContextTargets (Context context) =
  fmap
    ( \(t, f) ->
        case f of
          Nothing -> show t
          Just function -> show t <> "\t\t\t(required for the constructor " <> show function <> ")"
    )
    (reverse context)

-- | Make the input values of a given function
--   When a value has been made it is placed on top of the
--   existing registry so that it is memoized if needed in
--   subsequent calls
makeInputs ::
  Function ->
  -- | input types to build
  [SomeTypeRep] ->
  -- | current context of types being built
  Context ->
  -- | available entries to build values
  Entries ->
  -- | list of values to use when in a specific context
  Specializations ->
  -- | modifiers to apply before storing made values
  Modifiers ->
  Stack [Value] -- list of made values
makeInputs _ [] _ _ _ _ = pure []
makeInputs function (i : ins) c@(Context context) entries specializations modifiers =
  if i `elem` contextTypes c
    then
      lift $
        Left $
          toS $
            T.unlines $
              ["cycle detected! The current types being built are "]
                <> (show <$> context)
                <> ["But we are trying to build again " <> show i]
    else do
      madeInput <- makeUntyped i (Context ((i, Just (funDynTypeRep function)) : context)) entries specializations modifiers
      case madeInput of
        Nothing ->
          -- if one input cannot be made, iterate with the rest for better reporting
          -- of what could be eventually made
          makeInputs function ins (Context context) entries specializations modifiers
        Just v ->
          (v :) <$> makeInputs function ins (Context context) entries specializations modifiers
