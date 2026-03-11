{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--  Internal structure of a Registry and
--  associated functions
module Data.Registry.Internal.Registry where

import Data.Registry.Internal.Dynamic
import Data.Registry.Internal.Stack
import Data.Registry.Internal.Types
import Protolude as P
import Type.Reflection

-- | Find a value having a target type from:
--     - a list of "preferred values" (Specializations) to select when we are trying
--        to find the target in a specific context (Context). Context describes
--       the types of values we are currently trying to (recursively) make
--
--     - a list of already created values (Values)
--
--  3 subtleties:
--    1. if there are specialized values we need to find the most specialized for
--      the current context, that is the one having its "targetType" the "lowest" in the
--      values graph
--
--    2. if an already created value has the right type but if it is a specialization
--       and the type we are looking for is not in the specialization context
--       then we cannot use that value, we need to recreate a brand new one
--
--    3. if an already created value has the right type and is not specialized
--       but if there is an incompatible specialization for one of its dependencies
--       then it cannot be used
findValueOrSpecialization :: SomeTypeRep -> Context -> Specializations -> Values -> Maybe (Either Specialization Value)
findValueOrSpecialization target context specializations values = do
  -- 1. first try to find the target value in the list of specializations
  -- those all are all the specializations which make sense in this context
  let applicableSpecializations = specializations `applicableTo` context
  let bestSpecialization = findBestSpecializationFromApplicable target context applicableSpecializations

  let compatibleValue = findCompatibleCreatedValue target specializations values
  fmap Left bestSpecialization <|> fmap Right compatibleValue

-- | Among all the applicable specializations take the most specific one
--   if there exists any
findBestSpecializationFromApplicable :: SomeTypeRep -> Context -> Specializations -> Maybe Specialization
findBestSpecializationFromApplicable target context (Specializations sp) = do
  -- the candidates must have the required type
  let specializationCandidates = filter (\s -> target == specializationTargetType s) sp
  -- the best specialization is the one having its last context type the deepest in the current context
  let bestSpecializations = sortOn (specializationRange context) specializationCandidates
  head bestSpecializations

-- | Among all the created values, take a compatible one
--
--    - 2. and 3. if that value is a specialized value or has specialized
--      dependencies it must be compatible with the current context
findCompatibleCreatedValue :: SomeTypeRep -> Specializations -> Values -> Maybe Value
findCompatibleCreatedValue target specializations values = do
  let isNotSpecializedForAnotherContext value =
        not (hasSpecializedDependencies specializations value)
          && not (isInSpecializationContext target value)

  head $ filter isNotSpecializedForAnotherContext (findValues target values)

-- | Given a newly built value, check if there are modifiers for that
--   value and apply them before "storing" the value which means
--   adding it on top of the registry, represented by the `Values` state
--   in StateT Values.
--   We use a StateT Either because applying modifiers could fail and we want
--   to catch and report the error. Note that this error would be an implementation
--   error (and not a user error) since at the type-level everything should be correct
storeValue :: Modifiers -> Value -> Stack Value
storeValue (Modifiers ms) value = do
  let modifiers = findModifiers ms
  valueToStore <- modifyValue value modifiers
  modifyValues (addValue valueToStore)
  pure valueToStore
  where
    -- find the applicable modifiers
    findModifiers = filter (\(m, _) -> valueDynTypeRep value == m)

    -- apply a list of modifiers to a value
    modifyValue :: Value -> [(SomeTypeRep, ModifierFunction)] -> Stack Value
    modifyValue v [] = pure v
    modifyValue v ((_, f) : rest) = do
      applied <- lift $ applyModification (f (specializedContexts v)) v
      modifyValue applied rest
