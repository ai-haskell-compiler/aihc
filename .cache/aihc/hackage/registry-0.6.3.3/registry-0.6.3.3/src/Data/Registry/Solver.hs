{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- |
--  Type level functions to statically assess
--  if a value can be built out of a Registry
module Data.Registry.Solver where

import Data.Kind
import Data.Type.Equality
import GHC.TypeLits

-- | Compute the list of input types for a function
type family Inputs f :: [Type] where
  Inputs (i -> o) = i ': Inputs o
  Inputs x = '[]

-- | Compute the output type for a function
type family Output f :: Type where
  Output (i -> o) = Output o
  Output x = x

-- | Compute if a function can be added to a registry
type family CanMake (a :: Type) (els :: [Type]) (target :: Type) :: Constraint where
  CanMake a '[] t =
    TypeError
      ( Text "The function creating the output type "
          :$$: Text ""
          :$$: (Text "  " :<>: ShowType (Output t))
          :$$: Text ""
          :$$: Text "cannot be added to the registry because the input parameter"
          :$$: Text ""
          :$$: (Text "  " :<>: ShowType (Output a))
          :$$: Text ""
          :$$: Text " is not one of the registry outputs"
          :$$: Text ""
          :$$: (Text "The full function type for " :<>: ShowType (Output t) :<>: Text " is")
          :$$: Text ""
          :$$: ShowType t
          :$$: Text ""
      )
  CanMake a (a ': _els) _t = ()
  CanMake a (_b ': els) t = CanMake a els t

-- | Compute if a registry can be added to another registry
type family CanMakeMany (a :: Type) (els :: [Type]) (targets :: [Type]) :: Constraint where
  CanMakeMany a '[] ts =
    TypeError
      ( Text "The registry creating the output types "
          :$$: Text ""
          :$$: (Text "  " :<>: ShowType ts)
          :$$: Text ""
          :$$: Text "cannot be added to the other registry because one input parameter"
          :$$: Text ""
          :$$: (Text "  " :<>: ShowType (Output a))
          :$$: Text ""
          :$$: Text " is missing of the overall registry outputs"
          :$$: Text ""
      )
  CanMakeMany a (a ': _els) _ts = ()
  CanMakeMany a (_b ': els) ts = CanMakeMany a els ts

-- | Compute if each element of a list of types is contained in another list
--   when trying to add the function `target`
class IsSubset (ins :: [Type]) (out :: [Type]) (target :: Type)

instance IsSubset '[] out t

-- | The list of elements: a + els is a subset of out if els is a subset of out and
--   a is also included in the set out. The search for a in out is done via a
--   type family in order to be able to display an error message if it can't be found
instance (CanMake a out t, IsSubset els out t) => IsSubset (a ': els) out t

-- | Compute if each element of a list of types is contained in
--   another list when trying to append 2 registries together
--   where `target` is the list of inputs of the first registry
class AreSubset (ins :: [Type]) (out :: [Type]) (targets :: [Type])

instance AreSubset '[] out ts

instance (CanMakeMany a out ts, AreSubset els out ts) => AreSubset (a ': els) out ts

-- | Compute if each element of a list of types
--   is the same as another in a different order
class IsSameSet (types1 :: [Type]) (types2 :: [Type])

instance (ts1 ~ Normalized types1, ts2 ~ Normalized types2, IsSubset ts1 ts2 (), IsSubset ts1 ts2 ()) => IsSameSet types1 types2

-- | Compute if a type is contained in a list of types
type family Contains (a :: Type) (els :: [Type]) :: Constraint where
  Contains a els = Contains1 a els els

-- | Compute if a type is contained in a list of types
type family Contains1 (a :: Type) (els :: [Type]) (target :: [Type]) :: Constraint where
  Contains1 a '[] target = TypeError ('ShowType a ':<>: Text " cannot be found in " ':<>: 'ShowType target)
  Contains1 a (a ': els) t = ()
  Contains1 a (b ': els) t = Contains1 a els t

-- | Shorthand type alias when many such constraints need to be added to a type signature
type out :- a = Contains a out

-- | From the list of all the input types and outputs types of a registry
--   Can we create all the output types?
class Solvable (ins :: [Type]) (out :: [Type])

instance (IsSubset ins out ()) => Solvable ins out

-- | Extracted from the typelevel-sets project and adapted for the Registry datatype
--   This union deduplicates elements only if they appear in contiguously
--   What we really want is typelevel sets but they are too slow for now
--   https://github.com/dorchard/type-level-sets/issues/17
type family (:++) (x :: [k]) (y :: [k]) :: [k] where
  '[] :++ xs = xs
  (x ': xs) :++ ys = x ': (xs :++ ys)

-- | Return '[a] only if it is not already in the list of types
type family FindUnique (a :: Type) (as :: [Type]) :: [Type] where
  FindUnique a '[] = '[a]
  FindUnique a (a ': _rest) = '[]
  FindUnique a (_b ': rest) = FindUnique a rest

-- | Type family to remove some redundant types in a list of types
type family Normalized (as :: [Type]) :: [Type] where
  Normalized '[] = '[]
  Normalized '[a] = '[a]
  Normalized (a ': rest) = FindUnique a rest :++ Normalized rest
