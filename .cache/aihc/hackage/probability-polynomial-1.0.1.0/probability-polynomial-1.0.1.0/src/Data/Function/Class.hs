{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
Description : Type class for functions, e.g. polynomials.
-}
module Data.Function.Class
    ( Function (..)
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | An instance of 'Function' is a type that represents functions.
-- Function can be evaluated at points in their 'Domain'.
--
-- Examples: Polynomials, trigonometric polynomials, piecewise polynomials, …
class Function f where
    -- | The __domain__ of definition of the function.
    type Domain f
    -- | The __codomain__ of a function is the set of potential function values,
    -- i.e. function values never lie outside this set.
    --
    -- In contrast, the set of actual function values
    -- is called the __image__ and
    -- is typically a strict subset of the codomain.
    type Codomain f

    -- | Evaluate a function at a point in its 'Domain'.
    eval :: f -> Domain f -> Codomain f

-- | Functions are 'Function'.
instance Function (a -> b) where
    type Domain (a -> b) = a
    type Codomain (a -> b) = b

    eval = id

-- | @'Map.Map' k v@ represents a function @k -> Maybe v@.
--
-- > Domain   (Map k v) = k
-- > Codomain (Map k v) = Maybe v
instance Ord k => Function (Map.Map k v) where
    type instance Domain (Map.Map k v) = k
    type instance Codomain (Map.Map k v) = Maybe v

    eval = flip Map.lookup

-- | @'Set.Set' v@ represents a function @v -> Bool@.
--
-- > Domain   (Set v) = v
-- > Codomain (Set v) = Bool
instance Ord v => Function (Set.Set v) where
    type Domain (Set.Set v) = v
    type Codomain (Set.Set v) = Bool

    eval = flip Set.member
