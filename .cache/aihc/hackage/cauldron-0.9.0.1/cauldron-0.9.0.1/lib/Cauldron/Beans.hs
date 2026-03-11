{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | A map of 'Dynamic' values.
module Cauldron.Beans
  ( Beans,
    empty,
    insert,
    delete,
    restrictKeys,
    keysSet,
    singleton,
    fromDynList,
    toDynMap,

    -- * Looking for values
    taste,

    -- * Monoidal stuff
    unionBeansMonoidally,
    SomeMonoidTypeRep (..),
    someMonoidTypeRepMempty,

    -- * Re-exported
    toDyn,
  )
where

import Data.Dynamic
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup qualified
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Type.Equality (TestEquality (testEquality))
import Data.Typeable
import GHC.IsList
import Type.Reflection (SomeTypeRep (..))
import Type.Reflection qualified

empty :: Beans
empty = Beans Map.empty

insert :: forall bean. (Typeable bean) => bean -> Beans -> Beans
insert bean Beans {beanMap} =
  Beans {beanMap = Map.insert (typeRep (Proxy @bean)) (toDyn bean) beanMap}

delete :: TypeRep -> Beans -> Beans
delete tr Beans {beanMap} =
  Beans {beanMap = Map.delete tr beanMap}

-- | Restrict a 'Beans' map to only those 'TypeRep's found in a 'Set'.
restrictKeys :: Beans -> Set TypeRep -> Beans
restrictKeys Beans {beanMap} trs = Beans {beanMap = Map.restrictKeys beanMap trs}

singleton :: forall bean. (Typeable bean) => bean -> Beans
singleton bean = Beans do Map.singleton (typeRep (Proxy @bean)) (toDyn bean)

-- | Check if the 'Beans' map contains a value of type @bean@.
taste :: forall bean. (Typeable bean) => Beans -> Maybe bean
taste Beans {beanMap} =
  let tr = Type.Reflection.typeRep @bean
   in case Map.lookup (SomeTypeRep tr) beanMap of
        Just (Dynamic tr' v) | Just Refl <- tr `testEquality` tr' -> Just v
        _ -> Nothing

-- | A map of 'Dynamic' values, indexed by the 'TypeRep' of each 'Dynamic'.
-- Maintains the invariant that the 'TypeRep' of the key matches the 'TypeRep'
-- of the 'Dynamic'.
newtype Beans = Beans {beanMap :: Map TypeRep Dynamic}
  deriving newtype (Show)

-- | Union of two 'Beans' maps, right-biased: prefers values from the /right/
-- 'Beans' map when both contain the same 'TypeRep' key. (Note that
-- 'Data.Map.Map' is left-biased.)
instance Semigroup Beans where
  Beans {beanMap = r1} <> Beans {beanMap = r2} = Beans do Map.unionWith (flip const) r1 r2

instance Monoid Beans where
  mempty = Beans mempty

instance IsList Beans where
  type Item Beans = Dynamic
  toList (Beans {beanMap}) = Map.elems beanMap
  fromList = fromDynList

-- |
-- >>> :{
-- let beans = fromDynList [toDyn False, toDyn @Int 5]
--  in (taste @Bool beans, taste @Int beans, taste @String beans)
-- :}
-- (Just False,Just 5,Nothing)
fromDynList :: [Dynamic] -> Beans
fromDynList ds = Beans do Map.fromList do ds <&> \d -> (dynTypeRep d, d)

toDynMap :: Beans -> Map TypeRep Dynamic
toDynMap Beans {beanMap} = beanMap

-- | Like 'SomeTypeRep', but also remembering that the type has a 'Monoid' instance, which can be \"recovered\"
-- after pattern-matching on the 'SomeMonoidTypeRep'.
data SomeMonoidTypeRep where
  SomeMonoidTypeRep ::
    forall a.
    (Monoid a) =>
    Type.Reflection.TypeRep a ->
    SomeMonoidTypeRep

instance Show SomeMonoidTypeRep where
  show (SomeMonoidTypeRep tr) = show tr

instance Eq SomeMonoidTypeRep where
  (SomeMonoidTypeRep tr1) == (SomeMonoidTypeRep tr2) =
    SomeTypeRep tr1 == SomeTypeRep tr2

instance Ord SomeMonoidTypeRep where
  (SomeMonoidTypeRep tr1) `compare` (SomeMonoidTypeRep tr2) =
    SomeTypeRep tr1 `compare` SomeTypeRep tr2

-- | The 'mempty' value corresponding to the inner 'Type.Reflection.TypeRep'.
someMonoidTypeRepMempty :: SomeMonoidTypeRep -> Dynamic
someMonoidTypeRepMempty (SomeMonoidTypeRep tr) = Type.Reflection.withTypeable tr (go tr)
  where
    go :: forall t proxy. (Typeable t, Monoid t) => proxy t -> Dynamic
    go _ = toDyn (mempty @t)

-- | Union of to 'Beans' maps. If both share a 'TypeRep' key and the key is
-- present in the 'SomeMonoidTypeRep' 'Set', combine the values monoidally.
-- Otherwise, keep the value from the /second/ 'Beans' map.
unionBeansMonoidally :: Set SomeMonoidTypeRep -> Beans -> Beans -> Beans
unionBeansMonoidally reps (Beans beans1) (Beans beans2) =
  let d =
        reps
          & Set.map (\v@(SomeMonoidTypeRep tr) -> Data.Semigroup.Arg (SomeTypeRep tr) v)
          & Map.fromArgSet
      combine tr d1 d2 =
        case (Map.lookup tr d, d1, d2) of
          (Just (SomeMonoidTypeRep tr'), Dynamic tr1 v1, Dynamic tr2 v2)
            | Just Refl <- tr' `testEquality` tr1,
              Just Refl <- tr' `testEquality` tr2 ->
                Type.Reflection.withTypeable tr' (toDyn (v1 <> v2))
          _ -> d2
   in Beans $ Map.unionWithKey combine beans1 beans2

-- | The set of all 'TypeRep' keys of the map.
keysSet :: Beans -> Set TypeRep
keysSet Beans {beanMap} = Map.keysSet beanMap
