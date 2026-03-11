module Data.Type.Witness.Specific.FiniteAllFor where

import Data.Type.Witness.General.Finite
import Data.Type.Witness.Specific.All
import Data.Type.Witness.Specific.Either
import Data.Type.Witness.Specific.Some
import Import

type FiniteAllFor :: forall k. (k -> Type) -> (k -> Type) -> Type
type role FiniteAllFor representational representational
data FiniteAllFor f w = MkFiniteAllFor
    { finiteDomain :: [Some w]
    , finiteGetAllFor :: forall t. w t -> f t
    }

finiteAllFor :: FiniteAllFor f w -> AllFor f w
finiteAllFor (MkFiniteAllFor _ f) = MkAllFor f

finiteCodomain :: FiniteAllFor f w -> [Some f]
finiteCodomain sw = fmap (\(MkSome st) -> MkSome $ finiteGetAllFor sw st) $ finiteDomain sw

mapFiniteAllFor :: (forall t. f1 t -> f2 t) -> FiniteAllFor f1 w -> FiniteAllFor f2 w
mapFiniteAllFor ff (MkFiniteAllFor ai i) = MkFiniteAllFor ai $ \s -> ff $ i s

eitherFiniteAllFor :: FiniteAllFor t w1 -> FiniteAllFor t w2 -> FiniteAllFor t (EitherType w1 w2)
eitherFiniteAllFor (MkFiniteAllFor a1 i1) (MkFiniteAllFor a2 i2) =
    MkFiniteAllFor
        ((fmap (mapSome LeftType) a1) ++ (fmap (mapSome RightType) a2))
        (unAllFor $ eitherAllFor (MkAllFor i1) (MkAllFor i2))

mkFiniteAllFor ::
       forall f w. FiniteWitness w
    => (forall t. w t -> f t)
    -> FiniteAllFor f w
mkFiniteAllFor = MkFiniteAllFor allWitnesses
