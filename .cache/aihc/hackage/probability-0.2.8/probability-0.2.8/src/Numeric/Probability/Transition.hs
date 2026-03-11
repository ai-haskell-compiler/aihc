-- | Deterministic and probabilistic generators
module Numeric.Probability.Transition where

import qualified Numeric.Probability.Distribution as Dist

import qualified Control.Monad.Trans.Except as ME

import qualified Data.Map as Map

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Prelude hiding (map, maybe, id, )


-- * Transitions


-- | deterministic generator
type Change a = a -> a

-- | probabilistic generator
type T prob a = a -> Dist.T prob a


id :: (Num prob) => T prob a
id = Dist.certainly


{- |
'map' maps a change function to the result of a transformation
('map' is somehow a lifted form of 'Dist.map')
The restricted type of @f@ results from the fact that the
argument to @t@ cannot be changed to @b@ in the result 'T' type.
-}
map :: (Num prob, Ord a) =>
   Change a -> T prob a -> T prob a
map f t = Dist.map f . t


{- |
unfold a distribution of transitions into one transition

NOTE: The argument transitions must be independent
-}
unfold :: (Num prob, Ord a) =>
   Dist.T prob (T prob a) -> T prob a
unfold d x = Dist.unfold (fmap ($x) d)

{- |
Composition of transitions similar to 'Numeric.Probability.Monad.compose'
but with intermediate duplicate elimination.
-}
compose :: (Num prob, Ord a) =>
   [T prob a] -> T prob a
compose = foldl (\acc x v -> Dist.norm (acc v >>= x)) return


untilLeft :: (Num prob, Ord a, Ord b) =>
   (a -> Dist.T prob (Either b a)) -> Dist.T prob a -> Dist.T prob b
untilLeft f =
   let go final dist =
          if null (Dist.decons dist)
            then Dist.Cons $ Map.toList final
            else
               case ListHT.unzipEithers $
                    List.map (\(e,p) -> either (\l -> Left (l,p)) (\r -> Right (r,p)) e) $
                    Dist.decons $ Dist.norm $ dist >>= f of
                  (newFinal, stillActive) ->
                     go (Map.unionWith (+) (Map.fromListWith (+) newFinal) final) $
                     Dist.Cons stillActive
   in  go Map.empty

{- |
In @fix $ \go a -> do ...; go xy@
any action after a 'go' is ignored.
-}
fix :: (Num prob, Ord a, Ord b) =>
   ((a -> ME.ExceptT a (Dist.T prob) b) ->
    (a -> ME.ExceptT a (Dist.T prob) b)) ->
   Dist.T prob a -> Dist.T prob b
fix f =
   untilLeft $ \a ->
      case f ME.throwE a of
         ME.ExceptT m -> fmap (either Right Left) m


-- * Spreading changes into transitions

-- | functions to convert a list of changes into a transition
type SpreadC prob a = [Change a] -> T prob a

apply :: (Num prob) =>
   Change a -> T prob a
apply f = id . f


maybe :: (Num prob) => prob -> Change a -> T prob a
maybe p f x = Dist.choose p (f x) x

lift :: Dist.Spread prob a -> SpreadC prob a
lift s cs x = s $ List.map ($ x) cs

uniform :: (Fractional prob) => SpreadC prob a
uniform  = lift Dist.uniform

linear :: (Fractional prob) => SpreadC prob a
linear = lift Dist.linear

normal :: (Floating prob) => SpreadC prob a
normal   = lift Dist.normal

enum :: (RealFloat prob) => [Int] -> SpreadC prob a
enum xs  = lift (Dist.enum xs)

relative :: (RealFloat prob) => [prob] -> SpreadC prob a
relative xs  = lift (Dist.relative xs)


-- * Spreading transitions into transitions

-- | functions to convert a list of transitions into a transition
type SpreadT prob a = [T prob a] -> T prob a

liftT :: (Num prob, Ord a) =>
   Dist.Spread prob (T prob a) -> SpreadT prob a
liftT s = unfold . s

uniformT :: (Fractional prob, Ord a) => SpreadT prob a
uniformT  = liftT Dist.uniform

linearT :: (Fractional prob, Ord a) => SpreadT prob a
linearT = liftT Dist.linear

normalT :: (Floating prob, Ord a) => SpreadT prob a
normalT   = liftT Dist.normal

enumT :: (RealFloat prob, Ord a) => [Int] -> SpreadT prob a
enumT xs  = liftT (Dist.enum xs)

relativeT :: (RealFloat prob, Ord a) => [prob] -> SpreadT prob a
relativeT xs  = liftT (Dist.relative xs)
