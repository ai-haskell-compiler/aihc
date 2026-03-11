module Math.SetCover.Queue.Map (Methods, methods) where

import qualified Math.SetCover.Queue as Queue

import qualified Math.SetCover.EnumMap as EnumMapX
import qualified Data.OrdPSQ as PSQ
import qualified Data.EnumSet as EnumSet
import qualified Data.Map as Map; import Data.Map (Map)
import Control.Applicative ((<$>))
import Data.Monoid (Monoid, mempty, mappend)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Tuple.HT (mapFst, mapSnd)


type Methods a queue set = Queue.Methods (PSQ.OrdPSQ a Int queue) (Map a set)

methods :: Ord a => Queue.Methods queue set -> Methods a queue set
methods m =
   Queue.Methods {
      Queue.fromEnumMap =
         PSQ.fromList .
         mapMaybe
            (\(elm, sets) ->
               (\(minSize, ns) -> (elm, minSize, ns)) <$>
               (addMinSize m $ Queue.fromEnumMap m sets)) .
         Map.toList . EnumMapX.transposeMap,
      Queue.partition =
         applyWriter ((mapSnd (addMinSize m) .) . Queue.partition m),
      Queue.difference = \q s ->
         apply ((addMinSize m .) . Queue.difference m)
            q (EnumMapX.transposeMap s),
      Queue.findMinValue = \qo -> do
         (elm,_,qi) <- PSQ.findMin qo
         let (minSet,ns) =
               checkSubQueue "findMinValue" $ Queue.findMinValue m qi
         return (Map.singleton elm minSet, ns),
      Queue.null = PSQ.null
   }

checkSubQueue :: String -> Maybe queue -> queue
checkSubQueue name =
   fromMaybe (error ("Queue.Map." ++ name ++ ": empty sub-queue"))

addMinSize :: Queue.Methods queue set -> queue -> Maybe (Int, queue)
addMinSize m q = flip (,) q . EnumSet.size <$> Queue.findMin m q

applyWriter ::
   (Ord p, Ord k, Monoid c) =>
   (v -> b -> (c, Maybe (p, v))) ->
   PSQ.OrdPSQ k p v -> Map k b -> (c, PSQ.OrdPSQ k p v)
applyWriter f q =
   Map.foldlWithKey
      (\(sis, qi) a ss ->
         mapFst (mappend sis) $
         PSQ.alter (maybe (mempty, Nothing) $ \(_p,subq) -> f subq ss) a qi)
      (mempty,q)

apply ::
   (Ord k, Ord p) =>
   (a -> b -> Maybe (p, a)) ->
   PSQ.OrdPSQ k p a -> Map k b -> PSQ.OrdPSQ k p a
apply f =
   Map.foldlWithKey (\qi a ss -> updatePSQ (\(_p,subq) -> f subq ss) a qi)

updatePSQ ::
   (Ord p, Ord k) =>
   ((p, v) -> Maybe (p, v)) -> k -> PSQ.OrdPSQ k p v -> PSQ.OrdPSQ k p v
updatePSQ f k = snd . PSQ.alter ((,) () . (f=<<)) k
