module Game.Tree (
   T,
   build,
   mapNodesLeaves,
   mapTrees,
   maximumMove,
   maximumMoveFast,
   minimumMove,
   minimumMoveFast,
   pruneVolume,
   pruneDepth,
   selectDeepSubTree,
   selectSubTree,
   state,
   subTrees,
   scanChildren,
   ) where

import Data.Maybe (fromMaybe, )
import Data.List  (maximumBy, minimumBy, sortBy, )

import Data.Tuple.HT (mapSnd, )
import Data.Function.HT (compose2, )


data T move state =
     Cons {state    :: state,
           subTrees :: [(move, T move state)]}
   deriving (Show, Read)

mapTrees :: (a -> b) -> [(c,a)] -> [(c,b)]
mapTrees f = map (mapSnd f)

mapNodesLeaves :: (state0 -> state1) -> (state0 -> state1) ->
   T move state0 -> T move state1
mapNodesLeaves _ leafFunc (Cons st []) =
   Cons (leafFunc st) []
mapNodesLeaves nodeFunc leafFunc (Cons st subs) =
   Cons (nodeFunc st) (mapTrees (mapNodesLeaves nodeFunc leafFunc) subs)

instance Functor (T move) where
   fmap f (Cons st subs) =
      Cons (f st) (mapTrees (fmap f) subs)

scanChildren ::
   ([(move, T move state)] -> [(move, T move state)]) ->
      T move state -> T move state
scanChildren f (Cons st subs) =
   Cons st (f (mapTrees (scanChildren f) subs))


build :: (state -> [(move, state)]) -> state -> T move state
build nextMoves start =
   Cons start (mapTrees (build nextMoves)
                        (nextMoves start))

selectSubTree :: (Eq move) => move -> T move state -> T move state
selectSubTree mv (Cons _ subs) =
   fromMaybe (error "selectSubTree: illegal move") (lookup mv subs)

selectDeepSubTree :: (Eq move) => [move] -> T move state -> T move state
selectDeepSubTree =
   flip (foldl (flip selectSubTree))

{- | prune the tree to a fixed depth -}
pruneDepth :: Int -> T move state -> T move state
pruneDepth 0 (Cons st _) = Cons st []
pruneDepth n (Cons st subs) =
   Cons st (mapTrees (pruneDepth (n-1)) subs)

{- | prune the tree roughly to a fixed volume -}
pruneVolume :: Int -> T move state -> T move state
pruneVolume 0 (Cons st _) = Cons st []
pruneVolume n (Cons st subs) =
   let subSize = div n (length subs)
   in  Cons st (mapTrees (pruneVolume subSize) subs)


maximise, minimise :: (Ord score) => T move score -> score
maximise (Cons score []) = score
maximise (Cons _ subs) =
   maximum (map (minimise . snd) subs)

minimise (Cons score []) = score
minimise (Cons _ subs) =
   minimum (map (maximise . snd) subs)


maximumMove, minimumMove :: (Ord score) => T move score -> move
maximumMove (Cons _ subs) =
   fst (maximumBy (compose2 compare snd)
                  (mapTrees maximise subs))
minimumMove (Cons _ subs) =
   fst (minimumBy (compose2 compare snd)
                  (mapTrees minimise subs))


maximiseFast, minimiseFast :: (Ord score) => T move score -> [score]
maximiseFast (Cons score []) = [score]
maximiseFast (Cons _ subs) =
   mapMinimum (map (minimiseFast . snd) subs)

minimiseFast (Cons score []) = [score]
minimiseFast (Cons _ subs) =
   mapMaximum (map (maximiseFast . snd) subs)

{- it holds
      maximum (map minimum xs) = maximum (mapMinimum xs)
   but compared to (map minimum) those minima are omited,
   which do not alter the overall maximum -}
mapMaximum, mapMinimum :: (Ord score) => [[score]] -> [score]
mapMaximum [] = error "GameTree.mapMaximum: empty list"
mapMaximum (x:xs) =
   let bound = maximum x
   in  bound : map maximum (filter (all (<=bound)) xs)
mapMinimum [] = error "GameTree.mapMinimum: empty list"
mapMinimum (x:xs) =
   let bound = minimum x
   in  bound : map minimum (filter (all (>=bound)) xs)


{- only if child notes are sorted,
   then mapMaximum can prune uninteresting sub trees -}
sortChildrenAsc, sortChildrenDesc :: (Ord score) =>
   T move score -> T move score
-- sortChildrenAsc = scanChildren (sortBy (compose2 compare state))
sortChildrenAsc (Cons st subs) =
   Cons st (sortBy (compose2 compare (state . snd))
                   (mapTrees sortChildrenDesc subs))
sortChildrenDesc (Cons st subs) =
   Cons st (sortBy (compose2 (flip compare) (state . snd))
                   (mapTrees sortChildrenAsc subs))


maximumMoveFast, minimumMoveFast :: (Ord score) => T move score -> move
maximumMoveFast =
   fst . maximumBy (compose2 compare snd) .
   mapTrees (maximum . maximiseFast) . subTrees .
   sortChildrenAsc
minimumMoveFast =
   fst . minimumBy (compose2 compare snd) .
   mapTrees (minimum . minimiseFast) . subTrees .
   sortChildrenDesc
