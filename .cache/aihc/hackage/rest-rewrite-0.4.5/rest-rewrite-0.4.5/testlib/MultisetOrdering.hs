{-# LANGUAGE ScopedTypeVariables#-}
module MultisetOrdering where

import Data.Hashable
import Language.REST.Dot
import qualified Data.Maybe as Mb
import qualified Data.List as L
import qualified Language.REST.Internal.MultiSet as M
import qualified Data.HashMap.Strict as Mp
import qualified Data.HashSet as S
import Language.REST.Types

data Replace a =
    ReplaceOne a a
  | Replace a [a]
  deriving (Show)

newtype MultisetGE a = MultisetGE [Replace a] deriving (Show)

type GTE a = a -> a -> Bool

type Indexed a = (a, Int)

type IndexedMultisetPair a = (Indexed (M.MultiSet (Indexed a)) , Indexed (M.MultiSet (Indexed a)))

multisetGE :: forall a . Eq a => GTE a -> M.MultiSet a -> M.MultiSet a -> Maybe (MultisetGE a)
multisetGE gte ts0 us0 = go [] (M.toList ts0) (M.toList us0)
  where
    equiv t u = t `gte` u && u `gte` t
    gt t u = t `gte` u && not (u `gte` t)

    go :: [Replace a] -> [a] -> [a] -> Maybe (MultisetGE a)
    go rs (t : ts) us | Just u <- L.find (equiv t) us
      = go (ReplaceOne t u:rs) ts (L.delete u us)

    go rs (t : ts) us  =
        let
          (lts, us') = L.partition (t `gt`)  us
        in
          go (Replace t lts : rs) ts us'
    go rs ts [] = Just $ MultisetGE $ map (`Replace` []) ts ++ rs
    go _  [] _  = Nothing


multisetOrd :: (Eq a, Hashable a, Ord a)  => [a] -> [a] -> Maybe (MultisetGE a)
multisetOrd ts us = multisetGE (>=) (M.fromList ts) (M.fromList us)

zindex :: [a] -> [(a, Int)]
zindex xs = zip xs [0 ..]

indexMS :: (Eq a, Hashable a) => M.MultiSet a -> M.MultiSet (a, Int)
indexMS ms = M.fromList $ zindex (M.toList ms)

mkEdge :: NodeID -> NodeID -> Edge
mkEdge t u = Edge t u " " "black" " " "solid"

botNodeName :: Int -> Int -> String
botNodeName tIndex mIndex = "bot_" ++ show tIndex ++ "_" ++ show mIndex

botNode :: Int -> Int -> Node
botNode tIndex mIndex =
  Node (botNodeName tIndex mIndex) "⊥" "solid" "black"

toGraph' :: forall a. (Eq a, Hashable a, Show a) => GTE a -> [M.MultiSet a] -> DiGraph
toGraph' gte mss0 = DiGraph "msograph" (toOrderedSet (S.union elemNodes botNodes)) (toOrderedSet edges)
  where
    indexed :: [(M.MultiSet (a, Int), Int)]
    indexed = zindex (map indexMS mss0)

    pairs :: [((M.MultiSet (a, Int), Int), (M.MultiSet (a, Int), Int))]
    pairs = zip indexed (tail indexed)

    elemNodes = S.fromList $ filter hasEdge $ concatMap toNodes indexed

    hasEdge node = any (`pointsTo` node) $ S.toList edges

    pointsTo edge node =
      from edge == nodeID node || to edge == nodeID node

    edges :: S.HashSet Edge
    edges = S.fromList $ topEdges ++ map snd (replEdges pairs)

    topEdges = map go (M.toList (fst $ head indexed)) where
      go (_, index) =
        mkEdge "⊤" (nodeName (index,  0))

    botNodes = S.fromList $ Mb.mapMaybe fst (replEdges pairs)

    nodeName :: (Int,  Int) -> String
    nodeName (elemIndex,  msIndex) =
      "n" ++ show elemIndex ++ "_" ++ show msIndex

    replEdges = toEdges Mp.empty

    toEdges :: Mp.HashMap (Int, Int) (Int, Int) -> [IndexedMultisetPair a] -> [(Maybe Node, Edge)]
    toEdges _ [] = []
    toEdges mp (((ts, tsIndex), (us, usIndex)) : mss) =
        concatMap redges repls ++ toEdges mp' mss
      where
        Just (MultisetGE repls) = multisetGE (\t u -> gte (fst t) (fst u)) ts us

        lookupTIndex :: Int -> (Int, Int)
        lookupTIndex tindex = Mb.fromMaybe (tindex, tsIndex) (Mp.lookup (tindex, tsIndex) mp)

        mp' = go mp repls where
          go mpi [] = mpi
          go mpi ((ReplaceOne (_, i) (_, j)):repls')
            = go (Mp.insert (j, usIndex) (lookupTIndex i) mpi) repls'
          go mpi (_:repls') = go mpi repls'


        redges (Replace (_, index) [])
          = [ ( Just (botNode index tsIndex)
              , mkEdge
                (nodeName (lookupTIndex index))
                (botNodeName index tsIndex)
              ) ]
        redges (ReplaceOne _ _) = []
        redges (Replace (_, tindex) us') = map go us' where
          go (_, uindex) =
            (Nothing, mkEdge (nodeName (lookupTIndex tindex)) (nodeName (uindex,  usIndex)))

    toNodes (ms, index) = map go (M.toList ms) where

      go (e, elemIndex) =
        Node
          (nodeName (elemIndex, index))
          (show e)
          "solid"
          "black"

toGraph :: (Ord a, Eq a, Hashable a, Show a) => [[a]] -> DiGraph
toGraph mss = toGraph' (>=) $ map M.fromList mss

mkMSOGraph :: (Ord a, Eq a, Hashable a, Show a) => String -> [[a]] -> IO ()
mkMSOGraph name mss = mkGraph name (toGraph mss)

mkMSOGraphs :: (Ord a, Eq a, Hashable a, Show a) => String -> [[a]] -> IO ()
mkMSOGraphs name mss0 = mapM_ go (drop 1 $ L.inits mss0) where
  go mss = mkGraph (name ++ show (length mss)) (toGraph mss)

multisetGE' :: (Ord a, Hashable a) => [a] -> [a] -> Maybe (MultisetGE a)
multisetGE' ts us = multisetGE (>=) (M.fromList ts) (M.fromList us)
