-- This code is stolen from: http://okmij.org/ftp/Haskell/misc.htm
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}
module Shuffle (shuffle1) where

-- A complete binary tree, of leaves and internal nodes.
-- Internal node: Node card l r
-- where card is the number of leaves under the node.
-- Invariant: card >=2. All internal tree nodes are always full.
data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show

fix :: (a -> a) -> a
fix f = g where g = f g -- The fixed point combinator

-- Convert a sequence (e1...en) to a complete binary tree

buildTree :: [a] -> Tree a
buildTree = fix grow_level . map Leaf
  where
    grow_level _ [node] = node
    grow_level self l = self $ inner l

    inner [] = []
    inner [e] = [e]
    inner (e1:e2:rest) = join e1 e2 : inner rest

    join l@(Leaf _)       r@(Leaf _)       = Node 2 l r
    join l@(Node ct _ _)  r@(Leaf _)       = Node (ct+1) l r
    join l@(Leaf _)       r@(Node ct _ _)  = Node (ct+1) l r
    join l@(Node ctl _ _) r@(Node ctr _ _) = Node (ctl+ctr) l r


-- given a sequence (e1,...en) to shuffle, and a sequence
-- (r1,...r[n-1]) of numbers such that r[i] is an independent sample
-- from a uniform random distribution [0..n-i], compute the
-- corresponding permutation of the input sequence.
shuffle1 :: [a] -> [Int] -> [a]
shuffle1 elements = shuffle1' (buildTree elements)
  where
    shuffle1' (Leaf e) [] = [e]
    shuffle1' tree (r:r_others) =
      let (b,rest) = extract_tree r tree
      in b : shuffle1' rest r_others

    -- extract_tree n tree
    -- extracts the n-th element from the tree and returns
    -- that element, paired with a tree with the element
    -- deleted.
    -- The function maintains the invariant of the completeness
    -- of the tree: all internal nodes are always full.
    -- The collection of patterns below is deliberately not complete.
    -- All the missing cases may not occur (and if they do,
    -- that's an error.
    extract_tree 0 (Node _ (Leaf e) r) = (e,r)
    extract_tree 1 (Node 2 (Leaf l) (Leaf r)) = (r,Leaf l)
    extract_tree n (Node c (Leaf l) r) =
      let (e,new_r) = extract_tree (n-1) r
      in (e,Node (c-1) (Leaf l) new_r)
    extract_tree n (Node n1 l (Leaf e))
      | n+1 == n1 = (e,l)

    extract_tree n (Node c l@(Node cl _ _) r)
      | n < cl = let (e,new_l) = extract_tree n l
                 in (e,Node (c-1) new_l r)
      | otherwise = let (e,new_r) = extract_tree (n-cl) r
                    in (e,Node (c-1) l new_r)
