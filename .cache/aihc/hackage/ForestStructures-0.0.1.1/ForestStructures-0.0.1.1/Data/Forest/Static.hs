
-- | A data structure for a static forest.

module Data.Forest.Static where

import           Control.DeepSeq (NFData(..))
import           Control.Applicative ((<$>),(<*>))
import           Control.Monad (replicateM)
import           Data.Foldable (toList)
import           Data.Graph.Inductive.Basic
import           Data.List (span,uncons,sort)
import           Data.Traversable (mapAccumL)
import           Data.Tree (Tree)
import           Debug.Trace
import qualified Data.List as L
import qualified Data.Map.Strict as S
import qualified Data.Set as Set
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import           Test.QuickCheck
import           GHC.Generics(Generic)
import           Data.Aeson (ToJSON(..),FromJSON(..))



-- | Kind of possible @TreeOrder@s.
--
-- TODO @In@ for in-order traversal?
--
-- TODO @Unordered@ for trees that have no sorted order?

data TreeOrder = Pre | Post | Unordered



-- | A static forest structure. While traversals are always explicitly
-- possible by following the indices, the nodes themselves shall always be
-- ordered by the type @p :: TreeOrder@. This is not completely enforced,
-- given that @Forest@ is exporting the constructor, but encouraged via
-- construction with helper functions. The labels of type @a@ (in @label@)
-- require a vector structure @v@ for @O(1)@ access.

data Forest (p ∷ TreeOrder) v a = Forest
  { label     ∷ !(v a)
    -- ^ Each node @k@ in @[0..n-1]@ has a label at @label ! k@.
  , parent    ∷ !(VU.Vector Int)
    -- ^ Each node @k@ has a parent node, or @-1@ if there is no such
    -- parent.
  , children  ∷ !(V.Vector (VU.Vector Int))
    -- ^ Each node @k@ has a vector of indices for its children. For leaf
    -- nodes, the vector is empty.
  , lsib      ∷ !(VU.Vector Int)
    -- ^ The left sibling for a node @k@. Will *not* cross subtrees. I.e.
    -- if @k@ is @lsib@ of @l@, then @k@ and @l@ have the same parent.
  , rsib      ∷ !(VU.Vector Int)
    -- ^ The right sibling for a node @k@.
  , roots     ∷ !(VU.Vector Int)
    -- ^ The roots of the individual trees, the forest was constructed
    -- from.
  }
  deriving (Eq,Ord,Read,Show,Generic)

instance (NFData (v a)) ⇒ NFData (Forest p v a)

instance ToJSON (v a) ⇒ ToJSON (Forest p v a)

instance FromJSON (v a) ⇒ FromJSON (Forest p v a)




-- | Construct a static 'Forest' with a tree traversal function. I.e.
-- @forestWith preorderF trees@ will construct a pre-order forest from the
-- list of @trees@.
--
-- Siblings span trees in the forest!

forestWith ∷ (VG.Vector v a) ⇒ (forall a . [T.Tree a] → [a]) → [T.Tree a] → Forest (p∷TreeOrder) v a
forestWith f ts
  = Forest { label    = VG.fromList $ f ts
           , parent   = VU.fromList $ map (\(_,k,_ ,_) -> k             ) $ f pcs
           , children = V.fromList  $ map (\(_,_,cs,_) -> VU.fromList cs) $ f pcs
           , lsib     = VU.fromList $ map fst $ S.elems lr
           , rsib     = VU.fromList $ map snd $ S.elems lr
           , roots    = VU.fromList $ map (fst . T.rootLabel) us
           }
  where
    -- Step 1: construct a forest isomorphic to @ts@ but labelled with
    -- a total order of unique identifiers. (That is: label with @Int@s).
    -- The unique identifiers are in pre-order.
    ps = addIndicesF' 0 ts
    -- Step 2: use @f@ to produce a permutation map and apply this
    -- permutation to turn the pre-order @ps@ into the required order.
    backp = VU.fromList $ map snd $ sort $ zip (f ps) [0..]
    -- Step 3: decorate the forest with indices in the correct order. Keep
    -- the label in @snd@.
    us = map (fmap (\(k,l) -> (backp VG.! k,l))) $ addIndicesF 0 ts
    -- Step 4: add the correct relations (children, lrSibling, parents)
    pcs = parentChildrenF (-1) us
    -- A map with the left and right sibling
    lr  = lrSiblingF us



-- | Construct a pre-ordered forest.

forestPre ∷ (VG.Vector v a) ⇒ [T.Tree a] → Forest Pre v a
forestPre = forestWith preorderF

-- | Construct a post-ordered forest.

forestPost ∷ (VG.Vector v a) ⇒ [T.Tree a] → Forest Post v a
forestPost = forestWith postorderF

-- | Add @pre-ordered@ @(!)@ indices. First argument is the starting index.

addIndices ∷ Int → T.Tree a → T.Tree (Int,a)
addIndices k = snd . mapAccumL (\i e -> (i+1, (i,e))) k

-- | Add @pre-ordered@ @(!)@ indices, but to a forest.

addIndicesF ∷ Int → [T.Tree a] → [T.Tree (Int,a)]
addIndicesF k = snd . mapAccumL go k
  where go = mapAccumL (\i e -> (i+1, (i,e)))

-- | Add @pre-ordered@ @(!)@ indices to a forest, but throw the label away as
-- well.

addIndicesF' ∷ Int → [T.Tree a] → [T.Tree Int]
addIndicesF' k = snd . mapAccumL go k
  where go = mapAccumL (\i e -> (i+1, i))

-- | Add parent + children information. Yields
-- @(Index,Parent,[Child],Label)@. Parent is @-1@ if root node.

parentChildrenF ∷ Int → [T.Tree (Int,a)] → [T.Tree (Int,Int,[Int],a)]
parentChildrenF k ts = [ T.Node (i,k,children sf,l) (parentChildrenF i sf)  | T.Node (i,l) sf <- ts ]
  where children sf = map (fst . T.rootLabel) sf

-- | Return a map with all the nearest siblings for each node, for a forest.

lrSiblingF ∷ [T.Tree (Int,a)] → S.Map Int (Int,Int)
lrSiblingF = S.delete (-1) . lrSibling . T.Node (-1,error "laziness in lrSiblingF broken")

-- | Return a map with all the nearest siblings for each node, for a tree.

lrSibling ∷ T.Tree (Int,a) → S.Map Int (Int,Int)
lrSibling = S.fromList . map splt . T.flatten . go ([]::[Int])
  where go sib (T.Node (k,lbl) frst) = let cs = [l | T.Node (l,_) _ <- frst] in T.Node (k,lbl,sib) [ go cs t | t <- frst]
        splt (k,_,[])  = (k,(-1,-1))
        splt (k,_,sbl) = let (ls,rs) = span (/=k) sbl in (k,(last $ (-1):ls,head $ tail rs ++ [-1]))

-- | Return the left-most leaf for each node.

leftMostLeaves ∷ Forest p v a → VU.Vector Int
leftMostLeaves f = VG.map (leftMostLeaf f) $ VG.enumFromN 0 $ VG.length $ parent f

-- | Just the leaf-most leaf for a certain node.

leftMostLeaf ∷ Forest p v a → Int → Int
leftMostLeaf f = go
  where go k = let cs = children f VG.! k
               in if VG.null cs then k else go (VG.head cs)

-- | Return the right-most leaf for each node.

rightMostLeaves ∷ Forest p v a → VU.Vector Int
rightMostLeaves f = VG.map (rightMostLeaf f) $ VG.enumFromN 0 $ VG.length $ parent f

-- | Given a tree, and a node index, return the right-most leaf for the
-- node.

rightMostLeaf ∷ Forest p v a → Int → Int
rightMostLeaf f = go
  where go k = let cs = children f VG.! k
               in  if VG.null cs then k else go (VG.last cs)

-- | Return all left key roots. These are the nodes that have no (super-)
-- parent with the same left-most leaf.
--
-- This function is somewhat specialized for tree editing.
--
-- TODO group by

leftKeyRoots ∷ Forest Post v a → VU.Vector Int
leftKeyRoots f = VU.fromList . sort . S.elems $ VU.foldl' go S.empty (VU.enumFromN (0::Int) $ VG.length $ parent f)
        -- Build a map from left-most leaf to most root-near node.
  where go s k = S.insertWith max (lml VU.! k) k s
        lml  = leftMostLeaves f

-- | Returns the list of all sorted subsets of subforests in the forest.
-- If the forest is given in pre-order, then The subsets are returned in
-- reversed pre-order.
--
-- TODO turn this into @newtype vectors@ that enforce @size >= 1@.

sortedSubForests ∷ Forest p v a → [VU.Vector Int]
sortedSubForests f =
  -- cleanup
  map VU.fromList
  . L.nub     -- TODO revise later, is in @O(n^2)@
  . concat
  -- make sure that in our partial order we have smaller forests come
  -- first.
  . map (map unSrt . Set.toList . Set.fromList . map Srt)
  -- get all nonempty ordered subforests
  . map (concatMap (L.tail . L.subsequences))
  . map (L.permutations)
  . map VG.toList . VG.toList
  -- only nodes with children
  . VG.filter (not . VG.null)
  -- every node that has children in reverse order
  -- make sure that the roots are there, but come last
  $ VG.snoc (VG.reverse (children f)) (roots f)

newtype Srt = Srt { unSrt ∷ [Int] }
  deriving (Eq,Show)

instance Ord Srt where
  Srt xs <= Srt ys = length xs <= length ys

-- | Given a forest, return the list of trees that constitue the forest.

forestToTrees ∷ (VG.Vector v a) ⇒ Forest p v a → T.Forest a
forestToTrees Forest{..} = map getTree . VG.toList $ roots
  where getTree k = T.Node (label VG.! k) (map getTree . VG.toList $ children VG.! k)



-- * QuickCheck

-- | Wrapped quickcheck instance for 'T.Tree'.

newtype QCTree a = QCTree { getTree ∷ T.Tree a }
  deriving (Show)

instance (Arbitrary a) ⇒ Arbitrary (QCTree a) where
  arbitrary =
    let go = sized $ \n →
               do val ← arbitrary
                  let n' = n `div` 2
                  nodes ← if n' > 0
                    then do k ← choose (0,n')
                            resize n' $ replicateM k (getTree <$> arbitrary)
                    else return []
                  return $ T.Node val nodes
    in  QCTree <$> go
  shrink (QCTree (T.Node val forest)) =
    [] -- [ QCTree $ T.Node v f | v <- shrink val, f <- map (map getTree) $ shrink $ map QCTree forest ]

--  -- * Test functions
--  
--  test1 :: [T.Tree Char]
--  test1 = [T.Node 'R' [T.Node 'a' [], T.Node 'b' []], T.Node 'S' [T.Node 'x' [], T.Node 'y' []]]
--  
--  test2 :: [T.Tree Char]
--  test2 = [T.Node 'R' [T.Node 'a' [], T.Node 'b' [], T.Node 'c' []]]
--  
--  runtest t = do
--    print (forestPre t :: Forest Pre V.Vector Char)
--    print (forestPost t :: Forest Post V.Vector Char)
--    print (forestPost [T.Node 'R' [T.Node 'a' []]] :: Forest Post V.Vector Char)
--    print (forestPost [T.Node 'R' [T.Node 'a' [], T.Node 'b' []]] :: Forest Post V.Vector Char)
--    print (sortedSubForests (forestPre t :: Forest Pre V.Vector Char))
--  
