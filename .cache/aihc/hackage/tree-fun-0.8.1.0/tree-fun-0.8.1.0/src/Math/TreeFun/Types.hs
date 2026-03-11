-- Types module
-- By Gregory W. Schwartz

-- | Collects all types used in the program

module Math.TreeFun.Types where

-- Built-in
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Tree

-- Algebraic
data PropertySuperTree a b = PropertySuperTree
                             { superTree       :: !(Tree (SuperNode a))
                             , superProperties :: !(PropertyMap a b) }

-- Tree with super smart nodes
data SuperNode a = SuperRoot | SuperNode { myRootLabel :: !a
                                         , myParent    :: !(SuperNode a)
                                         , myLeaves    :: !(M.Map a (Int, Int)) }
                                         deriving (Read, Show, Eq, Ord)

-- Basic
type Height = Int

-- Advanced
type DistanceMap a        = M.Map a (M.Map Int (S.Seq a))
type PropertyMap a b      = M.Map a (S.Seq b)
type MaybePropertyMap a b = M.Map a (Maybe (S.Seq b))
