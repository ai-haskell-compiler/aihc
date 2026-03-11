module Data.Tree.BranchLeaf where

import           Data.Traversable as Traversable(Traversable(traverse))
import           Control.Applicative (Applicative, (<*>), )
import qualified Control.Applicative as App
import qualified Control.Monad as Monad
import qualified Data.List     as List

import Prelude hiding (map, mapM, )


data T branch leaf =
     Branch branch [T branch leaf]
   | Leaf leaf
   deriving (Show)

map :: (branch0 -> branch1)
    -> (leaf0 -> leaf1)
    -> (T branch0 leaf0 -> T branch1 leaf1)
map branchF leafF = fold (Branch . branchF) (Leaf . leafF)

mapCond ::
       (branch -> Bool)
    -> (branch -> branch)
    -> (leaf -> leaf)
    -> (T branch leaf -> T branch leaf)
mapCond descend branchF leafF =
   let recourse =
          switch
             (\branch -> Branch (branchF branch) .
                if descend branch
                   then List.map recourse
                   else id)
             (Leaf . leafF)
   in  recourse

fold :: (branch -> [a] -> a)
     -> (leaf -> a)
     -> (T branch leaf -> a)
fold branchF leafF =
   let recourse =
          switch (\x -> branchF x . List.map recourse) leafF
   in  recourse

switch ::
        (branch -> [T branch leaf] -> a)
     -> (leaf -> a)
     -> (T branch leaf -> a)
switch branchF _ (Branch x subTrees) = branchF x subTrees
switch _ leafF (Leaf x)          = leafF x

allSubTrees :: T branch leaf -> [T branch leaf]
allSubTrees tree =
   tree :
   switch (const (concatMap allSubTrees)) (const []) tree



mapA :: Applicative m =>
       (branch0 -> m branch1)
    -> (leaf0 -> m leaf1)
    -> (T branch0 leaf0 -> m (T branch1 leaf1))
mapA branchF leafF =
   foldA
      (App.liftA Branch . branchF)
      (App.liftA Leaf . leafF)

foldA :: Applicative m =>
        (branch -> m ([a] -> a))
     -> (leaf -> m a)
     -> (T branch leaf -> m a)
foldA branchF leafF =
   let recourse =
          switch (\x subTrees ->
                        branchF x <*> traverse recourse subTrees)
                   leafF
   in  recourse

foldM :: Monad m =>
        (branch -> [a] -> m a)
     -> (leaf -> m a)
     -> (T branch leaf -> m a)
foldM branchF leafF =
   let recourse =
          switch (\x subTrees ->
                        branchF x =<< Monad.mapM recourse subTrees)
                   leafF
   in  recourse
