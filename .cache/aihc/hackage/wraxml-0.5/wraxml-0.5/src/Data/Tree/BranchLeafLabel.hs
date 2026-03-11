module Data.Tree.BranchLeafLabel where

import           Control.Monad.HT ((<=<))
import           Data.Traversable as Traversable(Traversable(traverse))
import           Control.Applicative (Applicative, (<*>), )
import qualified Control.Applicative as App
import qualified Control.Monad as Monad
import qualified Data.List     as List

import Prelude hiding (map, mapM, )


type T i branch leaf = (i, Elem i branch leaf)

data Elem i branch leaf =
     Branch branch [T i branch leaf]
   | Leaf leaf
   deriving (Show)

map :: (branch0 -> branch1)
    -> (leaf0 -> leaf1)
    -> (T i branch0 leaf0 -> T i branch1 leaf1)
map branchF leafF = fold (,) (Branch . branchF) (Leaf . leafF)

mapLabel :: (i -> j)
    -> (T i branch leaf -> T j branch leaf)
mapLabel f = fold ((,) . f) Branch Leaf

mapCond ::
       (branch -> Bool)
    -> (branch -> branch)
    -> (leaf -> leaf)
    -> (T i branch leaf -> T i branch leaf)
mapCond descend branchF leafF =
   let recourse =
          switch (,)
             (\branch -> Branch (branchF branch) .
                if descend branch
                   then List.map recourse
                   else id)
             (Leaf . leafF)
   in  recourse

{- |
Process the subtrees for which the predicate holds.
If the predicate matches subtrees of a matching subtree,
the sub-subtrees are not mapped.
-}
mapSubTrees ::
       (branch -> Bool)
    -> ((branch, [T i branch leaf]) -> (branch, [T i branch leaf]))
    -> (T i branch leaf -> T i branch leaf)
mapSubTrees p f =
   let recourse =
          switch (,)
             (\branch subTrees -> uncurry Branch
                (if p branch
                   then f (branch, subTrees)
                   else (branch, List.map recourse subTrees)))
             Leaf
   in  recourse

filterBranch ::
       (branch -> Bool)
    -> (T i branch leaf -> [T i branch leaf])
filterBranch p =
   foldLabel
      (\i branch subTrees ->
         let jointSubTrees = concat subTrees
         in  if p branch
               then [(i, Branch branch jointSubTrees)]
               else jointSubTrees)
      (\i leaf -> [(i, Leaf leaf)])

fold :: (i -> a -> b)
     -> (branch -> [b] -> a)
     -> (leaf -> a)
     -> (T i branch leaf -> b)
fold iF branchF leafF =
   let recourse =
          switch iF (\x -> branchF x . List.map recourse) leafF
   in  recourse

switch ::
        (i -> a -> b)
     -> (branch -> [T i branch leaf] -> a)
     -> (leaf -> a)
     -> (T i branch leaf -> b)
switch iF branchF leafF (i,n) =
   iF i (switchElem branchF leafF n)

foldLabel ::
        (i -> branch -> [b] -> b)
     -> (i -> leaf -> b)
     -> (T i branch leaf -> b)
foldLabel branchF leafF =
   fold
      (flip ($))
      (\branch subTrees i -> branchF i branch subTrees)
      (\leaf i -> leafF i leaf)

foldLabelAlt ::
        (i -> branch -> [b] -> b)
     -> (i -> leaf -> b)
     -> (T i branch leaf -> b)
foldLabelAlt branchF leafF =
   let recourse =
          switchLabel (\i x -> branchF i x . List.map recourse) leafF
   in  recourse

switchLabel ::
        (i -> branch -> [T i branch leaf] -> b)
     -> (i -> leaf -> b)
     -> (T i branch leaf -> b)
switchLabel branchF leafF (i,n) =
   switchElem (branchF i) (leafF i) n

switchElem ::
        (branch -> [T i branch leaf] -> a)
     -> (leaf -> a)
     -> (Elem i branch leaf -> a)
switchElem branchF _ (Branch x subTrees) =
   branchF x subTrees
switchElem _ leafF (Leaf x) = leafF x

allSubTrees :: T i branch leaf -> [T i branch leaf]
allSubTrees tree =
   tree :
   switch (flip const) (const (concatMap allSubTrees)) (const []) tree



mapA :: Applicative m =>
       (branch0 -> m branch1)
    -> (leaf0 -> m leaf1)
    -> (T i branch0 leaf0 -> m (T i branch1 leaf1))
mapA branchF leafF =
   foldA
      (App.pure . (,))
      (App.liftA Branch . branchF)
      (App.liftA Leaf . leafF)

mapCondA :: Applicative m =>
       (branch -> Bool)
    -> (branch -> m branch)
    -> (leaf -> m leaf)
    -> (T i branch leaf -> m (T i branch leaf))
mapCondA descend branchF leafF =
   let recourse =
          switch
             (App.liftA . (,))
             (\branch -> App.liftA2 Branch (branchF branch) .
                if descend branch
                   then traverse recourse
                   else App.pure)
             (App.liftA Leaf . leafF)
   in  recourse

foldA :: Applicative m =>
        (i -> m (a -> b))
     -> (branch -> m ([b] -> a))
     -> (leaf -> m a)
     -> (T i branch leaf -> m b)
foldA iF branchF leafF =
   let recourse =
          switch (\i x -> iF i <*> x)
             (\x subTrees -> branchF x <*> traverse recourse subTrees)
             leafF
   in  recourse

foldM :: Monad m =>
        (i -> a -> m b)
     -> (branch -> [b] -> m a)
     -> (leaf -> m a)
     -> (T i branch leaf -> m b)
foldM iF branchF leafF =
   let recourse =
          switch ((=<<) . iF)
                   (\x -> branchF x <=< Monad.mapM recourse)
                   leafF
   in  recourse
