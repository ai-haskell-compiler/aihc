{- |
We provide a way to specify a set of columns
that matches the tuple structure of a certain graph type.
-}
module Graphics.Gnuplot.Value.ColumnSet
   (T(Cons), atom, pair, triple, quadruple,
   ) where

import qualified Graphics.Gnuplot.Value.Atom  as Atom


newtype T a = Cons [Int]

{-
Functor and Applicative instances would be useful
for combining column sets,
but they are dangerous, because they can bring
type and column number out of sync.

instance Functor Column where
   fmap _ (Cons n) = Cons n

instance Applicative Column where
   pure _ = Cons []
   Cons ns <*> Cons ms = Cons (ns++ms)
-}

atom :: Atom.C a => Int -> T a
atom i = Cons [i]

pair :: T a -> T b -> T (a,b)
pair (Cons ai) (Cons bi) = Cons (ai++bi)

triple :: T a -> T b -> T c -> T (a,b,c)
triple (Cons ai) (Cons bi) (Cons ci) = Cons (ai++bi++ci)

quadruple :: T a -> T b -> T c -> T d -> T (a,b,c,d)
quadruple (Cons ai) (Cons bi) (Cons ci) (Cons di) = Cons (ai++bi++ci++di)
