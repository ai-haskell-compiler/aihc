module Data.ApplicativeChain where

import Control.Applicative (Applicative(pure, (<*>)), )


data T a = Cons {runAll :: RunAll, result :: a}

data RunAll = RunAll
   deriving Show

instance Functor T where
   fmap f ~(Cons as a) = Cons as $ f a

instance Applicative T where
   pure = Cons RunAll
   Cons fs f <*> a =
      fmap f $ case fs of RunAll -> a
