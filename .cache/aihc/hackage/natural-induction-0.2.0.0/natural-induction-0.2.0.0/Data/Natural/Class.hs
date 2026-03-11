module Data.Natural.Class where

import Prelude hiding (iterate)
import Data.Functor.Const
import Data.Peano
import Data.Proxy

class Natural n where
    natural :: f Zero -> (∀ m . Natural m => f (Succ m)) -> f n

instance Natural Zero where
    natural zf _ = zf

instance Natural n => Natural (Succ n) where
    natural _ sf = sf

reify :: Natural n => Const Peano n
reify = iterate Succ Zero

iterate :: ∀ n a . Natural n => (a -> a) -> a -> Const a n
iterate f a = natural (Const a) (f' (iterate f a))
  where f' = Const . f . getConst :: ∀ n . Const a n -> Const a (Succ n)
