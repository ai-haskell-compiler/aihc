module Control.Alternative.Vector where

import Data.Vector (Vector, cons, empty)
import Control.Applicative (Alternative ((<|>)))


someV :: Alternative f => f a -> f (Vector a)
someV v = cons <$> v <*> manyV v


manyV :: Alternative f => f a -> f (Vector a)
manyV v = someV v <|> pure empty
