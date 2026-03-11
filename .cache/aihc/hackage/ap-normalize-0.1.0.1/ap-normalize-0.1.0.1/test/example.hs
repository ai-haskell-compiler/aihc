{-# OPTIONS_GHC -dsuppress-all #-}
{-# LANGUAGE TemplateHaskell #-}

-- Testing example from the documentation

import Control.Applicative (liftA2)
import Test.Inspection
import ApNormalize

data Example a = Example a Bool [a] (Example a)

traverseNaive :: Applicative f => (a -> f b) -> Example a -> f (Example b)
traverseNaive go (Example a b c d) =
  Example
    <$> go a
    <*> pure b
    <*> traverse go c
    <*> traverseNaive go d
  -- Total: 1 <$>, 3 <*>

traverseAN :: Applicative f => (a -> f b) -> Example a -> f (Example b)
traverseAN go (Example a b c d) =
  Example
    <$>^ go a
    <*>  pure b
    <*>^ traverse go c
    <*>^ traverseAN go d
    & lowerAps
  -- Total: 1 <$>, 3 <*>

traverseNormal :: Applicative f => (a -> f b) -> Example a -> f (Example b)
traverseNormal go (Example a b c d) =
  liftA2 (\a' -> Example a' b)
    (go a)
    (traverse go c)
    <*> traverseNormal go d
  -- Total: 1 liftA2, 1 <*>

traverseTree :: Applicative f => (a -> f b) -> Example a -> f (Example b)
traverseTree go (Example a b c d) =
  (\((a', b'), (c', d')) -> Example a' b' c' d')
    <$> ((,) <$> ((,) <$>^ go a
                      <*>  pure b)
             <*> ((,) <$>^ traverse go c
                      <*>^ traverseTree go d))
    & lowerAps
  -- 4 \<$\>, 3 \<*\>

inspect $ 'traverseNormal =/= 'traverseNaive
inspect $ 'traverseNormal === 'traverseAN
inspect $ 'traverseNormal === 'traverseTree

-- dummy
main :: IO ()
main = pure ()
