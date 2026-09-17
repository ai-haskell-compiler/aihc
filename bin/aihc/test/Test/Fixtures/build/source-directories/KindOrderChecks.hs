{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KindOrderChecks (kindOrderChecks) where

-- | The second parameter is phantom, so the checker invents a kind
-- variable for it. The constructor quantifies that kind variable before
-- @q@, and a use site passes the kind first.
data Tagged q a = Tagged Int (Maybe q)

class Walk a where
  walk :: (forall d b. (Walk d) => Tagged d (d -> b) -> d -> Tagged d b) -> (forall g. g -> Tagged Int g) -> a -> Tagged Int a

  -- An explicit forall on the method signature is peeled into the
  -- method scheme, so this default body sees its parameters, and @r@
  -- scopes over the helper's signature.
  count :: forall r. (r -> Int -> r) -> r -> a -> r
  count step start x0 = untag (walk k (\_ -> Tagged 0 Nothing) x0)
    where
      k :: (Walk d) => Tagged d (d -> b) -> d -> Tagged d b
      k (Tagged n q) _ = Tagged (n + 1) q
      untag :: Tagged Int a -> r
      untag (Tagged n _) = step start n

data Leaf = Leaf

instance Walk Leaf where
  walk _ z Leaf = z Leaf

kindOrderChecks :: Bool
kindOrderChecks =
  case Tagged 3 (Just 'x') :: Tagged Char Bool of
    Tagged n (Just c) -> n == 3 && c == 'x' && count (+) 10 Leaf == 10
    Tagged _ Nothing -> False
