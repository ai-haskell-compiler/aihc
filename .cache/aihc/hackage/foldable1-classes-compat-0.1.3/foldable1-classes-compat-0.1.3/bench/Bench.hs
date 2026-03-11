{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

#if MIN_VERSION_base(4,18,0)
# define HAS_FOLDABLE1_CONTAINERS MIN_VERSION_containers(0,6,7)
#else
# define HAS_FOLDABLE1_CONTAINERS 1
#endif

module Main (main) where

import Prelude hiding (foldl1, head, last, maximum)

import Control.DeepSeq    (NFData (..))
import qualified Data.Foldable as F (Foldable)
import Data.Foldable1
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup     (Min (..))
import Test.Tasty.Bench

#if HAS_FOLDABLE1_CONTAINERS
import Data.Tree          (Tree (..))
#endif

input :: NonEmpty Int
input = 1 :| take 10000000 [2 .. ]

#if HAS_FOLDABLE1_CONTAINERS
tree :: Tree Int
tree = go 7 0 where
    go :: Int -> Int -> Tree Int
    go n x
        | n <= 0    = Node x []
        | otherwise = Node x [ go (pred n) (x * 10 + x') | x' <- [0 .. 9] ]
#endif

main :: IO ()
main = defaultMain
    -- NonEmpty left folds
    [ env (return input) $ \ne -> bgroup "NonEmpty-vanilla"
        [ bench "foldMap1 Min"      $ whnf (getMin . foldMap1 Min) ne
        , bench "foldMap1' Min"     $ whnf (getMin . foldMap1' Min) ne
        , bench "foldl1' min"       $ whnf (foldl1' min) ne
        , bench "foldl1 min"        $ whnf (foldl1 min) ne
        , bench "foldlMap1' id min" $ whnf (foldlMap1' id min) ne
        , bench "foldlMap1 id min"  $ whnf (foldlMap1 id min) ne
        ]
    , env (return $ NE1 input) $ \ne -> bgroup "NonEmpty-foldMap1"
        [ bench "foldMap1 Min"      $ whnf (getMin . foldMap1 Min) ne
        , bench "foldMap1' Min"     $ whnf (getMin . foldMap1' Min) ne
        , bench "foldl1' min"       $ whnf (foldl1' min) ne
        , bench "foldl1 min"        $ whnf (foldl1 min) ne
        , bench "foldlMap1' id min" $ whnf (foldlMap1' id min) ne
        , bench "foldlMap1 id min"  $ whnf (foldlMap1 id min) ne
        ]
    , env (return $ NE3 input) $ \ne -> bgroup "NonEmpty-foldrMap1"
        [ bench "foldMap1 Min"      $ whnf (getMin . foldMap1 Min) ne
        , bench "foldMap1' Min"     $ whnf (getMin . foldMap1' Min) ne
        , bench "foldl1' min"       $ whnf (foldl1' min) ne
        , bench "foldl1 min"        $ whnf (foldl1 min) ne
        , bench "foldlMap1' id min" $ whnf (foldlMap1' id min) ne
        , bench "foldlMap1 id min"  $ whnf (foldlMap1 id min) ne
        ]

#if HAS_FOLDABLE1_CONTAINERS
    -- Trees
    , env (return tree) $ \tr -> bgroup "Tree-vanilla"
        [ bench "head" $ whnf head tr
        , bench "last" $ whnf last tr
        , bench "maximum" $ whnf maximum tr
        , bench "maximum'" $ whnf (foldl1' max) tr

        , bench "foldMap1 Min"      $ whnf (getMin . foldMap1 Min) tr
        , bench "foldMap1' Min"     $ whnf (getMin . foldMap1' Min) tr
        , bench "foldl1' min"       $ whnf (foldl1' min) tr
        , bench "foldl1 min"        $ whnf (foldl1 min) tr
        , bench "foldlMap1' id min" $ whnf (foldlMap1' id min) tr
        , bench "foldlMap1 id min"  $ whnf (foldlMap1 id min) tr
        ]
    , env (return $ Tree1 tree) $ \tr -> bgroup "Tree-foldMap1"
        [ bench "head" $ whnf head tr
        , bench "last" $ whnf last tr
        , bench "maximum" $ whnf maximum tr
        , bench "maximum'" $ whnf (foldl1' max) tr

        , bench "foldMap1 Min"      $ whnf (getMin . foldMap1 Min) tr
        , bench "foldMap1' Min"     $ whnf (getMin . foldMap1' Min) tr
        , bench "foldl1' min"       $ whnf (foldl1' min) tr
        , bench "foldl1 min"        $ whnf (foldl1 min) tr
        , bench "foldlMap1' id min" $ whnf (foldlMap1' id min) tr
        , bench "foldlMap1 id min"  $ whnf (foldlMap1 id min) tr
        ]
    , env (return $ Tree3 tree) $ \tr -> bgroup "Tree-foldr1Map"
        [ bench "head" $ whnf head tr
        , bench "last" $ whnf last tr
        , bench "maximum" $ whnf maximum tr
        , bench "maximum'" $ whnf (foldl1' max) tr

        , bench "foldMap1 Min"      $ whnf (getMin . foldMap1 Min) tr
        , bench "foldMap1' Min"     $ whnf (getMin . foldMap1' Min) tr
        , bench "foldl1' min"       $ whnf (foldl1' min) tr
        , bench "foldl1 min"        $ whnf (foldl1 min) tr
        , bench "foldlMap1' id min" $ whnf (foldlMap1' id min) tr
        , bench "foldlMap1 id min"  $ whnf (foldlMap1 id min) tr
        ]
#endif
    ]

-------------------------------------------------------------------------------
-- NonEmpty variants
-------------------------------------------------------------------------------

-- Using foldMap1 only
newtype NE1 a = NE1 (NonEmpty a)
  deriving (Functor, F.Foldable)

instance NFData a => NFData (NE1 a) where
    rnf (NE1 xs) = rnf xs

instance Foldable1 NE1 where
    foldMap1 f (NE1 xs) = foldMap1 f xs

-- Using toNonEmpty
-- newtype NE2 a = NE2 (NonEmpty a)
--   deriving (Functor, F.Foldable)
--
-- instance NFData a => NFData (NE2 a) where
--     rnf (NE2 xs) = rnf xs
--
-- instance Foldable1 NE2 where
--     toNonEmpty (NE2 xs) = toNonEmpty xs

-- Using to foldrMap1
newtype NE3 a = NE3 (NonEmpty a)
  deriving (Functor, F.Foldable)

instance NFData a => NFData (NE3 a) where
    rnf (NE3 xs) = rnf xs

instance Foldable1 NE3 where
    foldrMap1 g f (NE3 xs) = foldrMap1 g f xs

#if HAS_FOLDABLE1_CONTAINERS
-------------------------------------------------------------------------------
-- Tree variants
-------------------------------------------------------------------------------

-- Using foldMap1 only
newtype Tree1 a = Tree1 (Tree a)
  deriving (Functor, F.Foldable)

instance NFData a => NFData (Tree1 a) where
    rnf (Tree1 xs) = rnf xs

instance Foldable1 Tree1 where
    foldMap1 f (Tree1 xs) = foldMap1 f xs

-- Using toNonEmpty
-- newtype Tree2 a = Tree2 (Tree a)
--   deriving (Functor, F.Foldable)
--
-- instance NFData a => NFData (Tree2 a) where
--     rnf (Tree2 xs) = rnf xs
--
-- instance Foldable1 Tree2 where
--     toNonEmpty (Tree2 xs) = toNonEmpty xs

-- Using to foldrMap1
newtype Tree3 a = Tree3 (Tree a)
  deriving (Functor, F.Foldable)

instance NFData a => NFData (Tree3 a) where
    rnf (Tree3 xs) = rnf xs

instance Foldable1 Tree3 where
    foldrMap1 f g (Tree3 xs) = foldrMap1 f g xs
#endif
