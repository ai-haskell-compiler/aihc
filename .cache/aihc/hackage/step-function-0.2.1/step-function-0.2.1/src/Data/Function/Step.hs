{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

{-# LANGUAGE Safe              #-}

module Data.Function.Step (
    -- * Step Function
    -- $setup
    SF (..),
    Bound (..),
    -- * Construction
    constant,
    step,
    fromList,
    -- * Normalisation
    normalise,
    -- * Operators
    (!),
    values,
    -- * Debug
    showSF,
    putSF,
    ) where

import Control.Applicative  (Applicative (pure, (<*>)), liftA2, (<$>))
import Control.DeepSeq      (NFData (..))
import Control.Monad        (ap)
import Data.Functor.Classes
import Data.List            (intercalate)
import Data.Map             (Map)

import Prelude
       (Eq (..), Functor, IO, Maybe (..), Monad (..), Ord (..), Ordering (..),
       Show (..), String, fst, length, map, otherwise, putStrLn, replicate,
       uncurry, ($), (++), (-), (.))

import Data.Foldable    (Foldable, foldr, maximum)
import Data.Monoid      (Monoid (..))
import Data.Semigroup   (Semigroup (..))
import Data.Traversable (Traversable (traverse))

import Text.Show (showListWith)

import qualified Data.Map        as Map
import qualified Test.QuickCheck as QC

-- | Step function. Piecewise constant function, having finitely many pieces.
-- See <https://en.wikipedia.org/wiki/Step_function>.
--
-- @'SF' (fromList [('Open' k1, v1), ('Closed' k2, v2)]) v3 :: 'SF' k v@ describes a piecewise constant function \(f : k \to v\):
--
-- \[
-- f\,x = \begin{cases}
-- v_1, \quad x < k_1 \newline
-- v_2, \quad k_1 \le x \le k_2 \newline
-- v_3, \quad k_2 < x
-- \end{cases}
-- \]
--
-- or as you would write in Haskell
--
-- @
-- f x | x <  k1   = v1
--     | x <= k2   = v2
--     | otherwise = v3
-- @
--
-- /Note:/ [total-map](https://hackage.haskell.org/package/total-map-0.0.6/docs/Data-TotalMap.html) package,
-- which provides /function with finite support/.
--
-- Constructor is exposed as you cannot construct non-valid 'SF'.
--
-- === Merging
--
-- You can use 'Applicative' instance to /merge/ 'SF'.
--
-- >>> putSF $ liftA2 (+) (step 0 0 1) (step 1 0 1)
-- \x -> if
--     | x < 0     -> 0
--     | x < 1     -> 1
--     | otherwise -> 2
--
-- Following property holds, i.e. 'SF' and ordinary function 'Applicative' instances
-- are compatible (and '!' is a homomorphism).
--
-- prop> liftA2 (applyFun2 f) g h ! x == liftA2 (applyFun2 f :: A -> B -> C) (g !) (h !) (x :: Int)
--
-- Recall that for ordinary functions @'liftA2' f g h x = f (g x) (h x)@.
--
-- === Dense?
--
-- This dense variant is useful with [dense ordered](https://en.wikipedia.org/wiki/Dense_order) domains, e.g. 'Rational'.
-- 'Integer' is not dense, so you could use "Data.Function.Step.Discrete" variant instead.
--
-- >>> let s = fromList [(Open 0, -1),(Closed 0, 0)] 1 :: SF Rational Int
-- >>> putSF s
-- \x -> if
--     | x <  0 % 1 -> -1
--     | x <= 0 % 1 -> 0
--     | otherwise  -> 1
--
-- >>> import Data.Ratio ((%))
-- >>> map (s !) [-1, -0.5, 0, 0.5, 1]
-- [-1,-1,0,1,1]
--
data SF k v = SF !(Map (Bound k) v) !v
  deriving (Eq, Ord, Functor, Foldable, Traversable)

-- | Bound operations
data Bound k
    = Open k   -- ^ less-than, @<@
    | Closed k -- ^ less-than-or-equal, @≤@.
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Order is like @'Open' k = (k, False)@, @'Closed' k = (k, True)@.
--
instance Ord k => Ord (Bound k) where
    compare (Open k)   (Open k')   = compare k k'
    compare (Closed k) (Closed k') = compare k k'
    compare (Open k)   (Closed k') = case compare k k' of
        LT -> LT
        EQ -> LT
        GT -> GT
    compare (Closed k) (Open k')   = case compare k k' of
        LT -> LT
        EQ -> GT
        GT -> GT

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- | 'pure' is a constant function.
instance Ord k => Applicative (SF k) where
    pure  = constant
    (<*>) = ap

instance Ord k => Monad (SF k) where
    return = pure

    SF m def0 >>= f = SF
        (Map.fromDistinctAscList $ mkDistinctAscList $ pieces ++ pieces1)
        def1
      where
        pieces =
            [ (min k k', v')
            | (k, v) <- Map.toList m
            , let SF m' def = f v
            , (k', v') <- Map.toList m' ++ [(k, def)]
            ]
        (pieces1, def1) = let SF m' def = f def0 in (Map.toList m', def)

-- | Piecewise '<>'.
--
-- >>> putSF $ step 0 "a" "b" <> step 1 "c" "d"
-- \x -> if
--     | x < 0     -> "ac"
--     | x < 1     -> "bc"
--     | otherwise -> "bd"
--
instance (Ord k, Semigroup v) => Semigroup (SF k v) where
    (<>) = liftA2 (<>)

instance (Ord k, Monoid v) => Monoid (SF k v) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance (Ord k, QC.Arbitrary k, QC.Arbitrary v) => QC.Arbitrary (SF k v) where
    arbitrary = fromList <$> QC.arbitrary <*> QC.arbitrary
    shrink (SF m v) = uncurry fromList <$> QC.shrink (Map.toList m, v)

instance QC.Arbitrary k => QC.Arbitrary (Bound k) where
    arbitrary = QC.oneof [Open <$> QC.arbitrary, Closed <$> QC.arbitrary]

instance NFData k => NFData (Bound k) where
    rnf (Open k) = rnf k
    rnf (Closed k) = rnf k

instance (NFData k, NFData v) => NFData (SF k v) where
    rnf (SF m v) = rnf (m, v)

-------------------------------------------------------------------------------
-- Show
-------------------------------------------------------------------------------

instance Show2 SF where
    liftShowsPrec2 spk slk spv slv d (SF m v) = showsBinaryWith
        (\_ -> showListWith $ liftShowsPrec2 (liftShowsPrec spk slk) (liftShowList spk slk) spv slv 0)
        spv
        "fromList" d (Map.toList m) v

instance Show k => Show1 (SF k) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show k, Show v) => Show (SF k v) where
    showsPrec = showsPrec2

instance Show1 Bound where
    liftShowsPrec sp _ d (Open k)   = showsUnaryWith sp "Open"   d k
    liftShowsPrec sp _ d (Closed k) = showsUnaryWith sp "Closed" d k

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

mkDistinctAscList :: Ord k => [(k, b)] -> [(k, b)]
mkDistinctAscList []            = []
mkDistinctAscList ((k, v) : kv) = (k, v) : mkDistinctAscList' k kv

mkDistinctAscList' :: Ord k => k -> [(k, b)] -> [(k, b)]
mkDistinctAscList' _ [] = []
mkDistinctAscList' k (p@(k', _) : kv)
    | k < k'    = p : mkDistinctAscList' k' kv
    | otherwise =     mkDistinctAscList' k  kv

-------------------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------------------

infixl 9 !

-- | Apply 'SF'.
--
-- >>> heaviside ! 2
-- 1
(!) :: Ord k => SF k v -> k -> v
SF m def ! x = case Map.lookupGE (Closed x) m of
    Nothing     -> def
    Just (_, v) -> v

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Constant function
--
-- >>> putSF $ constant 1
-- \_ -> 1
--
constant :: a -> SF k a
constant = SF Map.empty

-- | Step function.
--
-- @'step' k v1 v2 = \\ x -> if x < k then v1 else v2@.
--
-- >>> putSF $ step 1 2 3
-- \x -> if
--     | x < 1     -> 2
--     | otherwise -> 3
--
step :: k -> v -> v -> SF k v
step k = SF . Map.singleton (Open k)

-- | Create function from list of cases and default value.
--
-- >>> let f = fromList [(Open 1,2),(Closed 3,4),(Open 4,5)] 6
-- >>> putSF f
-- \x -> if
--     | x <  1    -> 2
--     | x <= 3    -> 4
--     | x <  4    -> 5
--     | otherwise -> 6
--
-- >>> map (f !) [0..10]
-- [2,4,4,4,6,6,6,6,6,6,6]
--
fromList :: Ord k => [(Bound k, v)] -> v -> SF k v
fromList = SF . Map.fromList

-------------------------------------------------------------------------------
-- Conversions to/from list
-------------------------------------------------------------------------------

-- | Possible values of 'SF'
--
-- >>> values heaviside
-- [-1,1]
--
values :: SF k v -> [v]
values (SF m v) = Map.elems m ++ [v]

-------------------------------------------------------------------------------
-- Normalise
-------------------------------------------------------------------------------

-- | Merge adjustent pieces with same values.
--
-- /Note:/ 'SF' isn't normalised on construction.
-- Values don't necessarily are 'Eq'.
--
-- >>> putSF $ normalise heaviside
-- \x -> if
--     | x < 0     -> -1
--     | otherwise -> 1
--
-- >>> putSF $ normalise $ step 0 1 1
-- \_ -> 1
--
-- prop> normalise (liftA2 (+) p (fmap negate p)) == (pure 0 :: SF Int Int)
--
normalise :: Eq v => SF k v -> SF k v
normalise (SF m v) = uncurry mk $ foldr go ([], v) (Map.toList m) where
    mk m' _ = SF (Map.fromDistinctAscList m') v

    go p@(_, v') p'@(m', x)
        | v' == x   = p'
        | otherwise = (p : m', v')

-------------------------------------------------------------------------------
-- Pretty-printing
-------------------------------------------------------------------------------

-- | Show 'SF' as Haskell code
showSF :: (Show a, Show b) => SF a b -> String
showSF (SF m v) | Map.null m = "\\_ -> " ++ show v
showSF (SF m v) = intercalate "\n" $
    "\\x -> if" : [ "    | " ++ leftPad k ++ " -> " ++ x | (k, x) <- cases ]
  where
    cases     = cases' ++ [ ("otherwise", show v) ]

    m' = Map.toList m

    cases' = case traverse fromOpen m' of
        Nothing  -> [ ("x " ++ showBound k, show x) | (k, x) <- m' ]
        Just m'' -> [ ("x < " ++ show k,    show x) | (k, x) <- m'' ]

    fromOpen (Open k, x) = Just (k, x)
    fromOpen _           = Nothing

    len       = maximum (map (length . fst) cases)
    leftPad s = s ++ replicate (len - length s) ' '

showBound :: Show k => Bound k -> String
showBound (Open k)   = "<  " ++ showsPrec 5 k ""
showBound (Closed k) = "<= " ++ showsPrec 5 k ""

-- | @'putStrLn' . 'showSF'@
putSF :: (Show a, Show b) => SF a b -> IO ()
putSF = putStrLn . showSF

-- $setup
--
-- >>> import Test.QuickCheck (applyFun2)
-- >>> import Test.QuickCheck.Poly (A, B, C)
-- >>> import Control.Applicative (liftA2, pure)
-- >>> import Data.Semigroup (Semigroup (..))
--
-- == Examples
--
-- >>> let heaviside = step 0 (-1) 1 :: SF Int Int
-- >>> putSF heaviside
-- \x -> if
--     | x < 0     -> -1
--     | otherwise -> 1
--
-- >>> map (heaviside !) [-3, 0, 4]
-- [-1,1,1]
