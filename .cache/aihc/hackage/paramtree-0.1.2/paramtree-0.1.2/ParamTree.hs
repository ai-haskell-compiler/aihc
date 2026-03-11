{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  ParamTree
-- Copyright   :  (C) 2017-2023 Merijn Verstraaten
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Merijn Verstraaten <merijn@inconsistent.nl>
-- Stability   :  experimental
-- Portability :  haha
--
-- Easily generate a labelled tree of tests/benchmarks from a generation
-- function and sets of parameters to use for each of that functions arguments.
-- Example usecases include criterion benchmark trees or tasty test trees.
-------------------------------------------------------------------------------
module ParamTree
    ( Params
    , ParamFun
    , growTree
    , simpleParam
    , derivedParam
    , displayParam
    , customParam
    , paramSets
    ) where

import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Endo(..))

-- | Type family that converts a type level list into a function type:
--
-- @'ParamFun' ['Char', 'Int', 'Bool'] r@ =
-- @'Char' -> 'Int' -> 'Bool' -> 'String' -> r@
type family ParamFun (l :: [Type]) r where
    ParamFun '[] r = String -> r
    ParamFun (h ': t) r = h -> ParamFun t r

-- | Sets of parameters to generate the tree from.
data Params :: [Type] -> Type where
    Nil :: Params '[]
    Sets :: [Params l] -> Params l
    Param :: Eq r
          => (a -> String) -- Display parameter
          -> (a -> r)      -- Derive value from parameter
          -> String        -- Parameter name
          -> [a]           -- Parameter values
          -> Params l
          -> Params (r ': l)

data Tree :: [Type] -> Type where
    None :: Tree '[]
    Empty :: Tree l
    Grouped :: Eq r
            => Map (String, String) [(r, Tree l)]
            -> Tree (r ': l)

-- | A simple parameter set. The tree label is a combination of 'show'ing the
-- value and the parameter name.
simpleParam
    :: (Eq a, Show a)
    => String -- ^ Name of the parameter
    -> [a] -- ^ Set of values to use
    -> Params l
    -> Params (a ': l)
simpleParam = Param show id

-- | A derived parameter set. Useful when the input expected by your function
-- can't be conveniently rendered as a string label. For example:
--
-- @'derivedParam' ('enumFromTo' 0) \"My Parameter\" [1,2,5]@
--
-- The above passed @'enumFromTo' 0 1@, @'enumFromTo' 0 2@, etc. to your
-- function, while labelling them as \"1 My Parameter\" and \"2 My Parameter\"
-- respectively.
derivedParam
    :: (Eq r, Show a)
    => (a -> r) -- ^ Parameter derivation function
    -> String -- ^ Name of the parameter
    -> [a] -- ^ Set of values to derive from
    -> Params l
    -> Params (r ': l)
derivedParam f = Param show f

-- | A simple parameter set with a more flexible way of showing values,
-- 'simpleParam' is equivalent to @displayParam show@.
displayParam
    :: Eq a
    => (a -> String)
    -> String
    -> [a]
    -> Params l
    -> Params (a ': l)
displayParam display = Param display id

-- | A completely customisable parameter set, allows specification of how to
-- display values and how to derive values. Equivalencies:
--
-- @'simpleParam' = 'customParam' 'show' 'id'@
--
-- @'derivedParam' = 'customParam' 'show'@
--
-- @'displayParam' = \\f -> 'customParam' f 'id'@
customParam
    :: Eq r
    => (a -> String)
    -> (a -> r)
    -> String
    -> [a]
    -> Params l
    -> Params (r ': l)
customParam = Param

-- | Combine multiple sets of parameters into one. Allows a limited amount of
-- control over which combinations appear. For example:
--
-- @
-- 'paramSets'
--     [ 'simpleParam' \"Bool\" [True] . 'simpleParam' \"Char\" \"xy\"
--     , 'simpleParam' \"Bool\" [True,False] . 'simpleParam' \"Char\" \"a\"
--     ]
-- @
--
-- The result is \"axy\" being used in groups where the \"Bool\" parameter is
-- @True@, if the \"Bool\" parameter is @False@ only \"a\" is used.
paramSets :: [Params r -> Params l] -> Params r -> Params l
paramSets prefixes rest = Sets $ map ($ rest) prefixes

trim :: [Tree l] -> Tree l
trim [] = Empty
trim (None:_) = None
trim (Empty:l) = trim l
trim l@(Grouped{}:_) = Grouped . M.unionsWith fuse $ map explode l
  where
    explode :: Tree (h ': t) -> Map (String, String) [(h, Tree t)]
    explode Empty = M.empty
    explode (Grouped m) = m

sprout :: Params l -> Tree l
sprout Nil = None
sprout (Sets l) = trim $ map sprout l
sprout (Param display derive name values remainder) =
    Grouped . M.fromListWith fuse . map convert $ values
  where
    convert x = ((display x, name), [(derive x, sprout remainder)])

fuse :: Eq x => [(x, Tree l)] -> [(x, Tree l)] -> [(x, Tree l)]
fuse = appEndo . mconcat . map (Endo . insert)
  where
    insert (x, params) [] = [(x, params)]
    insert new@(x1, params1) ((x2, params2):l)
        | x1 == x2 = (x1, trim [params1, params2]):l
        | otherwise = (x2, params2) : insert new l

-- | Generate a tree from a function that produces a leaf and sets of
-- parameters. Useful for generating tasty TestTrees or criterion benchmark
-- trees from a function and a set of parameter. For example:
--
-- @
-- import Test.Tasty
-- import Test.Tasty.HUnit
--
-- genTestCase :: Int -> Bool -> Char -> String -> TestTree
--
-- params = 'simpleParam' \"Int\" [1,2]
--        . 'simpleParam' \"Bool\" [True]
--        . 'simpleParam' \"Char\" "xyz"
--
-- main :: IO ()
-- main = defaultMain $ testTree genTestCase params
--   where
--     testTree = growTree (Just "/") testGroup "my tests"
-- @
--
-- This generates a tasty TestTree with all combinations of values passed to
-- @genTestCase@. If the 'Maybe' 'String' argument is provided like in the
-- above example, groups with a single entry, such as \"Bool\" get collapsed
-- into their parent groups. So instead of a \"1 Int\" group containing a
-- \"True Bool\" group they get collapsed into a single \"1 Int/True Bool\"
-- group, where the \"/\" separator is the one specified by @'Just' \"\/\"@
growTree
    :: forall a l
     . Maybe String -- ^ Groups containing a single entry are skipped and their
                    -- label is appended to their child, separated by this
                    -- 'String' if not 'Nothing'.
    -> (String -> [a] -> a) -- ^ Tree labelling function, e.g. tasty's
                            -- @testGroup@ or criterion's @bgroup@
    -> String -- ^ Label for the root of the tree
    -> ParamFun l a -- ^ Function that produces leafs, such as tasty tests or
                    -- criterion benchmarks
    -> (Params '[] -> Params l) -- ^ Parameter sets to grow tree from
    -> a
growTree collapse labelFun label fun params =
    go (sprout $ params Nil) fun label
  where
    go :: Tree k -> ParamFun k a -> String -> a
    go None result lbl = result lbl
    go Empty _ lbl = labelFun lbl []
    go (Grouped l) f lbl = case concatMap flatten (M.toList l) of
        [x] | Just sep <- collapse -> buildBranch f (\n -> lbl ++ sep ++ n)  x
        branches -> labelFun lbl $ map (buildBranch f id) branches

    flatten :: ((String, String), [(h, Tree t)]) -> [(String, h, Tree t)]
    flatten ((param, name), rest) = map (\(v, r) -> (nextLabel, v, r)) rest
      where
        nextLabel | null name = param
                  | otherwise = param ++ " " ++ name

    buildBranch
        :: ParamFun (h ': t) a
        -> (String -> String)
        -> (String, h, Tree t)
        -> a
    buildBranch f namer (name, val, rest) = go rest (f val) (namer name)
