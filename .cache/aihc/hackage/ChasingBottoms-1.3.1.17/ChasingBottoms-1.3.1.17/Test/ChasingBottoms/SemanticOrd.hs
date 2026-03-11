{-# LANGUAGE ScopedTypeVariables, RankNTypes,
             FlexibleInstances, UndecidableInstances,
             MonoLocalBinds #-}

-- |
-- Module      :  Test.ChasingBottoms.SemanticOrd
-- Copyright   :  (c) Nils Anders Danielsson 2004-2022, 2024-2025
-- License     :  See the file LICENCE.
--
-- Maintainer  :  http://www.cse.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (GHC-specific)
--
-- Generic semantic equality and order. The semantic order referred
-- to is that of a typical CPO for Haskell types, where e.g. @('True',
-- 'bottom') '<=!' ('True', 'False')@, but where @('True', 'True')@
-- and @('True', 'False')@ are incomparable.
--
-- The implementation is based on 'isBottom', and has the same
-- limitations. Note that non-bottom functions are not handled by any
-- of the functions described below.
--
-- One could imagine using QuickCheck for testing equality of
-- functions, but I have not managed to tweak the type system so that
-- it can be done transparently.

module Test.ChasingBottoms.SemanticOrd
  ( Tweak(..)
  , noTweak
  , SemanticEq(..)
  , SemanticOrd(..)
  ) where

import Data.Generics
import Test.ChasingBottoms.IsBottom
import Test.ChasingBottoms.IsType
import qualified Data.Maybe as Maybe
import Test.ChasingBottoms.Nat
import Test.ChasingBottoms.Approx

infix 4 <!, <=!, ==!, >=!, >!, /=!
infix 5 \/!
infixl 5 /\!

-- | The behaviour of some of the functions below can be tweaked.

data Tweak = Tweak
  { approxDepth :: Maybe Nat
    -- ^ If equal to @'Just' n@, an @'approxAll' n@ is performed on
    -- all arguments before doing whatever the function is supposed to
    -- be doing.
  , timeOutLimit :: Maybe Int
    -- ^ If equal to @'Just' n@, then all computations that take more
    -- than @n@ seconds to complete are considered to be equal to
    -- 'bottom'. This functionality is implemented using
    -- 'isBottomTimeOut'.
  }
  deriving (Eq, Ord, Show)

-- | No tweak (both fields are 'Nothing').

noTweak :: Tweak
noTweak = Tweak
  { approxDepth  = Nothing
  , timeOutLimit = Nothing
  }

-- | 'SemanticEq' contains methods for testing whether two terms are
-- semantically equal.

-- Note that we only allow a -> a -> Bool here, not a -> b ->
-- Bool. Otherwise we would allow behaviour like the following:
--   > (bottom : bottom :: [Int]) <=!! ("tr" :: String)
--   True

class SemanticEq a where
  (==!), (/=!) :: a -> a -> Bool
  semanticEq :: Tweak -> a -> a -> Bool

  (/=!) = \x y -> not (x ==! y)
  (==!) = semanticEq noTweak

-- | 'SemanticOrd' contains methods for testing whether two terms are
-- related according to the semantic domain ordering.

class SemanticEq a => SemanticOrd a where
  (<!), (<=!), (>=!), (>!) :: a -> a -> Bool

  semanticCompare :: Tweak -> a -> a -> Maybe Ordering
  -- ^ @'semanticCompare' tweak x y@ returns 'Nothing' if @x@ and @y@ are
  -- incomparable, and @'Just' o@ otherwise, where @o :: 'Ordering'@
  -- represents the relation between @x@ and @y@.

  (\/!) :: a -> a -> Maybe a
  (/\!) :: a -> a -> a
  semanticJoin :: Tweak -> a -> a -> Maybe a
  semanticMeet :: Tweak -> a -> a -> a
  -- ^ @x '\/!' y@ and @x '/\!' y@ compute the least upper and greatest
  -- lower bounds, respectively, of @x@ and @y@ in the semantical
  -- domain ordering. Note that the least upper bound may not always
  -- exist.
  -- This functionality was implemented just because it was
  -- possible (and to provide analogues of 'max' and 'min' in the 'Ord'
  -- class). If anyone finds any use for it, please let me know.

  (>=!) = flip (<=!)
  (<!)  = \x y -> x <=! y && x /=! y
  (>!)  = \x y -> x >=! y && x /=! y

  x <=! y = case semanticCompare noTweak x y of
    Just LT -> True
    Just EQ -> True
    _       -> False

  (\/!) = semanticJoin noTweak
  (/\!) = semanticMeet noTweak

instance Data a => SemanticEq a where
  semanticEq tweak = liftAppr tweak semanticEq'

instance Data a => SemanticOrd a where
  semanticCompare tweak = liftAppr tweak semanticCompare'
    where
    semanticCompare' tweak x y =
      case ( semanticEq' tweak x y
           , semanticLE' tweak x y
           , semanticLE' tweak y x ) of
        (True,  _,    _)    -> Just EQ
        (_,     True, _)    -> Just LT
        (_,     _,    True) -> Just Prelude.GT
        (_,     _,    _)    -> Nothing
  semanticJoin tweak    = liftAppr tweak semanticJoin'
  semanticMeet tweak    = liftAppr tweak semanticMeet'

liftAppr :: (Data a, Data b) => Tweak -> (Tweak -> a -> a -> b) -> a -> a -> b
liftAppr tweak op x y = op tweak (appr x) (appr y)
  where appr = maybe id approxAll (approxDepth tweak)

------------------------------------------------------------------------

type Rel' = forall a b. (Data a, Data b) => Tweak -> a -> b -> Bool
type Rel  = forall a b. (Data a, Data b) => a -> b -> Bool

semanticEq', semanticLE' :: Rel'

semanticEq' tweak a b = case ( isBottomTimeOut (timeOutLimit tweak) a
                             , isBottomTimeOut (timeOutLimit tweak) b ) of
  (True, True)   -> True
  (False, False) -> allOK (semanticEq' tweak) a b
  _              -> False

semanticLE' tweak a b = case ( isBottomTimeOut (timeOutLimit tweak) a
                             , isBottomTimeOut (timeOutLimit tweak) b ) of
  (True, _)      -> True
  (False, False) -> allOK (semanticLE' tweak) a b
  _              -> False

allOK :: Rel -> Rel
allOK op a b =
  -- It's really enough to test just a, since we restrict the types
  -- above, but why complicate things?
  if isFunction a || isFunction b then
    -- cast' a `fop` cast' b
    nonBottomError
      "The generic versions of (==!) and friends do not accept non-bottom \
      \functions."
   else
    a =^= b && childrenOK op a b

-- Check top-level. Note that this test always fails for "function
-- constructors".
(=^=) :: Rel
a =^= b = toConstr a == toConstr b

-- Check children.
childrenOK :: Rel -> Rel
childrenOK op x y = and (gzipWithQ (\x y -> op x y) x y)

------------------------------------------------------------------------

semanticMeet' :: (Data a, Data b) => Tweak -> a -> b -> b
semanticMeet' tweak a (b :: b) =
  if isBottomTimeOut (timeOutLimit tweak) a ||
     isBottomTimeOut (timeOutLimit tweak) b then
    bottom
   else if isFunction a || isFunction b then
    nonBottomError "/\\! does not handle non-bottom functions."
   else if not (a =^= b) then
    bottom
   else
    gzipWithT (\x y -> semanticMeet' tweak x y) a b

semanticJoin' :: (Data a, Data b) => Tweak -> a -> b -> Maybe b
semanticJoin' tweak a (b :: b) =
  case ( isBottomTimeOut (timeOutLimit tweak) a
       , isBottomTimeOut (timeOutLimit tweak) b ) of
    (True,  True)  -> Just bottom
    (True,  False) -> Just b
    (False, True)  -> cast a
    (False, False)
      | isFunction a || isFunction b ->
          nonBottomError "\\/! does not handle non-bottom functions."
      | not (a =^= b) -> Nothing
      | otherwise     -> gzipWithM (\x y -> semanticJoin' tweak x y) a b

------------------------------------------------------------------------

-- Variant of cast.

-- cast' :: (Typeable a, Typeable b) => a -> b
-- cast' = Maybe.fromJust . cast

------------------------------------------------------------------------

-- TODO: Implement a comparison operator which also works for functions.

-- newtype EqFun = EqFun { unEqFun ::
--   forall a b . (Data a, Data b) => a -> b -> Bool }

-- class SemanticFunEq a where
--   (!==!), (!/=!) :: a -> a -> Bool

--   (!/=!) = \x y -> not (x !==! y)

-- instance Data a => SemanticFunEq a where
--  x !==! y =
--   let test :: (Arbitrary b, Show b, Data c) =>
--               (b -> c1) -> (b -> c2) -> Bool
--       test f g = testIt (forAll arbitrary $ \(x :: b) -> f x !==!! g x)
--   in let ?funTest = EqFun test
--   in x !==!! y

-- (!==!!) :: (Data a, Data b, ?funTest :: EqFun) => a -> b -> Bool
-- x !==!! y = case (isBottom x, isBottom y) of
--   (True, True)   -> True
--   (False, False) | isFunction x -> unEqFun ?funTest x y
--                  | otherwise -> x =^= y && tmapQl (&&) True (!==!!) x y
--   _              -> False

-- This one works, but it only handles functions on the top level, not
-- functions inside e.g. lists.

-- instance (Show a, Arbitrary a, SemanticFunEq b) => SemanticFunEq (a -> b) where
--   f !==! g = case (isBottom f, isBottom g) of
--     (True, True)   -> True
--     (False, False) -> testIt (forAll arbitrary $ \x -> f x !==! g x)
--     _              -> False

-- instance SemanticEq a => SemanticFunEq a where
--   a !==! b = case (isBottom a, isBottom b) of
--     (True, True)   -> True
--     (False, False) -> -- We know that we are not dealing with functions.
--                       a ==! b
--     _              -> False
