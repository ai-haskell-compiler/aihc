{-# LANGUAGE ScopedTypeVariables,
             FlexibleInstances, UndecidableInstances #-}

-- |
-- Module      :  Test.ChasingBottoms.ApproxShow
-- Copyright   :  (c) Nils Anders Danielsson 2004-2022, 2024-2025
-- License     :  See the file LICENCE.
--
-- Maintainer  :  http://www.cse.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (GHC-specific)
--
-- Functions for converting arbitrary (non-function, partial,
-- possibly infinite) values into strings.

module Test.ChasingBottoms.ApproxShow
  ( Prec
  , ApproxShow(..)
  ) where

import Data.Generics
import Test.ChasingBottoms.IsBottom
import Test.ChasingBottoms.Nat
import Test.ChasingBottoms.IsType
import qualified Data.List as List

-- | Precedence level.
type Prec = Int

class ApproxShow a where
  -- | The 'Data' instance of 'ApproxShow' makes sure that
  -- @'approxShowsPrec' n@ behaves (more or less) like the derived
  -- version of 'showsPrec', with the following differences:
  --
  --   * After @n@ levels of descent into a term the output is
  --     replaced by @\"_\"@.
  --
  --   * All detectable occurences of bottoms are replaced by @\"_|_\"@.
  --
  --   * Non-bottom functions are displayed as @\"\<function \/= _|_\>\"@.
  --
  approxShowsPrec :: Nat -> Prec -> a -> ShowS
  approxShows     :: Nat -> a -> ShowS
  approxShow      :: Nat -> a -> String

  approxShows n a = approxShowsPrec n 0 a
  approxShow n a  = approxShowsPrec n 0 a ""

instance Data a => ApproxShow a where
  approxShowsPrec n p = gShowsPrec False n p

-- This is a gigantic hack (due to special treatment of lists and
-- strings). Now I realise how I should have written it:
--   A wrapper taking care of n == 0 and bottoms.
--   A generic case treating ordinary data types
--   Special cases (type specific extensions) for tuples, functions,
--     lists and strings.
-- I'm not sure if it's possible to have a type specific extension that
-- works for, for instance, all list types, though. I guess that it
-- would have to be monomorphic.
--
-- Anyway, I don't have time improving this right now. All tests go
-- through, so this should be fine.

gShowsPrec :: Data a => Bool -> Nat -> Prec -> a -> ShowS
gShowsPrec insideList n p (a :: a)
  | n == 0       = showString "_"
  | isBottom a   = showString "_|_"
  | isFunction a = showString "<function /= _|_>"
  | isTuple a    = showParen True $ drive $
                    List.intersperse (showString ", ") $
                     (continueR (:) [] minPrec a)
  | isString a && isAtom a = when' (not insideList) (showString "\"") $
                             showString "\""  -- End of string.
  | isString a   = when' (not insideList) (showString "\"") $
                    gmapQr (.) id
                     ( id  -- Dummy.
                       `mkQ`
                       (\c -> if n == 1 then showString "_" else
                              if isBottom c then showString "_|_"
                              else showChar c)
                       `extQ`
                       (\(a :: String) -> if n == 1 then id else
                         if isBottom a then showString "_|_"
                         else gShowsPrec True (pred n) minPrec a
                       )
                     ) a
  | isList a && isAtom a = when' (not insideList) (showString "[") $
                           showString "]"  -- End of list.
  | isList a     = when' (not insideList) (showString "[") $
                    gmapQr (.) id
                     ( gShowsPrec False (pred n) minPrec
                      `extQ`
                       (\(a :: a) ->
                         if n == 1 then id
                         else if isBottom a then showString "_|_"
                         else (if not (isAtom a) then showString ", "
                                                 else id) .
                              gShowsPrec True (pred n) minPrec a
                       )
                     ) a
  | isInfix a    = showParen (not (isAtom a) && p > appPrec) $
                       -- We know that we have at least two args,
                       -- because otherwise we would have a function.
                   let (arg1:arg2:rest) =
                          continueR (:) [] (succ appPrec) a
                   in (showParen (not (null rest)) $
                       arg1 .^. showCon a .^. arg2
                      ) . drive rest
  | otherwise    = showParen (not (isAtom a) && p > appPrec) $
                   showCon a .
                   continueL (.^.) nil (succ appPrec) a

    where
    continueL f x p = gmapQl f x (gShowsPrec False (pred n) p)
    continueR f x p = gmapQr f x (gShowsPrec False (pred n) p)

drive          = foldr (.) id
nil            = showString ""
f .^. g        = f . showChar ' ' . g
appPrec        = 10
minPrec        = 0
-- Some infix constructors seem to have parentheses around them in
-- their conString representations. Maybe something should be done about
-- that. See the Q test case, and compare with ordinary lists.
showCon a      = showString $ showConstr $ toConstr a
isAtom a       = glength a == 0
isPrimitive a  = not $ isAlgType (dataTypeOf a)
isInfix a      = if isPrimitive a then
                   False
                  else
                   constrFixity (toConstr a) == Infix
wrap s         = \s' -> s . s' . s
when' b s      = if b then (s .) else (id .)
