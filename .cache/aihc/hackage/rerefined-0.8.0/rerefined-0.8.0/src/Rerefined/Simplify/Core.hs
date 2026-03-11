{-# LANGUAGE UndecidableInstances #-}

{- | Core predicate simplification algorithm.

This is related to an NP-complete problem (see Boolean satisfiability problem).
We focus on /immediate, operation-reducing simplifications/, and hope that the
input is formed in such a way that our rules match.

In short, the simplifier is largely contextless. It inspects (usually) a single
layer/depth at a time. So we can consistently simplify things like logical
identities. But don't expect simplifications hard to spot with the naked eye.

Implementation pitfalls:

* not extensible: only works for built-in logical & relational predicates
* no protection against non-termination e.g. if a pair of transformations loop
* very tedious to write. that's life

__Internal module. Exports may change without warning. Try not to use.__
-}

module Rerefined.Simplify.Core where

import Rerefined.Predicate.Succeed
import Rerefined.Predicate.Fail

import Rerefined.Predicate.Logical

import Rerefined.Predicate.Relational ( CompareLength, CompareValue, FlipRelOp )
import Rerefined.Simplify.Relational
  ( SimplifyCompareLength
  , SimplifyCompareLengthAnd
  , SimplifyCompareLengthOr
  )

-- note that we can't modularize logical simplifications because they're
-- mutually recursive with the main simplifier :(

-- | Try to perform a single simplification step on the given predicate.
--
-- Returns 'Nothing' if we were unable to simplify.
type family SimplifyStep p where
    SimplifyStep (Not  p)   = SimplifyNot  p

    SimplifyStep (And  l r) = SimplifyAnd  l r
    SimplifyStep (Or   l r) = SimplifyOr   l r
    SimplifyStep (Nand l r) = SimplifyNand l r
    SimplifyStep (Nor  l r) = SimplifyNor  l r
    SimplifyStep (Iff  l r) = SimplifyIff  l r
    SimplifyStep (Xor  l r) = SimplifyXor  l r
    SimplifyStep (If   l r) = SimplifyIf   l r

    SimplifyStep (CompareLength op n) = SimplifyCompareLength op n
    -- Don't think we can do anything for CompareValue.

    SimplifyStep p = Nothing

type family SimplifyAnd l r where
    -- identity laws
    SimplifyAnd p       Fail    = Just Fail
    SimplifyAnd Fail    p       = Just Fail
    SimplifyAnd p       Succeed = Just p
    SimplifyAnd Succeed p       = Just p

    SimplifyAnd p p = Just p

    -- distributivity
    SimplifyAnd (Or x y) (Or x z) = Just (Or x (And y z))

    -- special
    SimplifyAnd (CompareLength lop ln) (CompareLength rop rn) =
        SimplifyCompareLengthAnd lop ln rop rn

    -- recurse
    SimplifyAnd l r =
        (OrElseAndL r (SimplifyStep l)
            (OrElseAndR l (SimplifyStep r)
                Nothing))

type family OrElseAndL r mp cont where
    OrElseAndL r Nothing   cont = cont
    OrElseAndL r (Just l') cont = Just (And l' r)

type family OrElseAndR l mp cont where
    OrElseAndR l Nothing   cont = cont
    OrElseAndR l (Just r') cont = Just (And l r')

type family SimplifyOr l r where
    -- identity laws
    SimplifyOr Succeed p       = Just Succeed
    SimplifyOr p       Succeed = Just Succeed
    SimplifyOr Fail    p       = Just p
    SimplifyOr p       Fail    = Just p

    SimplifyOr p p = Just p

    -- distributivity
    SimplifyOr (And x y) (And x z) = Just (And x (Or y z))

    -- special relational
    SimplifyOr (CompareLength lop ln) (CompareLength rop rn) =
        SimplifyCompareLengthOr lop ln rop rn

    -- recurse
    SimplifyOr l r =
        (OrElseOrL r (SimplifyStep l)
            (OrElseOrR l (SimplifyStep r)
                Nothing))

type family OrElseOrL r mp cont where
    OrElseOrL r Nothing   cont = cont
    OrElseOrL r (Just l') cont = Just (Or l' r)

type family OrElseOrR l mp cont where
    OrElseOrR l Nothing   cont = cont
    OrElseOrR l (Just r') cont = Just (Or l r')

type family SimplifyNand l r where
    -- identity laws
    SimplifyNand Fail    p       = Just Succeed
    SimplifyNand p       Fail    = Just Succeed
    SimplifyNand Succeed p       = Just (Not p)
    SimplifyNand p       Succeed = Just (Not p)

    SimplifyNand p p = Just (Not p)

    -- recurse
    SimplifyNand l r =
        (OrElseNandL r (SimplifyStep l)
            (OrElseNandR l (SimplifyStep r)
                Nothing))

type family OrElseNandL r mp cont where
    OrElseNandL r Nothing   cont = cont
    OrElseNandL r (Just l') cont = Just (Nand l' r)

type family OrElseNandR l mp cont where
    OrElseNandR l Nothing   cont = cont
    OrElseNandR l (Just r') cont = Just (Nand l r')

type family SimplifyNor l r where
    -- identity laws
    SimplifyNor Succeed p       = Just Fail
    SimplifyNor p       Succeed = Just Fail
    SimplifyNor Fail    p       = Just (Not p)
    SimplifyNor p       Fail    = Just (Not p)

    SimplifyNor p p = Just (Not p)

    -- recurse
    SimplifyNor l r =
        (OrElseNorL r (SimplifyStep l)
            (OrElseNorR l (SimplifyStep r)
                Nothing))

type family OrElseNorL r mp cont where
    OrElseNorL r Nothing   cont = cont
    OrElseNorL r (Just l') cont = Just (Nor l' r)

type family OrElseNorR l mp cont where
    OrElseNorR l Nothing   cont = cont
    OrElseNorR l (Just r') cont = Just (Nor l r')

type family SimplifyXor l r where
    -- identity laws
    SimplifyXor Fail    p       = Just p
    SimplifyXor p       Fail    = Just p
    SimplifyXor Succeed p       = Just (Not p)
    SimplifyXor p       Succeed = Just (Not p)

    SimplifyXor p p = Just Fail

    -- recurse
    SimplifyXor l r =
        (OrElseXorL r (SimplifyStep l)
            (OrElseXorR l (SimplifyStep r)
                Nothing))

type family OrElseXorL r mp cont where
    OrElseXorL r Nothing   cont = cont
    OrElseXorL r (Just l') cont = Just (Xor l' r)

type family OrElseXorR l mp cont where
    OrElseXorR l Nothing   cont = cont
    OrElseXorR l (Just r') cont = Just (Xor l r')

type family SimplifyIf l r where
    -- identity laws
    SimplifyIf Fail    p       = Just Succeed
    SimplifyIf p       Fail    = Just Succeed
    SimplifyIf Succeed p       = Just p
    SimplifyIf p       Succeed = Just p

    SimplifyIf p p = Just Succeed

    -- recurse
    SimplifyIf l r =
        (OrElseIfL r (SimplifyStep l)
            (OrElseIfR l (SimplifyStep r)
                Nothing))

type family OrElseIfL r mp cont where
    OrElseIfL r Nothing   cont = cont
    OrElseIfL r (Just l') cont = Just (If l' r)

type family OrElseIfR l mp cont where
    OrElseIfR l Nothing   cont = cont
    OrElseIfR l (Just r') cont = Just (If l r')

type family SimplifyIff l r where
    -- identity laws
    SimplifyIff Succeed p       = Just p
    SimplifyIff p       Succeed = Just p
    SimplifyIff Fail    p       = Just (Not p)
    SimplifyIff p       Fail    = Just (Not p)

    SimplifyIff p p = Just Succeed

    -- recurse
    SimplifyIff l r =
        (OrElseIffL r (SimplifyStep l)
            (OrElseIffR l (SimplifyStep r)
                Nothing))

type family OrElseIffL r mp cont where
    OrElseIffL r Nothing   cont = cont
    OrElseIffL r (Just l') cont = Just (Iff l' r)

type family OrElseIffR l mp cont where
    OrElseIffR l Nothing   cont = cont
    OrElseIffR l (Just r') cont = Just (Iff l r')

type family SimplifyNot p where
    -- double negation
    SimplifyNot (Not p) = Just p

    SimplifyNot Succeed = Just Fail
    SimplifyNot Fail    = Just Succeed

    -- special relational
    SimplifyNot (CompareLength op      n) =
        Just (CompareLength (FlipRelOp op)      n)
    SimplifyNot (CompareValue  op sign n) =
        Just (CompareValue  (FlipRelOp op) sign n)

    -- recurse
    SimplifyNot p = OrElseNot (SimplifyStep p) Nothing

type family OrElseNot mp cont where
    OrElseNot (Just p') cont = Just (Not p')
    OrElseNot Nothing   cont = cont
