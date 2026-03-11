{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Test.TypeSpec hiding ( And )
import Rerefined.Simplify
import Rerefined.Predicates
import Rerefined.Predicate
import TypeLevelShow.Utils ( type (++) )

-- | Pretty opaque predicate for simplification proofs.
data Opaque
instance Predicate Opaque where type PredicateName d Opaque = "p"

-- | Shorthand for the pretty opaque predicate.
type P = Opaque

main :: IO ()
main = print spec

spec :: Expect
    -- AND identity laws
    (And Fail    P       `SimplifiesTo` Fail
 -/- And P       Fail    `SimplifiesTo` Fail
 -/- And Succeed P       `SimplifiesTo` P
 -/- And P       Succeed `SimplifiesTo` P

     -- OR identity laws
 -/- Or  Succeed P       `SimplifiesTo` Succeed
 -/- Or  P       Succeed `SimplifiesTo` Succeed
 -/- Or  Fail    P       `SimplifiesTo` P
 -/- Or  P       Fail    `SimplifiesTo` P

     -- other
 -/- And (CompareLength RelOpLT 3) (CompareLength RelOpGT 3)
        `SimplifiesTo` Fail
    )
spec = Valid

-- | TODO I'd like to write my own failure message here, but type-spec is
--   complicated.
type SimplifiesTo p p' = It
    (PredicateName 0 p ++ " simplifies to " ++ PredicateName 0 p')
    (TrySimplify p `Is` Just p')
