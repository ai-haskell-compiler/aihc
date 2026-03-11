{-# LANGUAGE OverloadedLists #-}

module Expression.ReorderSpec
    ( spec
    ) where

import Data.Bifunctor (first, second)
import Data.List.NonEmpty (NonEmpty)
import Test.Hspec
import Test.Expr (parenthesize, parenthesizeVerbose)
import Test.Parser (expr)
import Expression.Reorder (Validation(..), Ambiguity(..), reorder)

{- Input:

* @(...)@ is the association for the parser (default priority is postfix, prefix, then left infix).
* @{...}@ is grouping.
* @<n.@ is an @infixl n@ operator.
* @>n.@ is an @infixr n@ operator.
* @=n.@ is an @infix n@ operator.
* @~n.@ is a @prefix n@ operator.
* @!n.@ is a @postfix n@ operator.
* @\@{x, y, ...}@ is a @postfix 99@ operator that can contain many expressions.

Output:

* @(...)@ is the actual tree structure of the expression.
* @[i:s-e]@ means the operator node has index @i@ and spans @s@ to @e@ in the source.
* Everything else is the same.
-}

-- Include source position and operator index in output.
(~=) :: String -> String -> Expectation
l ~= r = parenthesizeVerbose l <$> reorder (expr l) `shouldBe` Success r

-- Only include parentheses.
(~~) :: String -> String -> Expectation
l ~~ r = parenthesize <$> reorder (expr l) `shouldBe` Success r

-- Test that the offending expression is correct.
(~!) :: String -> NonEmpty (Ambiguity, String) -> Expectation
l ~! r = f (reorder (expr l)) `shouldBe` Failure r
    where
        f = (first . fmap . second) parenthesize

spec :: Spec
spec = do
    describe "reorder" $ do
        it "keeps closed both sides (atoms)" $ do
            "a <1. b"
            ~~ "(a <1. b)"

        it "keeps closed both sides (postfix, prefix)" $ do
            "a !1. <1. ~1. b"
            ~~ "((a !1.) <1. (~1. b))"

        it "keeps right associative" $ do
            "a >4. (b >4. c)"
            ~~ "(a >4. (b >4. c))"

        it "reorders left associativity out" $ do
            "(a <1. b) <2. c"
            ~~ "(a <1. (b <2. c))"

        it "keeps stronger left associativity" $ do
            "(a <2. b) <1. c"
            ~~ "((a <2. b) <1. c)"

        it "reorders stronger left, weaker right" $ do
            "(a <7. b) <5. (c <3. d)"
            ~~ "(((a <7. b) <5. c) <3. d)"

        it "reorders weaker left, stronger right" $ do
            "(a <3. b) <5. (c <7. d)"
            ~~ "(a <3. (b <5. (c <7. d)))"

        it "reorders weaker both sides" $ do
            "(a <4. b) <5. (c <3. d)"
            ~~ "((a <4. (b <5. c)) <3. d)"

        it "reorders weaker (equal left assoc) both sides" $ do
            "(a <3. b) <5. (c <3. d)"
            ~~ "((a <3. (b <5. c)) <3. d)"

        it "reorders weaker (equal right assoc) both sides" $ do
            "(a >3. b) <5. (c >3. d)"
            ~~ "(a >3. ((b <5. c) >3. d))"

        it "reorders weaker left, equal (assoc left) right" $ do
            "(a <3. b) <5. (c <5. d)"
            ~~ "(a <3. ((b <5. c) <5. d))"

        it "reorders weaker left, equal (assoc right) right" $ do
            "(a <3. b) >5. (c >5. d)"
            ~~ "(a <3. (b >5. (c >5. d)))"

        it "reorders equal (assoc left) left, weaker right" $ do
            "(a <5. b) <5. (c <3. d)"
            ~~ "(((a <5. b) <5. c) <3. d)"

        it "reorders equal (assoc right) left, weaker right" $ do
            "(a >5. b) >5. (c <3. d)"
            ~~ "((a >5. (b >5. c)) <3. d)"

        it "is ambiguous when equal (different assoc) left, weaker right" $ do
            "(a <5. b) >5. (c <3. d)"
            ~! [(AmbiguityMismatchAssoc, "((a <5. b) >5. c)")]

        it "is ambiguous when weaker (equal no assoc) both sides" $ do
            "(a =3. b) <5. (c =3. d)"
            ~! [(AmbiguityAssocNone, "((a =3. b) <5. (c =3. d))")]

        it "is ambiguous when weaker (different assoc) both sides" $ do
            "(a <3. b) =5. (c >3. d)"
            ~! [(AmbiguityMismatchAssoc, "((a <3. b) =5. (c >3. d))")]

        it "is ambiguous when equal (different assoc) both sides" $ do
            "(a <5. b) =5. (c >5. d)"
            ~! [(AmbiguityMismatchAssoc, "((a <5. b) =5. (c >5. d))")]

        it "is ambiguous when stronger left, equal (different assoc) right" $ do
            "(a <7. b) <5. (c >5. d)"
            ~! [(AmbiguityMismatchAssoc, "(b <5. (c >5. d))")]

        it "is ambiguous when equal (different assoc) left, stronger right" $ do
            "(a >5. b) <5. (c =7. d)"
            ~! [(AmbiguityMismatchAssoc, "((a >5. b) <5. c)")]

        it "reorders higher fixity in right-hand side" $ do
            "a <3. b <4. c"
            ~= "(a <3.[0:0-13] (b <4.[1:6-13] c))"

        it "keeps correct left associativity" $ do
            "a <3. b <3. c"
            ~= "((a <3.[0:0-7] b) <3.[1:0-13] c)"

        it "reorders a postfix into a binary" $ do
            "(a <1. b) !2."
            ~~ "(a <1. (b !2.))"

        it "reorders a prefix into a binary" $ do
            "~2. (a <1. b)"
            ~~ "((~2. a) <1. b)"

        it "reorders postfix over prefix" $ do
            "~2. (a !1.)"
            ~= "((~2.[0:0-6] a) !1.[1:0-10])"

        it "reorders prefix over postfix" $ do
            "(~1. a) !2."
            ~= "(~1.[0:1-11] (a !2.[1:5-11]))"

        it "reorders alternating prefix and postfix" $ do
            "~2. ~4. a !5. !3."
            ~= "(~2.[0:0-17] ((~4.[1:4-13] (a !5.[2:8-13])) !3.[3:4-17]))"

        it "reorders right associative operator" $ do
            "a >4. b >4. c"
            ~= "(a >4.[0:0-13] (b >4.[1:6-13] c))"

        it "reorders a prefix operator outside" $ do
            "~1. a <2. b"
            ~= "(~1.[0:0-11] (a <2.[1:4-11] b))"

        it "keeps a prefix operator inside" $ do
            "~2. a <1. b"
            ~= "((~2.[0:0-5] a) <1.[1:0-11] b)"

        it "reorders a postfix operator outside" $ do
            "a <2. b !1."
            ~= "((a <2.[0:0-7] b) !1.[1:0-11])"

        it "reorders higher fixity on both sides" $ do
            "a <2. b <1. c <2. d"
            ~= "((a <2.[0:0-7] b) <1.[1:0-19] (c <2.[2:12-19] d))"

        it "reorders inside leaves" $ do
            "{a <3. b <4. c}"
            ~="{(a <3.[0:1-14] (b <4.[1:7-14] c))}"

        it "reorders inside many leaves" $ do
            "{a <3. b <4. c} @{a <3. b <4. c, a <3. b <4. c}"
            ~= "({(a <3.[0:1-14] (b <4.[1:7-14] c))} @[2:0-47]{(a <3.[3:18-31] (b <4.[4:24-31] c)), (a <3.[5:33-46] (b <4.[6:39-46] c))})"

        it "reorders something complicated" $ do
            "~2. ~4. {a >3. {b >3. c} <4. d <4. e =1. f} !5. !3."
            ~= "(~2.[0:0-51] ((~4.[1:4-47] ({((a >3.[2:9-36] (({(b >3.[3:16-23] c)} <4.[4:15-30] d) <4.[5:15-36] e)) =1.[6:9-42] f)} !5.[7:8-47])) !3.[8:4-51]))"

        it "is ambiguous when chaining non-associative" $ do
            "a =1. b =1. c"
            ~! [(AmbiguityAssocNone, "((a =1. b) =1. c)")]

        it "is ambiguous when same precedence prefix and postfix" $ do
            "~1. a !1."
            ~! [(AmbiguityMismatchAssoc, "((~1. a) !1.)")]

        it "is ambiguous when same precedence left and right associative" $ do
            "a <3. b >3. c"
            ~! [(AmbiguityMismatchAssoc, "((a <3. b) >3. c)")]

        it "finds multiple ambiguities in closed children" $ do
            "a @{a =1. b =1. c, a =2. b =2. c}"
            ~! [ (AmbiguityAssocNone, "((a =1. b) =1. c)")
               , (AmbiguityAssocNone, "((a =2. b) =2. c)")
               ]

        it "finds multiple ambiguities in open children" $ do
            "{a =1. b =1. c} <1. {a =2. b =2. c}"
            ~! [ (AmbiguityAssocNone, "((a =1. b) =1. c)")
               , (AmbiguityAssocNone, "((a =2. b) =2. c)")
               ]

        it "finds multiple ambiguities in open and closed children" $ do
            "{a =1. b =1. c} @{a =2. b =2. c}"
            ~! [ (AmbiguityAssocNone, "((a =1. b) =1. c)")
               , (AmbiguityAssocNone, "((a =2. b) =2. c)")
               ]

        it "reorders something really complicated" $ do
            "~2. a <4. (b <4. c) >5. (d >5. (e =3. f) !7.) =1. g"
            ~~ "((~2. (((a <4. b) <4. (c >5. (d >5. e))) =3. (f !7.))) =1. g)"

        it "reorders binary containing binary and postfix" $ do
            "(a <2. b) <2. (c !1.)" ~~ "(((a <2. b) <2. c) !1.)"

        it "reorders binary containing prefix and binary" $ do
            "(~1. a) <2. (b <2. c)" ~~ "(~1. ((a <2. b) <2. c))"

        it "reorders postfix and prefix in a binary" $ do
            "(~1. a) <3. (c !2.)" ~~ "(~1. ((a <3. c) !2.))"

        it "is ambiguous when all same precedence, all different associativity" $ do
            "(a <3. b) =3. (c >3. d)"
            ~! [(AmbiguityMismatchAssoc, "((a <3. b) =3. (c >3. d))")]

        it "is ambiguous when all same precedence, left different associativity" $ do
            "(a <3. b) >3. (c >3. d)"
            ~! [(AmbiguityMismatchAssoc, "((a <3. b) >3. c)")]

        it "is ambiguous when all same precedence, right different associativity" $ do
            "(a <3. b) <3. (c >3. d)"
            ~! [(AmbiguityMismatchAssoc, "(b <3. (c >3. d))")]

        it "is ambiguous when all same precedence, no associativity" $ do
            "(a =3. b) =3. (c =3. d)"
            ~! [(AmbiguityAssocNone, "((a =3. b) =3. (c =3. d))")]

        it "reorders all same precedence, left associative" $ do
            "(a <3. b) <3. (c <3. d)"
            ~~ "(((a <3. b) <3. c) <3. d)"

        it "reorders all same precedence, right associative" $ do
            "(a >3. b) >3. (c >3. d)"
            ~~ "(a >3. (b >3. (c >3. d)))"
