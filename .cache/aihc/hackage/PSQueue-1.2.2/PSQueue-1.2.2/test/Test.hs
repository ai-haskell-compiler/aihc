{-# Language FlexibleContexts, StandaloneDeriving #-}

import Prelude hiding (lookup)

import Data.PSQueue.Internal

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust)

isBalanced :: LTree Int Int -> Bool
isBalanced Start = True
isBalanced (LLoser s k p l m r) =
  (size' l + size' r <= 2 || (size' l <= omega * size' r && size' r <= omega * size' l))
  && isBalanced l && isBalanced r
isBalanced (RLoser s k p l m r) =
  (size' l + size' r <= 2 || (size' l <= omega * size' r && size' r <= omega * size' l))
  && isBalanced l && isBalanced r

instance (Ord k, Ord p, Arbitrary k, Arbitrary p) => Arbitrary (PSQ k p)
  where
    arbitrary =
      do ks <- arbitrary
         ps <- arbitrary
         return . fromList $ zipWith (:->) ks ps

prop_Balanced :: PSQ Int Int -> Bool
prop_Balanced Void = True
prop_Balanced (Winner _ _ t _) = isBalanced t

prop_OrderedKeys :: PSQ Int Int -> Bool
prop_OrderedKeys t = let ks = map key . toAscList $ t in sort ks == ks

prop_AtMost :: (PSQ Int Int,Int) -> Bool
prop_AtMost (t,p) =
  let ps = map prio . atMost p $ t
  in all (<=p) ps

prop_AtMostRange :: (PSQ Int Int,Int,Int,Int) -> Bool
prop_AtMostRange (t,p,l_,r_) =
  let l = min (abs l_) (abs r_)
      r = max (abs l_) (abs r_)
      (ks,ps) = unzip . map (\b -> (key b,prio b)) . atMostRange p (l,r) $ t
  in  all (flip inrange (l,r)) ks && all (<=p) ps

prop_MinView :: PSQ Int Int -> Bool
prop_MinView t =
  case minView t of
    Nothing -> True
    Just (b1,t') ->
      case minView t' of
        Nothing -> True
        Just (b2,_) -> prio b1 <= prio b2 && prop_MinView t'

prop_SizeValid :: PSQ Int Int -> Bool
prop_SizeValid p@Void = size p == 0
prop_SizeValid p@(Winner _ _ t _) = size p == 1 + count t && go t
  where
    go Start = True
    go ll@(LLoser s _ _ l _ r) = s == count ll && go l && go r
    go rl@(RLoser s _ _ l _ r) = s == count rl && go l && go r

    count Start = 0
    count (LLoser _ _ _ l _ r) = 1 + count l + count r
    count (RLoser _ _ _ l _ r) = 1 + count l + count r

prop_LTreeBSTValid :: PSQ Int Int -> Bool
prop_LTreeBSTValid Void = True
prop_LTreeBSTValid (Winner qk _ l qm) = qk <= qm && go (<= qm) l
  where
    go _ Start = True
    go p (LLoser _ k _ l m r) =
        p k && go (\x -> p x && x <= m) l && go (\x -> p x && x > m) r
    go p (RLoser _ k _ l m r) =
        p k && go (\x -> p x && x <= m) l && go (\x -> p x && x > m) r

prop_LTreeKeysValid :: PSQ Int Int -> Bool
prop_LTreeKeysValid Void = True
prop_LTreeKeysValid p@(Winner _ _ l qm) = hasKey qm && go l
  where
    hasKey k = isJust (lookup k p)

    go Start = True
    go (LLoser _ _ _ l m r) = hasKey m && go l && go r
    go (RLoser _ _ _ l m r) = hasKey m && go l && go r

prop_LTreeSemiHeap :: PSQ Int Int -> Bool
prop_LTreeSemiHeap Void = True
prop_LTreeSemiHeap (Winner _ mp lt _) = go mp lt
  where
    go _ Start = True
    go d (LLoser _ _ p l _ r) = p >= d && go p l && go d r
    go d (RLoser _ _ p l _ r) = p >= d && go d l && go p r

prop_LTreeOriginates :: PSQ Int Int -> Bool
prop_LTreeOriginates Void = True
prop_LTreeOriginates (Winner _ _ lt _) = go lt
  where
    go Start = True
    go (LLoser _ k _ l m r) = k <= m && go l && go r
    go (RLoser _ k _ l m r) = k > m  && go l && go r

prop_PennantHeap :: PSQ Int Int -> Bool
prop_PennantHeap Void = True
prop_PennantHeap p@(Winner _ mp _ _) = go mp (tourView p)
  where
    go _ Null = True
    go d (Single _ p) = p >= d
    go d (Play l r) = fromMaybe False $ do
        (_ :-> pl) <- findMin l
        (_ :-> pr) <- findMin r
        pure $ pl >= d && pr >= d && go pl (tourView l) && go pr (tourView r)

prop_PennantBST :: PSQ Int Int -> Bool
prop_PennantBST Void = True
prop_PennantBST p = go (const True) (tourView p)
  where
    go _ Null = True
    go p (Single k _) = p k
    go p (Play l r) = fromMaybe False $ do
        (kl :-> _) <- findMin l
        (kr :-> _) <- findMin r
        pure $ kl < kr && p kl && p kr &&
            go (\x -> p x && x <= kr) (tourView l) &&
            go (\x -> p x && x >= kl) (tourView r)


assertion_BalanceFromlist :: Assertion
assertion_BalanceFromlist =
    assertBool "fromList builds a balanced tree" (prop_Balanced (fromList ls))
  where
    ls :: [Binding Int Int]
    ls = [ 63 :-> 19, 60 :-> 24, -10 :-> -27, 66 :-> 7, 60 :-> -25
         , -5 :-> -48, -3 :-> 37, -1 :-> -38, 12 :-> 67, 52 :-> -43
         , 40 :-> -29, 50 :-> -38, -30 :-> -65, 4 :-> -64, 53 :-> -5
         , -22 :-> -22, -34 :-> -51, 51 :-> 49, -43 :-> 18
         ]

assertion_BalancePlay :: Assertion
assertion_BalancePlay =
    assertBool "play gives a balanced tree" (prop_Balanced (ql `play` qr))
  where
    ql :: PSQ Int Int
    ql = Winner (-30) (-65) (LLoser 3 (-34) (-51) (LLoser 1 (-43) 18 Start (-43) Start) (-34) (RLoser 1 (-22) (-22) Start (-30) Start)) (-22)

    qr :: PSQ Int Int
    qr = Winner 4 (-64) (RLoser 13 52 (-43) (RLoser 6 40 (-29) (LLoser 4 (-5) (-48) (RLoser 2 (-3) 37 (LLoser 1 (-10) (-27) Start (-10) Start) (-5) Start) (-3) (LLoser 1 (-1) (-38) Start (-1) Start)) 4 (LLoser 1 12 67 Start 12 Start)) 40 (LLoser 6 50 (-38) (RLoser 1 51 49 Start 50 Start) 51 (RLoser 4 66 7 (RLoser 1 53 (-5) Start 52 Start) 53 (LLoser 2 60 24 Start 60 (LLoser 1 63 19 Start 63 Start))))) 66


main = defaultMain $ testGroup "Tests" [properties, regressions]
  where
    properties = testGroup "PropertyTests"
        [ testProperty "Balanced" prop_Balanced
        , testProperty "OrderedKeys" prop_OrderedKeys
        , testProperty "MinView" prop_MinView
        , testProperty "AtMost" prop_AtMost
        , testProperty "AtMostRange" prop_AtMostRange
        , testProperty "SizeValid" prop_SizeValid
        , testProperty "LTreeBSTValid" prop_LTreeBSTValid
        , testProperty "LTreeKeysValid" prop_LTreeKeysValid
        , testProperty "LTreeSemiHeap" prop_LTreeSemiHeap
        , testProperty "LTreeOriginates" prop_LTreeOriginates
        , testProperty "PennantHeap" prop_PennantHeap
        , testProperty "PennantBST" prop_PennantBST
        ]
    regressions = testGroup "RegressionTests"
        [ testCase "BalanceFromlist" assertion_BalanceFromlist
        , testCase "BalancePlay" assertion_BalancePlay
        ]
